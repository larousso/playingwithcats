package tests.catsmtl

import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.mtl.{MonadState, _}
import cats.{ApplicativeError, Functor, Monad, MonadError}
import tests.catsmtl.MainMtl.BIO

object MonadStateImpl {
  def create[E, S](s: S): MonadStateImpl[E, S] = {
    new MonadStateImpl[E, S](Ref.of[IO, S](s))
  }
}

class MonadStateImpl[E, S](io: IO[Ref[IO, S]]) extends MonadState[BIO[E, ?], S] {

  private val value: Ref[IO, S] = io.unsafeRunSync()
  override val monad: Monad[BIO[E, ?]] = Monad[BIO[E, ?]]

  override def get: BIO[E, S] = EitherT.liftF[IO, E, S](value.get)
  override def set(s: S): BIO[E, Unit] = EitherT.liftF[IO, E, Unit](value.set(s))
  override def inspect[A](f: S => A): BIO[E, A] = EitherT.liftF[IO, E, A](value.get.map(f))
  override def modify(f: S => S): BIO[E, Unit] = EitherT.liftF[IO, E, Unit](value.update(f))

}

object FunctorListenImpl {
  def create[E, S, L](s: S, l: L): FunctorListenImpl[E, S, L] = new FunctorListenImpl[E, S, L](Ref.of[IO, (S, L)]((s, l)))
}
class FunctorListenImpl[E, S, L](io: IO[Ref[IO, (S, L)]]) extends FunctorListen[BIO[E, ?], L] {
  private val value: Ref[IO, (S, L)] = io.unsafeRunSync()

  override val tell: FunctorTell[BIO[E, ?], L] = new FunctorTell[BIO[E, ?], L] {
    override val functor: Functor[BIO[E, ?]] = Functor[BIO[E, ?]]
    override def tell(l: L): BIO[E, Unit] = EitherT.liftF[IO, E, Unit](value.update {
      case (s, _) => (s, l)
    })
    override def writer[A](a: A, l: L): BIO[E, A] = EitherT.liftF[IO, E, A](value.modify(t => ((t._1, l), a)).map(_ => a))
    override def tuple[A](ta: (L, A)): BIO[E, A] = writer(ta._2, ta._1)
  }

  override def listen[A](fa: BIO[E, A]): BIO[E, (A, L)] = {
    fa.flatMap { a =>
      EitherT.liftF[IO, E, (S, L)](value.get).map {
        case (_, l) => (a, l)
      }
    }
  }

  override def listens[A, B](fa: BIO[E, A])(f: L => B): BIO[E, (A, B)] = EitherT.liftF[IO, E, (S, L)](value.get).flatMap { case (_, l) => fa.map(a => (a, f(l)))}
}

object MainMtl extends App {

  case class Reservation(id: String)
  case class Reservations(reservations: List[Reservation] = List.empty)
  sealed trait Event
  case class CreatedEvent(id: String) extends Event

  case class ReservationCreated(id: String)
  sealed trait Command
  case class Create(id: String, guests: Int) extends Command

  type AppError = String
  type ReservationsState[F[_]] = MonadState[F, Reservations]
  type EventLog[F[_]] = FunctorListen[F, List[Event]]
  type AppErrors[F[_]] = MonadError[F, AppError]

  type BIO[E, T] = EitherT[IO, E, T]
  type EBIO[T] = BIO[AppError, T]

  def run(command: Command, state: Reservations): IO[Either[AppError, (List[Event], Reservations)]] = {
    import cats.mtl.implicits._
    implicit val stateMInstance: MonadStateImpl[AppError, Reservations] = MonadStateImpl.create[AppError, Reservations](Reservations())
    implicit val writerMInstance: FunctorListenImpl[AppError, Unit, List[Event]] = FunctorListenImpl.create[AppError, Unit, List[Event]]((), List.empty[Event])
    (
      for {
        l <- processCommand[EBIO](command).listen
        s <- MonadState[EBIO, Reservations].get
      } yield (l._2, s)
    ).value
  }

  private val value = run(Create("1", 2), Reservations())

  private val sync: Either[AppError, (List[Event], Reservations)] = value.unsafeRunSync
  println(sync)

  def processCommand[F[_]: AppErrors: ReservationsState: EventLog](command: Command): F[Unit] = {
    import cats.syntax.all._
    command match {
      case c: Create =>
        (validateDup(c),
          validateGuest(c)
        ).tupled.void
    }
  }

  def validateDup[F[_]: AppErrors](command: Create): F[Unit] = valid[F, Unit](())

  def generateEvents(command: Create): List[Event] = List(CreatedEvent(command.id))
  def generateNewState(reservations: Reservations, events: List[Event]): Reservations = {
    events.foldLeft(reservations)(appyEvent)
  }

  def appyEvent(reservations: Reservations, event: Event): Reservations = {
    event match {
      case CreatedEvent(id) => reservations.copy(Reservation(id) :: reservations.reservations)
    }
  }

  def getReservation[F[_]: ReservationsState]: F[Reservations] = MonadState[F, Reservations].get
  def setReservation[F[_]: ReservationsState: Functor](r: Reservations): F[Reservations] = {
    import cats.implicits._
    MonadState[F, Reservations].set(r).map(_ => r)
  }
  def setEvents[F[_]: EventLog](r: List[Event]): F[Unit] = {
    import cats.mtl.implicits._
    FunctorTell[F, List[Event]].tell(r)
  }
  def invalid[F[_]: AppErrors](err: AppError): F[Unit] = ApplicativeError[F, AppError].raiseError(err)
  def valid[F[_]: AppErrors, T](v: T): F[Unit] = ApplicativeError[F, AppError].pure(v)

  def validateGuest[F[_]: Monad: ReservationsState : EventLog : AppErrors](command: Create): F[Unit] = {
    import cats.implicits._
    import cats.mtl.implicits._
    if (command.guests <= 0) {
      invalid[F]("Non positive guest")
    } else {
      for {
        state   <- getReservation[F]
        events  = generateEvents(command)
        e       <- setEvents[F](events).listen
        newState = generateNewState(state, events)
        r       <- setReservation[F](newState)
      } yield ()
    }
  }
}
