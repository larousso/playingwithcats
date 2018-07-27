package tests.catsmtl


object MainMtl2 extends App {

  import cats.{MonadError, ApplicativeError, Functor, Monad}
  import cats.data._
  import cats.effect._
  import cats.mtl._

  case class Reservation(id: String)
  case class Reservations(reservations: List[Reservation] = List.empty)
  sealed trait Event
  case class CreatedEvent(id: String) extends Event

  case class ReservationCreated(id: String)
  sealed trait Command
  case class Create(id: String, guests: Int) extends Command

  type AppError = String

  type Effect0[T] = EitherT[IO, AppError, T]
  type Effect1[T] = WriterT[Effect0, List[Event], T]
  type Effect[T] = StateT[Effect1, Reservations, T]

  {
    import cats.implicits._
    import cats.mtl.implicits._
    val value: Effect[Unit] = processCommand[Effect](Create("1", 2))
    val sync: Either[AppError, (List[Event], (Reservations, Unit))] = value.run(Reservations()).run.value.unsafeRunSync
    println(sync)
  }

  def processCommand[F[_]: MonadError[?[_], AppError]: MonadState[?[_], Reservations]: FunctorTell[?[_], List[Event]]](command: Command): F[Unit] = {
    command match {
      case c: Create =>
        validateGuest(c)
    }
  }

  def validateDup[F[_]: ApplicativeError[?[_], AppError]](command: Create): F[Unit] = valid[F, Unit](())

  def generateEvents(command: Create): List[Event] = List(CreatedEvent(command.id))
  def generateNewState(reservations: Reservations, events: List[Event]): Reservations = {
    events.foldLeft(reservations)(appyEvent)
  }
  def appyEvent(reservations: Reservations, event: Event): Reservations = {
    event match {
      case CreatedEvent(id) => reservations.copy(Reservation(id) :: reservations.reservations)
    }
  }

  def getReservation[F[_]: MonadState[?[_], Reservations]]: F[Reservations] = MonadState[F, Reservations].get
  def setReservation[F[_]: Functor: MonadState[?[_], Reservations]](r: Reservations): F[Reservations] = {
    import cats.implicits._
    MonadState[F, Reservations].set(r).map(_ => r)
  }
  def setEvents[F[_]: FunctorTell[?[_], List[Event]]](r: List[Event]): F[Unit] = FunctorTell[F, List[Event]].tell(r)
  def invalid[F[_]: ApplicativeError[?[_], AppError]](err: AppError): F[Unit] = ApplicativeError[F, AppError].raiseError(err)
  def valid[F[_]: ApplicativeError[?[_], AppError], T](v: T): F[Unit] = ApplicativeError[F, AppError].pure(v)

  def validateGuest[F[_]: Monad: MonadError[?[_], AppError]: MonadState[?[_], Reservations] : FunctorTell[?[_], List[Event]]](command: Create): F[Unit] = {
    import cats.implicits._
    if (command.guests <= 0) {
      invalid[F]("Non positive guest")
    } else {
      for {
        state   <- getReservation[F]
        events  = generateEvents(command)
        _       <- setEvents[F](events)
        newState = generateNewState(state, events)
        _       <- setReservation[F](newState)
      } yield ()
    }
  }



  //
//  import cats._
//  import cats.data._
//  import cats.implicits._
//  import cats.effect._
//  import cats.mtl._
//  import cats.mtl.implicits._
//  import fs2.Stream
//
//  case class Reservation(id: String)
//  case class Reservations(reservations: List[Reservation] = List.empty)
//  sealed trait Event
//  case class CreatedEvent(id: String) extends Event
//
//  case class ReservationCreated(id: String)
//  sealed trait Command
//  case class Create(id: String, guests: Int) extends Command
//
//  type AppError = String
//  type ReservationsState[F[_]] = MonadState[F, Reservations]
//  type EventLog[F[_]] = FunctorListen[F, List[Event]]
//  type AppErrors[F[_]] = MonadError[F, AppError]
//
//  val _ = {
//    type Effect0[T] = EitherT[IO, MonadError[IO, AppError], T]
//    type Effect1[T] = WriterT[Effect0, List[Event], T]
//    type Effect[T] = StateT[Effect1, Reservations, T]
//
//    val value: Effect[Unit] = processCommand[Effect](Create("1", 2))
//    val sync: Either[AppError, (List[Event], (Reservations, Unit))] = value.run(Reservations()).run.value.unsafeRunSync
//    println(sync)
//  }
//
//
//
//  def processCommand[F[_]: AppErrors: ReservationsState: EventLog](command: Command): F[Unit] = {
//    command match {
//      case c: Create =>
//        validateDup(c)
//    }
//  }
//
//  def validateDup[F[_]: AppErrors](command: Create): F[Unit] = valid[F, Unit](())
//
//  def generateEvents(command: Create): List[Event] = List(CreatedEvent(command.id))
//  def generateNewState(reservations: Reservations, events: List[Event]): Reservations = reservations
//
//  def getReservation[F[_]: ReservationsState]: F[Reservations] = MonadState[F, Reservations].get
//  def setReservation[F[_]: ReservationsState: Functor](r: Reservations): F[Reservations] = MonadState[F, Reservations].set(r).map(_ => r)
//  def setEvents[F[_]: EventLog](r: List[Event]): F[Unit] = FunctorListen[F, List[Event]].tell(r)
//  def invalid[F[_]: AppErrors](err: AppError): F[Unit] = ApplicativeError[F, AppError].raiseError(err)
//  def valid[F[_]: AppErrors, T](v: T): F[Unit] = ApplicativeError[F, AppError].pure(v)
//
//  def validateGuest[F[_]: AppErrors: ReservationsState : EventLog](command: Create): F[Unit] = {
//    if (command.guests <= 0) {
//      invalid[F]("Non positive guest")
//    } else {
//      for {
//        state   <- getReservation[F]
//        events  = generateEvents(command)
//        _       <- setEvents[F](events)
//        newState = generateNewState(state, events)
//        _       <- setReservation[F](newState)
//      } yield ()
//    }
//  }

}
