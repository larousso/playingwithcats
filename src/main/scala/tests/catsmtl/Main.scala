package tests.catsmtl

import cats.effect.IO
import fs2.Stream
import tests.catsmtl.MainMtl._


object Main extends App {

  import cats.data._
  import cats.implicits._


  case class Reservation(id: String)
  case class Reservations(reservations: List[Reservation] = List.empty)
  sealed trait Event

  case class ReservationCreated(id: String)
  sealed trait Command
  case class Create(id: String, guests: Int) extends Command
  case class CreatedEvent(id: String) extends Event

  type AppError = String
  type Result[T] = Either[AppError, T]
  type EventAnd[T] = WriterT[Result, List[Event], T]
  type CommandProcessor[T] = StateT[EventAnd, Reservations, T]

  def process(commands: Stream[IO, Command], initial: Reservations): Stream[IO, (Reservations, List[Event])] = {
    commands.mapAccumulate(initial) { (state, command) =>
      processCommand(command).run(state).run match {
        case Left(error) =>
          (state, Nil)
        case Right((events, (nextState, _))) =>
          (nextState, events)
      }
    }
  }

  def persist(stream: Stream[IO, (Reservations, List[Event])]) =
    stream.evalMap {
      case ( state, events) =>
        (persistState(state), persistEvents(events)).tupled.void
    }

  def persistEvents(events: List[Event]): IO[Unit] = ???
  def persistState(reservations: Reservations): IO[Unit] = ???


  println(processCommand(Create("id", 2)).run(Reservations()).run)

  def processCommand(command: Command): CommandProcessor[Unit] = {
    command match {
      case c: Create =>
        (validateDup(c),
          validateGuest(c)
        ).tupled.void
    }
  }

  def validateDup(command: Create): CommandProcessor[Unit] = {
    val res: Result[Unit] = ().asRight[AppError]
    StateT.liftF[EventAnd, Reservations, Unit](WriterT.liftF(res))
  }


  def validateGuest(command: Create): CommandProcessor[Unit] = {
    if (command.guests <= 0) {
      val res: Result[Unit] = "Non positive guest".asLeft[Unit]
      StateT.liftF[EventAnd, Reservations, Unit](WriterT.liftF(res))
    } else {
      for {
        state <- StateT.get[EventAnd, Reservations]
        events = generateEvents(command)
        _ <- StateT.liftF[EventAnd, Reservations, Unit](WriterT.tell[Result, List[Event]](events))
        newState = generateNewState(state, events)
        _ <- StateT.set[EventAnd, Reservations](newState)
      } yield ()
    }
  }

  def generateEvents(command: Create): List[Event] = List(CreatedEvent(command.id))
  def generateNewState(reservations: Reservations, events: List[Event]): Reservations = {
    events.foldLeft(reservations)(appyEvent)
  }

  def appyEvent(reservations: Reservations, event: Event): Reservations = {
    event match {
      case CreatedEvent(id) => reservations.copy(Reservation(id) :: reservations.reservations)
    }
  }

}
