package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import cats.syntax.traverse.toTraverseOps
import scala.annotation.tailrec

object Day20 extends IOApp.Simple {

  enum Pulse:
    case High, Low

  enum FlipFlop:
    case On, Off
    def transition: Pulse => (FlipFlop, Option[Pulse]) =
      case Pulse.High => this -> None
      case Pulse.Low  =>
        this match
          case On  => Off -> Some(Pulse.Low)
          case Off => On  -> Some(Pulse.High)

  final case class Conjunction(memory: Map[String, Pulse]):
    def transition(input: String, pulse: Pulse): (Conjunction, Pulse) =
      val updatedMemory: Map[String, Pulse] = memory.updated(input, pulse)
      copy(memory = updatedMemory) -> (if updatedMemory.values.forall(_ == Pulse.High) then Pulse.Low else Pulse.High)

  sealed trait Module:
    val name: String
    val next: Vector[String]
  final case class FlipFlopModule(val name: String, val next: Vector[String], state: FlipFlop)       extends Module:
    def receive: Pulse => (FlipFlop, Option[Pulse]) = state.transition
  final case class ConjunctionModule(val name: String, val next: Vector[String], state: Conjunction) extends Module:
    def receive: (String, Pulse) => (Conjunction, Pulse) = state.transition
  final case class BroadcasterModule(val next: Vector[String])                                       extends Module:
    val name: String = "broadcaster"

  def constructConjunctionInputs(transitionsMap: Map[String, Module]): Map[String, Module] =
    transitionsMap
      .collect {
        case (k: String, _: ConjunctionModule) =>
          k -> transitionsMap.values.collect { case m: Module if m.next.contains(k) => m.name }
      }
      .foldLeft(transitionsMap) {
        case (acc: Map[String, Module], (name: String, newMemory: Iterable[String])) =>
          acc(name) match
            case cm: ConjunctionModule =>
              acc.updated(name, cm.copy(state = Conjunction(cm.state.memory ++ newMemory.toList.map(_ -> Pulse.Low))))
            case _                     => acc
      }

  def buttonPush(transitionsMap: Map[String, Module], buttonPushes: Int, cycles: Map[String, Long])
      : (Map[String, Module], Int, Int, Map[String, Long]) =
    @tailrec
    def buttonPush_(
        transitionsMap: Map[String, Module],
        inputs: Vector[(String, Pulse, String)],
        lowPulsesCount: Int,
        highPulsesCount: Int,
        cycles: Map[String, Long],
      ): (Map[String, Module], Int, Int, Map[String, Long]) =
      if inputs.isEmpty then (transitionsMap, lowPulsesCount + 1, highPulsesCount, cycles)
      else
        val (
          newTransitionsMap: Map[String, Module],
          newInputs: Vector[(String, Pulse, String)],
          newCycles: Map[String, Long],
        ) =
          inputs.foldLeft((transitionsMap, Vector.empty[(String, Pulse, String)], cycles)) {
            case (
                   (
                     accMap: Map[String, Module],
                     accInputs: Vector[(String, Pulse, String)],
                     accCycles: Map[String, Long],
                   ),
                   (parentName: String, pulse: Pulse, name: String),
                 ) =>
              transitionsMap.get(name) match
                case Some(flipflop: FlipFlopModule) =>
                  flipflop.receive(pulse) match
                    case (_, None)                                   => (accMap, accInputs, accCycles)
                    case (newState: FlipFlop, Some(newPulse: Pulse)) =>
                      (
                        accMap.updated(name, flipflop.copy(state = newState)),
                        (accInputs :++ flipflop.next.map((name, newPulse, _))),
                        accCycles,
                      )

                case Some(conjunction: ConjunctionModule) =>
                  val (newState: Conjunction, newPulse: Pulse) = conjunction.receive(parentName, pulse)
                  val newCycles: Map[String, Long]             =
                    if newPulse == Pulse.High && !accCycles.contains(conjunction.name) then
                      accCycles.updated(conjunction.name, buttonPushes.toLong)
                    else accCycles
                  (
                    accMap.updated(name, conjunction.copy(state = newState)),
                    (accInputs :++ conjunction.next.map((name, newPulse, _))),
                    newCycles,
                  )

                case None | Some(_: BroadcasterModule) => (accMap, accInputs, accCycles)
          }

        buttonPush_(
          newTransitionsMap,
          newInputs,
          lowPulsesCount + newInputs.count((_, pulse: Pulse, _) => pulse == Pulse.Low),
          highPulsesCount + newInputs.count((_, pulse: Pulse, _) => pulse == Pulse.High),
          newCycles,
        )

    val initialInputs: Vector[String] = transitionsMap("broadcaster").next
    buttonPush_(transitionsMap, initialInputs.map(("broadcaster", Pulse.Low, _)), initialInputs.length, 0, cycles)

  def parseModules(lines: List[String]): List[Module] = lines.collect {
    case s"broadcaster -> $next" => BroadcasterModule(next.split(", ").toVector)
    case s"%$name -> $next"      => FlipFlopModule(name, next.split(", ").toVector, FlipFlop.Off)
    case s"&$name -> $next"      => ConjunctionModule(name, next.split(", ").toVector, Conjunction(Map.empty))
  }

  val task1: IO[Unit] = for {
    lines: List[String]                           <- Utils.readLines[IO]("day20.input.txt").compile.toList
    modules: List[Module]                          = parseModules(lines)
    transitionsMap: Map[String, Module]            = constructConjunctionInputs(Map.from(modules.map((m: Module) => m.name -> m)))
    (_, lowPulsesCount: Int, highPulsesCount: Int) =
      (1 to 1000).foldLeft((transitionsMap, 0, 0)) {
        case ((acc: Map[String, Module], lowPulsesCountAcc: Int, highPulsesCountAcc: Int), i) =>
          val (newTransitionsMap: Map[String, Module], lowPulsesCount: Int, highPulsesCount: Int, _) =
            buttonPush(acc, i, Map.empty)
          (newTransitionsMap, lowPulsesCount + lowPulsesCountAcc, highPulsesCount + highPulsesCountAcc)
      }
    _                                             <- IO.println(lowPulsesCount * highPulsesCount)
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[String]                <- Utils.readLines[IO]("day20.input.txt").compile.toList
    modules: List[Module]               = parseModules(lines)
    transitionsMap: Map[String, Module] = constructConjunctionInputs(Map.from(modules.map((m: Module) => m.name -> m)))
    (_, cycles: Map[String, Long])      =
      (1 to 5000).foldLeft(transitionsMap -> Map.empty[String, Long]) {
        case ((acc: Map[String, Module], cycles: Map[String, Long]), i) =>
          val (newTransitionsMap: Map[String, Module], _, _, newCycles: Map[String, Long]) = buttonPush(acc, i, cycles)
          newTransitionsMap -> newCycles
      }
    rxInputModules: Iterable[String]    =
      transitionsMap
        .values
        .filter(_.next.exists(transitionsMap.values.filter(_.next.contains("rx")).map(_.name).toList.contains(_)))
        .map(_.name)
    res: Option[Long]                   = rxInputModules.map(cycles.get(_)).toList.sequence.map(_.product)
    _                                  <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
