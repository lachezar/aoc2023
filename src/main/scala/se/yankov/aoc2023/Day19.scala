package se.yankov.aoc2023

import cats.effect.{ IO, IOApp }
import scala.annotation.tailrec

object Day19 extends IOApp.Simple {

  extension [A, B](t: Tuple2[A, A]) def bimap(f: (A => B)): Tuple2[B, B] = f(t._1) -> f(t._2)

  enum Property:
    case X, M, A, S

  enum Operator:
    case LT, GT

  final case class Part(x: Int, m: Int, a: Int, s: Int):
    def run(workflowName: WorkflowName, workflows: Map[WorkflowName, Workflow]): Accepted | Rejected =
      val workflow: Workflow = workflows(workflowName)
      workflow.rules.flatMap(_.run(this)).headOption.getOrElse(workflow.elseRule) match
        case name: WorkflowName => run(name, workflows)
        case accepted: Accepted => accepted
        case rejected: Rejected => rejected

    def getPropertyValue: Property => Int = {
      case Property.X => x
      case Property.M => m
      case Property.A => a
      case Property.S => s
    }

  final case class PossibleParts(x: Seq[Int], m: Seq[Int], a: Seq[Int], s: Seq[Int]):
    def partitionByProperty(property: Property, cond: (Int) => Boolean): (PossibleParts, PossibleParts) =
      property match
        case Property.X => x.partition(cond).bimap((x: Seq[Int]) => copy(x = x))
        case Property.M => m.partition(cond).bimap((m: Seq[Int]) => copy(m = m))
        case Property.A => a.partition(cond).bimap((a: Seq[Int]) => copy(a = a))
        case Property.S => s.partition(cond).bimap((s: Seq[Int]) => copy(s = s))
    def allCombinations: Long                                                                           = x.size.toLong * m.size * a.size * s.size

  final case class Rule(property: Property, op: Operator, value: Int, gotoWorkflow: WorkflowName | Accepted | Rejected):
    def run(part: Part): Option[WorkflowName | Accepted | Rejected]             = op match
      case Operator.LT => Option.when(part.getPropertyValue(property) < value)(gotoWorkflow)
      case Operator.GT => Option.when(part.getPropertyValue(property) > value)(gotoWorkflow)
    def partition(possibleParts: PossibleParts): (PossibleParts, PossibleParts) = op match
      case Operator.LT => possibleParts.partitionByProperty(property, (_ < value))
      case Operator.GT => possibleParts.partitionByProperty(property, (_ > value))

  final case class Accepted()
  final case class Rejected()

  opaque type WorkflowName = String

  final case class Workflow(name: WorkflowName, rules: Vector[Rule], elseRule: WorkflowName | Accepted | Rejected)

  object Workflow:
    def apply(name: WorkflowName, rawRules: Vector[String]): Workflow =
      val rules: Vector[Rule] = rawRules.dropRight(1).map {
        case s"$property<$value:$transition" =>
          Rule(Property.valueOf(property.toUpperCase), Operator.LT, value.toInt, parseTransition(transition))
        case s"$property>$value:$transition" =>
          Rule(Property.valueOf(property.toUpperCase), Operator.GT, value.toInt, parseTransition(transition))
      }
      Workflow(name, rules, parseTransition(rawRules.last))

  def parseTransition(transition: WorkflowName): WorkflowName | Accepted | Rejected =
    transition match
      case "A"          => Accepted()
      case "R"          => Rejected()
      case name: String => name

  def parse(lines: List[String]): (Map[WorkflowName, Workflow], List[Part]) =
    lines.foldLeft(Map.empty[WorkflowName, Workflow] -> List.empty[Part]) {
      case ((workflowsMap: Map[WorkflowName, Workflow], parts: List[Part]), line: String) =>
        line match
          case s"$name{$rules}" if !name.isEmpty =>
            workflowsMap.updated(name, Workflow(name, rules.split(',').toVector)) -> parts
          case s"{x=$x,m=$m,a=$a,s=$s}"          => (workflowsMap, Part(x.toInt, m.toInt, a.toInt, s.toInt) :: parts)
          case _                                 => (workflowsMap, parts)
    }

  def partition(rules: Vector[Rule], possibleParts: PossibleParts): List[PossibleParts] =
    rules match
      case rule +: rest if !rest.isEmpty =>
        val (mainPart, elsePart) = rule.partition(possibleParts)
        mainPart +: partition(rest, elsePart)
      case rule +: rest                  => rule.partition(possibleParts).toList
      case _                             => Nil

  @tailrec
  def walk(workflowsMap: Map[WorkflowName, Workflow], stack: List[(WorkflowName, PossibleParts)], max: Long): Long =
    stack match
      case Nil                                                                                     => max
      case (name -> (possibleParts: PossibleParts)) :: (next: List[(WorkflowName, PossibleParts)]) =>
        val workflow: Workflow                                            = workflowsMap(name)
        val (newStack: List[(WorkflowName, PossibleParts)], newMax: Long) =
          (workflow.rules.map(_.gotoWorkflow) :+ workflow.elseRule)
            .zip(partition(workflow.rules, possibleParts))
            .foldLeft(List.empty[(WorkflowName, PossibleParts)] -> 0L) {
              case ((stackAcc, maxAcc), (name: WorkflowName, possibleParts)) =>
                ((name -> possibleParts) :: stackAcc) -> maxAcc
              case ((stackAcc, maxAcc), (_: Accepted, possibleParts))        =>
                stackAcc -> (maxAcc + possibleParts.allCombinations)
              case (acc, _)                                                  => acc
            }

        walk(workflowsMap, newStack ++ next, max + newMax)

  val task1: IO[Unit] = for {
    lines: List[String]                                           <- Utils.readLines[IO]("day19.input.txt").compile.toList
    (workflowsMap: Map[WorkflowName, Workflow], parts: List[Part]) = parse(lines)
    res: Int                                                       = parts
                                                                       .map((p: Part) => p -> p.run("in", workflowsMap))
                                                                       .collect { case (p: Part, Accepted()) => p.x + p.m + p.a + p.s }
                                                                       .sum
    _                                                             <- IO.println(res)
  } yield ()

  val task2: IO[Unit] = for {
    lines: List[String]                                       <- Utils.readLines[IO]("day19.input.txt").compile.toList
    (workflowsMap: Map[WorkflowName, Workflow], _: List[Part]) = parse(lines)
    initialRating: List[Int]                                   = (1 to 4000).toList
    initialPossibleParts: PossibleParts                        = PossibleParts(initialRating, initialRating, initialRating, initialRating)
    res: Long                                                  = walk(workflowsMap, ("in" -> initialPossibleParts) :: Nil, 0L)
    _                                                         <- IO.println(res)
  } yield ()

  def run: IO[Unit] = task1 >> task2
}
