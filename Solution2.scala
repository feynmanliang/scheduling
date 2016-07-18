import scala.collection.mutable

object Solution extends App {
  abstract trait Time {
    val time: Int
    val intervalId: Int
    val userId: Int
  }
  case class Interval(startTime: Int, stopTime: Int)
  case class Start(
    override val time: Int,
    override val intervalId: Int,
    override val userId: Int) extends Time
  case class End(
    override val time: Int,
    val date: Double,
    override val intervalId: Int,
    override val userId: Int) extends Time

  def merge(busyIntervals: List[List[Interval]]): List[Time] = {
    busyIntervals.zipWithIndex.flatMap { case (user, userId) =>
      user.zipWithIndex.flatMap { case (interval, intervalId) =>
        List(
          Start(interval.startTime, intervalId, userId),
          End(interval.stopTime, intervalId, userId)
        )
      }
    }.sortBy(_.time)
    // TODO: sort using SortedSet on fringe times
  }

  def freeIntervals(startStopTimes: List[Time]): List[Interval] = {
    val res = mutable.ListBuffer[Interval]()

    var isFree = false
    var freeStart = 0

    val active = mutable.Set[(Int, Int)]()
    for (t <- startStopTimes) {
      t match {
        case End(time, intervalId, userId) => {
          active.remove((intervalId, userId))
          if (active.isEmpty) {
            isFree = true
            freeStart = t.time
          }
        }
        case Start(time, intervalId, userId) => {
          active.add((intervalId, userId))
          if (isFree && freeStart != time) {
            res += Interval(freeStart, time)
          }
          isFree = false
        }
      }
    }

    res.toList
  }

  val in1 = List(
    List(Interval(1, 3), Interval(6, 7))
  )

  println(merge(in1))
  println(freeIntervals(merge(in1)))
}

// vim: set ts=2 sw=2 et sts=2:
