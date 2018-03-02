import java.io._

import scala.annotation.tailrec

object HashCode {
  case class Car(pos: (Int, Int), step: Int, rides: Vector[Ride], canRide: Boolean)
  case class Ride(index: Int, start: (Int, Int), end: (Int, Int), es: Int, dl: Int, valid: Boolean)
  case class Input(rides: Vector[Ride], vehicles: Int, rideCount: Int, bonus: Int, steps: Int)

  def readFile(filename: String): Input = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = bufferedSource.getLines()
    val first = lines.next().split(' ')

    val rides = (for {
      (line, i) <- bufferedSource.getLines().zipWithIndex
      row = line.split(' ').toVector.map(_.toInt)
      ride = Ride(i, row(0) -> row(1), row(2) -> row(3), row(4), row(5), true)
    } yield ride).toVector

    bufferedSource.close
    Input(rides, first(2).toInt, first(3).toInt, first(4).toInt, first(5).toInt)
  }

  def distance(p1:(Int, Int), p2: (Int, Int)) = Math.abs(p2._1 - p1._1) + Math.abs(p2._2 - p1._2)

  def weight(car: Car, ride: Ride): Float =
    ride.es  / (car.step + 1 + ((distance(car.pos, ride.start))))

  def weight2(car: Car, ride: Ride): Float =
    (distance(ride.start, ride.end) + ride.es) / (car.step + ((distance(car.pos, ride.start))))

  def clamp0(x: Int): Int = if (x < 0) 0 else x

  @tailrec
  def algorithm(rides: Vector[Ride], vehicles: Vector[Car], car: Int): Vector[Car] = {
    val current = vehicles(car)
    val isValid = (car: Car, r: Ride) => r.dl > (car.step + distance(car.pos, r.start) + distance(r.start, r.end))
    val validRides = rides.filter(r => r.valid)
    lazy val reallyValid = validRides.filter(isValid(current, _))

    if (validRides.isEmpty || vehicles.filter(_.canRide == false).size == vehicles.size)
      vehicles
    else if(reallyValid.isEmpty || !current.canRide)
      algorithm(rides, vehicles.updated(car, current.copy(canRide = false)), (car + 1) % vehicles.size)
    else {
      val ride = reallyValid.sortBy(weight2(current, _)).last
      val wait = clamp0(ride.es - current.step + distance(current.pos, ride.start))
      val nStep = current.step + distance(current.pos, ride.start) + wait
      val nCar = current.copy(pos = ride.end, step = nStep, rides = current.rides :+ ride)
      val nRides = rides.updated(ride.index, ride.copy(valid = false))
      algorithm(nRides, vehicles.updated(car, nCar), (car + 1) % vehicles.size)
    }
  }

  def makeOutput(data: Vector[Car]): String = (for {
    c <- data
    sr = c.rides.map { _.index }.mkString(" ")
    r = s"${c.rides.size} $sr"
  } yield r).mkString("\n")

  def main(args: Array[String]): Unit = {
    val input = readFile(args lift(0) getOrElse("input.in"))
    val file = new File(args lift(1) getOrElse("output.in"))
    val vehicles = Vector.fill(input.vehicles)(Car((0,0), 0, Vector.empty, true))
    val result = algorithm(input.rides, vehicles, 0)
    val bw = new BufferedWriter(new FileWriter(file))
    val output = makeOutput(result)
    bw.write(output)
    println("Done !!!!")
    bw.close()
  }
}
