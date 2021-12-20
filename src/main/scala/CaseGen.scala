import java.io.Writer
import scala.util.Random
import scala.math.{abs, min, random, sqrt}

class Module (val boundary: List[List[Double]], val ports: List[List[List[Double]]]) {
  def print(writer: Writer) = {
    writer.write("Boundary:")
    boundary.foreach(p => writer.write("(" + p(0).formatted("%.1f") + ", " + p(1).formatted("%.1f") + ")"))
    writer.write(";GATE\n")
    ports.zipWithIndex.foreach(
      (ps) => {
        writer.write("Port:")
        ps._1.foreach(p => writer.write("(" + p(0).formatted("%.1f") + ", " + p(1).formatted("%.1f") + ")"))
        if (ps._2 == 1)
          writer.write(";GATE\n")
        else
          writer.write(";SD\n")
      }
    )
  }
}

class CaseGen (polyNum: Int, cutNum: Int, ratio: Double){
  val numRec = Random.between(0, polyNum / 3)
  val numL = Random.between((polyNum - numRec) / 2, polyNum - numRec)
  val numT = polyNum - numL - numRec
  val lenLow: Double = 80
  val lenHigh: Double = 150
  def gen(writer: Writer) = {
//    Random.setSeed(5)
    val area = lenHigh * lenHigh * polyNum * ratio
    val minX = lenHigh * sqrt(polyNum) / 2
    val areaX = Random.between(minX, area / minX)
    val areaY = area / areaX
    val areaPoly = genRectagle(List(0,0), areaX, areaY)
    writer.write("Area:")
    val areaBoundary = rearrangePoints(cutPoint(areaPoly.boundary, cutNum))
    areaBoundary.foreach(p => writer.write("(" + p(0).formatted("%.1f") + ", " + p(1).formatted("%.1f") + ")"))
    writer.write("\n")
    writer.write("Rule:GATE(5,5);SD(5,5);GATE_SD(0.5);GATE_ITO(0.5);SD_ITO(0.5)\n")

    (1 to numRec).foreach(i => {
      val len = Random.between(lenLow, lenHigh)
      val wid = Random.between(lenLow, lenHigh)
      val rec = genRectagle(List(0,0), len, wid)
      writer.write("Module:M" + i + "\n")
      rec.print(writer)
    })

    (numRec+1 to numRec+numL).foreach(i => {
      val len = Random.between(lenLow, lenHigh)
      val wid = Random.between(lenLow, lenHigh)
      val rec = genL(List(0,0), len, wid)
      writer.write("Module:M" + i + "\n")
      rec.print(writer)
    })

    (numRec+numL+1 to numRec+numL+numT).foreach(i => {
      val len = Random.between(lenLow, lenHigh)
      val wid = Random.between(lenLow, lenHigh)
      val rec = genT(List(0,0), len, wid)
      writer.write("Module:M" + i + "\n")
      rec.print(writer)
    })
  }

  def mx(rec: List[List[Double]], y: Double) = {
    rec.map(a => List(a(0), 2*y - a(1)))
  }

  def my(rec: List[List[Double]], x: Double) = {
    rec.map(a => List(2 * x - a(0), a(1)))
  }

  def change(lPoly: List[List[Double]], state: Int, x: Double, y: Double): List[List[Double]] = {
    state match {
      case 0 => lPoly
      case 1 => mx(lPoly, y)
      case 2 => my(lPoly, x)
      case 3 => mx(my(lPoly, x), y)
    }
  }

  def change(lPoly: Module, state: Int, x: Double, y: Double): Module = {
    new Module(change(lPoly.boundary, state, x, y), List(change(lPoly.ports(0), state, x, y),
      change(lPoly.ports(1), state, x, y), change(lPoly.ports(2), state, x, y)))
  }

  def rearrangePoints(ls: List[List[Double]]) = {
    val maxX = ls.map(p => p(0)).max
    val maxY = ls.map(p => p(1)).max
    val part = List(ls.filter(p => p(0) < maxX/2 && p(1) < maxY/2), ls.filter(p => p(0) >= maxX/2 && p(1) < maxY/2),
      ls.filter(p => p(0) >= maxX/2 && p(1) >= maxY/2), ls.filter(p => p(0) < maxX/2 && p(1) >= maxY/2))
    val p0 =  part(0).sortWith((a, b) => (a(0) <= b(0) && a(1) >= b(1)))
    val p1 = part(1).sortWith((a, b) => (a(0) <= b(0) && a(1) <= b(1)))
    val p2 = part(2).sortWith((a, b) => (a(0) >= b(0) && a(1) <= b(1)))
    val p3 = part(3).sortWith((a, b) => (a(0) >= b(0) && a(1) >= b(1)))
    p0 ++ p1 ++ p2 ++ p3
  }

  def cutPoint(ls: List[List[Double]], cut: Int, cutPs: List[List[Double]] = Nil): List[List[Double]] = {
    val x = ls.map(p => p(0)).max
    val y = ls.map(p => p(1)).max
    val maxX = x / 5
    val maxY = y / 5

    val allPs = ls ++ cutPs
    cut match {
      case 0 => allPs
      case _ => {
        val num = Random.between(0, 3)
        val cutP = ls(num)
        val qx = allPs.find(p => p(0) == cutP(0) && p(1) != cutP(1))
        val qy = allPs.find(p => p(1) == cutP(1) && p(0) != cutP(0))
        val dX = min(abs(qy.getOrElse(List[Double](0.0,0.0))(0) - cutP(0)), maxX)
        val dY = min(abs(qx.getOrElse(List[Double](0.0,0.0))(1) - cutP(1)), maxY)
        val cX = Random.between(dX / 3, dX * 2 / 3)
        val cY = Random.between(dY / 3, dY * 2 / 3)
        val cutAddPsX = if ((cutP(0) < x / 2)) {
          val newX = cutP(0) + cX
          if (newX < x / 2) Some(newX) else {
            println("Warning: not cut X"); None
          }
        } else {
          val newX = cutP(0) - cX
          if (newX > x / 2) Some(newX) else {
            println("Warning: not cut X"); None
          }
        }
        val cutAddPsY = if ((cutP(1) < y / 2)) {
          val newY = cutP(1) + cY
          if (newY < y / 2) Some(newY) else {
            println("Warning: not cut Y"); None
          }
        } else {
          val newY = cutP(1) - cY
          if (newY > y / 2) Some(newY) else {
            println("Warning: not cut Y"); None
          }
        }
        val cutAddPx = cutAddPsX match {
          case Some(x) => List(List(x, cutP(1)))
          case None => Nil
        }
        val cutAddPy = cutAddPsY match {
          case Some(y) => List(List(cutP(0), y))
          case None => Nil
        }
        val cutAddPs = cutAddPx ++ cutAddPy
        val nCutPs = cutAddPs match {
          case List(a, b) => List(a(0), b(1)) +: cutPs
          case _ => cutPs
        }
        val nToCutPs = cutAddPs match {
          case List(a, b) => ls.take(num) ++ ls.takeRight(ls.size - num - 1) ++ cutAddPs
          case _ => ls
        }
        cutPoint(nToCutPs, cut - 1, nCutPs)
      }
    }
  }

  def genRectagle(xy: List[Double], len: Double, wid: Double) = {
    val x = xy(0)
    val y = xy(1)
    val boundary = List(List(x,y), List(x + len, y), List(x + len, y + wid), List(x, y + wid))
    val port1 = List(List(x, y), List(x + len/10, y), List(x + len/10, y + wid), List(x, y + wid))
    val port2 = List(List(x,y), List(x + len, y), List(x + len, y + wid), List(x, y + len))
    val port3 = List(List(x + 9*len/10, y), List(x + len, y), List(x + len, y + wid), List(x + 9*len/10, y + wid))
    new Module(boundary, List(port1, port2, port3))
  }

  def genL(xy: List[Double], len: Double, wid: Double) = {
    val l1 = Random.between(1.0/3 * len, 2.0/3 * len)
    val l2 = Random.between(1.0/3 * wid, 2.0/3 * wid)
    val rec1 = genRectagle(xy, l1, l2)
    val rec2 = genRectagle(rec1.boundary(1), len - l1, wid)
    val state = Random.between(0, 4)
    val lPoly = rec1.boundary.take(1) ++ rec2.boundary.takeRight(3) ++ rec1.boundary.takeRight(2)
    val lPort = rec1.ports.take(1) ++ rec2.ports.takeRight(2)
    val lModule = new Module(lPoly, lPort)
    val b = xy(1) + wid/2
    val a = xy(0) + len/2
    change(lModule, state, a, b)
  }

  def genT(xy: List[Double], len: Double, wid: Double) = {
    val l1 = Random.between(1.0/6 * len, 1.0/2 * len)
    val l2 = Random.between(1.0/3 * (len - l1), 2.0/3 * (len - l1))
    val w1 = Random.between(1.0/2 * wid, wid)
    val rec1 = genRectagle(xy, l1, w1)
    val rec2 = genRectagle(rec1.boundary(1), l2, wid)
    val rec3 = genRectagle(rec2.boundary(1), len - l1 - l2, w1)
    val state = Random.between(0, 4)
    val tPoly = rec1.boundary.take(1) ++ rec3.boundary.takeRight(3) ++ rec2.boundary.slice(2,4) ++ rec1.boundary.takeRight(2)
    val tPort = rec1.ports.take(1) ++ rec2.ports.takeRight(2).take(1) ++ rec3.ports.takeRight(1)
    val tModule = new Module(tPoly, tPort)
    val b = xy(1) + wid/2
    val a = xy(0) + len/2
    change(tModule, state, a, b)
  }

}
