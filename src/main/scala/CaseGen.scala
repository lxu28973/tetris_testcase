import java.io.Writer
import scala.util.Random

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

class CaseGen (polyNum: Int){
  val numRec = Random.between(0, polyNum)
  val numL = Random.between(0, polyNum - numRec)
  val numT = polyNum - numL - numRec

  def gen(writer: Writer) = {
    val area = 14400 * polyNum
    val areaX = Random.between(120.0, area / 120.0)
    val areaY = area / areaX
    val areaPoly = genRectagle(List(0,0), areaX, areaY)
    writer.write("Area:")
    areaPoly.boundary.foreach(p => writer.write("(" + p(0).formatted("%.1f") + ", " + p(1).formatted("%.1f") + ")"))
    writer.write("\n")
    writer.write("Rule:GATE(5,5);SD(5,5);GATE_SD(0.5);GATE_ITO(0.5);SD_ITO(0.5)\n")

    (1 to numRec).foreach(i => {
      val len = Random.between(50.0, 120.0)
      val wid = Random.between(50.0, 120.0)
      val rec = genRectagle(List(0,0), len, wid)
      writer.write("Module:M" + i + "\n")
      rec.print(writer)
    })

    (numRec+1 to numRec+numL).foreach(i => {
      val len = Random.between(50.0, 120.0)
      val wid = Random.between(50.0, 120.0)
      val rec = genL(List(0,0), len, wid)
      writer.write("Module:M" + i + "\n")
      rec.print(writer)
    })

    (numRec+numL+1 to numRec+numL+numT).foreach(i => {
      val len = Random.between(50.0, 120.0)
      val wid = Random.between(50.0, 120.0)
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
