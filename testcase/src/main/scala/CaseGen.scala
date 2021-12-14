import scala.util.Random

class CaseGen (polyNum: Int){
  val numRec = Random.between(0, polyNum)
  val numL = Random.between(0, polyNum - numRec)
  val numT = polyNum - numL - numRec

  def mx(rec: List[List[Double]], y: Double) = {
    rec.map(a => List(a(0), 2*y - a(1)))
  }

  def my(rec: List[List[Double]], x: Double) = {
    rec.map(a => List(2 * x - a(0), a(1)))
  }

  def genRectagle(xy: List[Double], len: Double, wid: Double) = {
    val x = xy(0)
    val y = xy(1)
    List(List(x,y), List(x + len, y), List(x + len, y + wid), List(x, y + len))
  }

  def getL(xy: List[Double], len: Double, wid: Double) = {
    val l1 = Random.between(0, len)
    val l2 = Random.between(0,wid)
    val rec1 = genRectagle(xy, l1, l2)
    val rec2 = genRectagle(rec1(1), len - l1, wid)
    val state = Random.between(0, 4)
    val lPoly = rec1.take(1) ++ rec2.takeRight(3) ++ rec1.takeRight(2)
    state match {
      case 0 => lPoly
      case 1 => mx(lPoly, xy(1) + wid/2)
      case 2 => my(lPoly, xy(0) + len/2)
      case 3 => mx(my(lPoly, xy(0) + len/2), xy(1) + wid/2)
    }
  }

  def getT(xy: List[Double], len: Double, wid: Double) = {
    val l1 = Random.between(0, len)
    val l2 = Random.between(0, len - l1)
    val w1 = Random.between(0, wid)
    val rec1 = genRectagle(xy, l1, w1)
    val rec2 = genRectagle(rec1(1), l2, wid)
    val rec3 = genRectagle(rec2(1), len - l1 - l2, w1)
    val state = Random.between(0, 4)
    val tPoly = rec1.take(1) ++ rec3.takeRight(3) ++ rec2.slice(1,3) ++ rec1.takeRight(2)
    state match {
      case 0 => tPoly
      case 1 => mx(tPoly, xy(1) + wid/2)
      case 2 => my(tPoly, xy(0) + len/2)
      case 3 => mx(my(tPoly, xy(0) + len/2), xy(1) + wid/2)
    }
  }

}
