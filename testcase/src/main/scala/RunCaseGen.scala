import java.io._

object RunCaseGen extends App {
  val writer = new PrintWriter(new File("test.txt" ))
  val genCase = new CaseGen(40)
  val rec = genCase.gen(writer)
  writer.close()
}
