import java.io._

object RunCaseGen extends App {
  (20 to 60).foreach(i => {
    val writer = new PrintWriter(new File("Module" + i + ".txt" ))
    val genCase = new CaseGen(i, 80, 150)
    genCase.gen(writer)
    writer.close()
  })
}
