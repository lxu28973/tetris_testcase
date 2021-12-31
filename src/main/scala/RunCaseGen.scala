import java.io._

object RunCaseGen extends App {
  (20 to 600).foreach(i => {
    val writer = new PrintWriter(new File("Module" + i + ".txt" ))
    val genCase = new CaseGen(i, 10, 1.2)
    genCase.gen(writer)
    writer.close()
  })
}
