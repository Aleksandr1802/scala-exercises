
object main {

  def main(args: Array[String]): Unit = {
    println(Section5.GenStreamFibonacci().takeWhile(x => x < 1000).toList)

  }


}
