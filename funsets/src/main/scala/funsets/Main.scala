package funsets

object Main extends App {
  import FunSets._
  singletonSet(1)
  println(contains(singletonSet(1), 1))
}
