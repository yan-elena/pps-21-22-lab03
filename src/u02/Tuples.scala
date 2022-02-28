package u02

object Tuples extends App {

  case class Tup1[A](a: A)
  case class Tup2[A, B](a: A, b: B)
  case class Tup3[A, B, C](a: A, b: B, c: C)
  case class Tup4[A, B, C, D](a: A, b: B, c: C, d: D)

  val pair = Tup2(10, true)
  val pairTyped: Tup2[Int, Boolean] = Tup2(10, true)

  println((pair, pairTyped, pair == pairTyped)) // Tup2(10,true), Tup2(10,true), true

  def checkFirstComponent[A, B](pair: Tup2[A, B], element: A): Boolean = pair match
    case Tup2(e1, e2) if e1 == element => true
    case _ => false

  // note type inference: A=Int, B=Boolean
  println(checkFirstComponent(pair, 10)) // true
  // the following: type mismatch.. not detected by IntelliJ editor
  // println(checkFirstComponent(tu,"a"))
  // Built-in tuples: Tuple2, Tuple3, .... Tuple22, aliased to (,) (,,) (,,,)
  val builtinTuple = (10, true)
  val tupleNoSugar = Tuple2(10, true)

  println((builtinTuple, tupleNoSugar, builtinTuple == tupleNoSugar)) // (10,true), (10,true), true
}
