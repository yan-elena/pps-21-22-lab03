package u02

object SumTypes extends App {

  // Sum type: a sealed base trait, and cases extending it
  sealed trait Person //sealed: no other impl. except Student, Teacher
  case class Student(name: String, year: Int) extends Person
  case class Teacher(name: String, course: String) extends Person

  def name(p: Person): String = p match
    case Student(n, _) => n
    case Teacher(n, _) => n

  println(name(Student("mario", 2015)))

  // A LinkedList of Int
  sealed trait IntList
  case class IntListCons(head: Int, tail: IntList) extends IntList
  case class IntListNil() extends IntList

  def sum(l: IntList): Int = l match
    case IntListCons(h, t) => h + sum(t)
    case _ => 0

  println(sum(IntListCons(10, IntListCons(20, IntListCons(30, IntListNil())))))

}
