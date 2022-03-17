package u03

import org.junit.Assert.assertEquals
import org.junit.Test
import Lists.*

class StreamTest:
  import Streams.*
  import List.*

  @Test def dropTest() =
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(Stream.take(Stream.iterate(0)(_ + 1))(10))(6)))

  @Test def constantTest() =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(Stream.constant("x"))(5)))
    
  @Test def fibsTest() =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), Stream.toList(Stream.take(Stream.fibs)(8)))