package u03.cats

object FunctionalBooleans:
  trait Bool:
    def `if`[A](t: A)(f: A): A

  val True: Bool = new Bool:
    override def `if`[A](t: A)(f: A): A = t
  val False: Bool = new Bool:
    override def `if`[A](t: A)(f: A): A = f

  def and(b1: Bool, b2: Bool): Bool =
    new Bool:
      override def `if`[A](t: A)(f: A): A =
        b1.`if`(b2)(False).`if`(t)(f)

  def or(b1: Bool, b2: Bool): Bool =
    new Bool:
      override def `if`[A](t: A)(f: A): A =
        b1.`if`(True)(b2).`if`(t)(f)

  def not(b: Bool): Bool =
    new Bool:
      override def `if`[A](t: A)(f: A): A =
        b.`if`(f)(t)

@main def tryFunctionalBooleans(): Unit =
  import FunctionalBooleans.*
  println(and(True, True).`if`("yes")("no"))
  println(and(False, True).`if`("yes")("no"))

  println(or(True, False).`if`("yes")("no"))
  println(not(True).`if`("yes")("no"))
