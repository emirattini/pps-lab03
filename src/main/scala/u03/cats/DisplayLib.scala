package u03.cats

object DisplayLib:

  trait Display[A]:
    def display(value: A): String

  object Display:
    given Display[String] with
      override def display(value: String): String = value

    given Display[Int] with
      override def display(value: Int): String = value.toString

    given Display[Cat] with
      override def display(value: Cat): String =
        s"${Display.display(value.name)} is a " +
          s"${Display.display(value.age)} year-old " +
          s"${Display.display(value.color)} cat"

    def display[A](value: A)(using p: Display[A]): String =
      p.display(value)

    def print[A](value: A)(using Display[A]): Unit =
      println(display(value))

  final case class Cat(name: String, age: Int, color: String)

object DisplaySyntax:
  import DisplayLib.Display
  extension [A](value: A)(using p: Display[A])
    def display: String = p.display(value)
    def print: Unit = Display.print(value)


@main def tryDisplayLib(): Unit =
  import DisplayLib.*
  import DisplaySyntax.{display, print}

  val mia = Cat("Mia", 8, "tigrato")
  print(mia.display)
  mia.print
