
import scala.meta._
import scala.meta.contrib._
import scala.meta.tokens.Token.Comment

object Main extends App {

  val code =
    """
    /**
      * Test description
      *
      * @param Foo bar
      * @constructor Main constructor
      *
      * {{{
      * def foo: Int = 4
      * }}}
      * Bla Bla Bla
      */
      class A[T](foo: T)
    """.parse[Source].get

  val comments = AssociatedComments(code.tokens)
  val defnClass = code.collectFirst { case t: Defn.Class => t }.get
  val comment: Comment = comments.leading(defnClass).head

  println(ScaladocParser.parseScaladoc(comment))
}
