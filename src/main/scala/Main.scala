
import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.AssociatedComments
import scala.meta.tokens.Token.Comment

object Main extends App {

  val code =
    """
    /** this is docstring
      * and this is a multiline continuation.
      * @constructor the constructor description
      * and this is a constructor continuation.
      * @param foo bar
      * @tparam T a
      * @throws A RuntimeException
      * @note this is a test class
      * @see other classes
      * @example with an example.
      * @usecase test scaladoc parser
      * @author xavierfernandez
      * @version 0.2
      * @since 0.1
      * @todo consider using fastparser
      * @deprecated message version
      * @inheritdoc
      * @othertag with body
      */
      class A[T](foo: T)
    """.parse[Source].get

  val comments = AssociatedComments(code.tokens)
  val defnClass = code.collectFirst { case t: Defn.Class => t }.get
  val comment: Comment = comments.leading(defnClass).head

  println(ScaladocParser.parseScaladoc(comment))

}

