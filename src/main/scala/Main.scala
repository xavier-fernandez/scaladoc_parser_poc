
import scala.meta._
import scala.meta.contrib._
import scala.meta.tokens.Token.Comment

object Main extends App {

  val code =
    """
    /**
      * Hola [[scala.Some]]
      */
      class A[T](foo: T)
    """.parse[Source].get

  val comments = AssociatedComments(code.tokens)
  val defnClass = code.collectFirst { case t: Defn.Class => t }.get
  val comment: Comment = comments.leading(defnClass).head


  private val referenceParser = {

    import fastparse.all.{AnyChar, P}
    import fastparse.all._
    import fastparse._

    P(
      ((AnyChar ~ !"[[").rep ~ AnyChar).? ~
      "[[" ~
        ((AnyChar ~ !"]]").rep ~ AnyChar).!  ~
        "]]"
    )
  }

  println(referenceParser.parse(ScaladocParser.parseScaladoc(comment).head.body.get))
}
