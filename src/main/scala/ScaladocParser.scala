import DocToken._

import scala.meta.Syntax
import scala.meta.tokens.Token.Comment

object ScaladocParser {

  def parseScaladoc(comment : Comment) : Seq[DocToken] = {

    val trimmedComment: String = comment.show[Syntax].trim

    require(trimmedComment.startsWith("/*"))

    val lineSeparedDocString: Seq[String] =
      trimmedComment
        .split(Array('\n', '\r')) // Splits the Scaladoc in lines
        .map(_.trim) // Removes leading and trailing doc line whitespaces
        .map(_.dropWhile(_ != ' ').trim) // Removes trailing comments
        .filter(_.nonEmpty) // Removes empty scaladoc lines

    lineSeparedDocString.map(parseScaladocLine)
  }

  private[this] def parseScaladocLine(docLine: String) : DocToken = {
    docLine match {
      // DocConstructor
      case _ if docLine.startsWith("@constructor") =>
        DocConstructor(docLine.replaceFirst("@constructor", "").trim)
      // DocParam
      case _ if docLine.startsWith("@param") =>
        DocParam.tupled(splitNameAndBody(docLine, "@param"))
      // DocTypeParam
      case _ if docLine.startsWith("@tparam") =>
        DocTypeParam.tupled(splitNameAndBody(docLine, "@tparam"))
      // DocReturn
      case _ if docLine.startsWith("@return") =>
        DocReturn(docLine.replaceFirst("@return", "").trim)
      // DocThrows
      case _ if docLine.startsWith("@throws") =>
        DocThrows.tupled(splitNameAndBody(docLine, "@throws"))
      // DocSee
      case _ if docLine.startsWith("@see") =>
        DocSee(docLine.replaceFirst("@see", "").trim)
      // DocNote
      case _ if docLine.startsWith("@note") =>
        DocNote(docLine.replaceFirst("@note", "").trim)
      // DocExample
      case _ if docLine.startsWith("@example") =>
        DocExample(docLine.replaceFirst("@example", "").trim)
      // DocUseCase
      case _ if docLine.startsWith("@usecase") =>
        DocUseCase(docLine.replaceFirst("@usecase", "").trim)
      // DocAuthor
      case _ if docLine.startsWith("@author") =>
        DocAuthor(docLine.replaceFirst("@author", "").trim)
      // DocVersion
      case _ if docLine.startsWith("@version") =>
        DocVersion(docLine.replaceFirst("@version", "").trim)
      // DocSince
      case _ if docLine.startsWith("@since") =>
        DocSince(docLine.replaceFirst("@since", "").trim)
        // DocTodo
      case _ if docLine.startsWith("@todo") =>
        DocTodo(docLine.replaceFirst("@todo", "").trim)
        // TODO: Migration / Deprecated
        // DocInheritDoc
      case _ if docLine.equals("@inheritdoc") =>
        DocInheritDoc
      // DocText

      case text => DocText(text)
    }
  }

  private[this] def splitNameAndBody(docLine: String, lineTag : String) : (String, String) = {
    val nameAndDescription = docLine.replaceFirst(s"$lineTag ", "")
    val name = nameAndDescription.split(' ').head
    val description = nameAndDescription.replaceFirst(s"$name ", "")
    (name, description)
  }
}