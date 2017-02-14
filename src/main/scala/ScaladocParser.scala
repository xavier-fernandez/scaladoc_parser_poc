import DocToken._

import scala.meta.Syntax
import scala.meta.tokens.Token.Comment

object ScaladocParser {

  def parseScaladoc(comment: Comment): Seq[DocToken] = {

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

  private[this] def parseScaladocLine(docLine: String): DocToken = {
    docLine match {
      // DocConstructor
      case _ if docLine.startsWith("@constructor") =>
        DocToken(DocConstructor, docLine.replaceFirst("@constructor", "").trim)
      // DocParam
      case _ if docLine.startsWith("@param") =>
        prepareMultipleParameterToken(DocParam, docLine, "@param")
      // DocTypeParam
      case _ if docLine.startsWith("@tparam") =>
        prepareMultipleParameterToken(DocTypeParam, docLine, "@tparam")
      // DocReturn
      case _ if docLine.startsWith("@return") =>
        DocToken(DocReturn, docLine.replaceFirst("@return", "").trim)
      // DocThrows
      case _ if docLine.startsWith("@throws") =>
        prepareMultipleParameterToken(DocThrows, docLine, "@throws")
      // DocSee
      case _ if docLine.startsWith("@see") =>
        DocToken(DocSee, docLine.replaceFirst("@see", "").trim)
      // DocNote
      case _ if docLine.startsWith("@note") =>
        DocToken(DocNote, docLine.replaceFirst("@note", "").trim)
      // DocExample
      case _ if docLine.startsWith("@example") =>
        DocToken(DocExample, docLine.replaceFirst("@example", "").trim)
      // DocUseCase
      case _ if docLine.startsWith("@usecase") =>
        DocToken(DocUseCase, docLine.replaceFirst("@usecase", "").trim)
      // DocAuthor
      case _ if docLine.startsWith("@author") =>
        DocToken(DocAuthor, docLine.replaceFirst("@author", "").trim)
      // DocVersion
      case _ if docLine.startsWith("@version") =>
        DocToken(DocVersion, docLine.replaceFirst("@version", "").trim)
      // DocSince
      case _ if docLine.startsWith("@since") =>
        DocToken(DocSince, docLine.replaceFirst("@since", "").trim)
      // DocTodo
      case _ if docLine.startsWith("@todo") =>
        DocToken(DocTodo, docLine.replaceFirst("@todo", "").trim)
      // DocInheritDoc
      case _ if docLine.equals("@inheritdoc") =>
        DocToken(DocInheritDoc, "")
      // DocText
      case text => DocToken(DocText, text)
    }
  }

  private[this] def prepareMultipleParameterToken(docKind: DocKind,
                                                  docLine: String,
                                                  label: String): DocToken = {

    val nameAndDescription = docLine.replaceFirst(s"$label ", "")
    val name: String = nameAndDescription.split(' ').head
    val description: String = nameAndDescription.replaceFirst(s"$name ", "")
    DocToken(docKind, name, description)
  }
}