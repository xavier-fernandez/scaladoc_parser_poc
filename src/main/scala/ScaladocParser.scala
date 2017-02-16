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

    mergeTokens(
      lineSeparedDocString.map(parseScaladocLine)
    )
  }

  /**
    * Once the Scaladoc parsing is done, merges multiline [[DocToken]].
    */
  private[this] def mergeTokens(docTokens: Seq[DocToken]): Seq[DocToken] = {
    docTokens.foldLeft(Seq[DocToken]()) {
      (acc: Seq[DocToken], nextToken: DocToken) => {
        acc.lastOption match {
          // If the next token is a DocText, append it to the previous token
          case Some(previousToken) if nextToken.kind.equals(DocText) =>
            acc.dropRight(1) :+ previousToken.append(nextToken)
          // If the documentation is the first one allow everything
          case _ =>
            acc :+ nextToken
        }
      }
    }
  }

  private[this] def parseScaladocLine(docLine: String): DocToken = {
    docLine match {
      // DocConstructor
      case _ if docLine.startsWith(DocConstructor.label) => prepareSingleParameterToken(DocConstructor, docLine)
      // DocParam
      case _ if docLine.startsWith(DocParam.label) => prepareMultipleParameterToken(DocParam, docLine)
      // DocTypeParam
      case _ if docLine.startsWith(DocTypeParam.label) => prepareMultipleParameterToken(DocTypeParam, docLine)
      // DocReturn
      case _ if docLine.startsWith(DocReturn.label) => prepareSingleParameterToken(DocReturn, docLine)
      // DocThrows
      case _ if docLine.startsWith(DocThrows.label) => prepareMultipleParameterToken(DocThrows, docLine)
      // DocSee
      case _ if docLine.startsWith(DocSee.label) => prepareSingleParameterToken(DocSee, docLine)
      // DocNote
      case _ if docLine.startsWith(DocNote.label) => prepareSingleParameterToken(DocNote, docLine)
      // DocExample
      case _ if docLine.startsWith(DocExample.label) => prepareSingleParameterToken(DocExample, docLine)
      // DocUseCase
      case _ if docLine.startsWith(DocUseCase.label) => prepareSingleParameterToken(DocUseCase, docLine)
      // DocAuthor
      case _ if docLine.startsWith(DocAuthor.label) => prepareSingleParameterToken(DocAuthor, docLine)
      // DocVersion
      case _ if docLine.startsWith(DocVersion.label) => prepareSingleParameterToken(DocVersion, docLine)
      // DocSince
      case _ if docLine.startsWith(DocSince.label) => prepareSingleParameterToken(DocSince, docLine)
      // DocTodo
      case _ if docLine.startsWith(DocTodo.label) => prepareSingleParameterToken(DocTodo, docLine)
      // DocDeprecated
      case _ if docLine.startsWith(DocDeprecated.label) => prepareSingleParameterToken(DocDeprecated, docLine)
      // DocMigration
      case _ if docLine.startsWith(DocMigration.label) => prepareSingleParameterToken(DocMigration, docLine)
      // DocInheritDoc
      case _ if docLine.equals(DocInheritDoc.label) => DocToken(DocInheritDoc, "")
      // DocOtherTag
      case _ if docLine.startsWith(DocOtherTag.label) =>
        val label = docLine.trim.split(" ").head
        val description = docLine.replaceFirst(label, "").trim
        DocToken(DocOtherTag, label, description)
      // DocText
      case text => DocToken(DocText, text)
    }
  }

  private[this] def prepareSingleParameterToken(docKind: DocKind, docLine: String): DocToken = {
    DocToken(docKind, docLine.replaceFirst(docKind.label, "").trim)
  }

  private[this] def prepareMultipleParameterToken(docKind: DocKind, docLine: String): DocToken = {

    val nameAndDescription = docLine.replaceFirst(s"${docKind.label} ", "")
    val name: String = nameAndDescription.split(' ').head
    val description: String = nameAndDescription.replaceFirst(s"$name ", "")
    DocToken(docKind, name, description)
  }
}
