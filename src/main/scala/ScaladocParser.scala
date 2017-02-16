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
      case _ if docLine.startsWith(DocConstructor.label) =>
        DocToken(DocConstructor, docLine.replaceFirst(DocConstructor.label, "").trim)
      // DocParam
      case _ if docLine.startsWith(DocParam.label) =>
        prepareMultipleParameterToken(DocParam, docLine)
      // DocTypeParam
      case _ if docLine.startsWith(DocTypeParam.label) =>
        prepareMultipleParameterToken(DocTypeParam, docLine)
      // DocReturn
      case _ if docLine.startsWith(DocReturn.label) =>
        DocToken(DocReturn, docLine.replaceFirst(DocReturn.label, "").trim)
      // DocThrows
      case _ if docLine.startsWith(DocThrows.label) =>
        prepareMultipleParameterToken(DocThrows, docLine)
      // DocSee
      case _ if docLine.startsWith(DocSee.label) =>
        DocToken(DocSee, docLine.replaceFirst(DocSee.label, "").trim)
      // DocNote
      case _ if docLine.startsWith(DocNote.label) =>
        DocToken(DocNote, docLine.replaceFirst(DocNote.label, "").trim)
      // DocExample
      case _ if docLine.startsWith(DocExample.label) =>
        DocToken(DocExample, docLine.replaceFirst(DocExample.label, "").trim)
      // DocUseCase
      case _ if docLine.startsWith(DocUseCase.label) =>
        DocToken(DocUseCase, docLine.replaceFirst(DocUseCase.label, "").trim)
      // DocAuthor
      case _ if docLine.startsWith(DocAuthor.label) =>
        DocToken(DocAuthor, docLine.replaceFirst(DocAuthor.label, "").trim)
      // DocVersion
      case _ if docLine.startsWith(DocVersion.label) =>
        DocToken(DocVersion, docLine.replaceFirst(DocVersion.label, "").trim)
      // DocSince
      case _ if docLine.startsWith(DocSince.label) =>
        DocToken(DocSince, docLine.replaceFirst(DocSince.label, "").trim)
      // DocTodo
      case _ if docLine.startsWith(DocTodo.label) =>
        DocToken(DocTodo, docLine.replaceFirst(DocTodo.label, "").trim)
      // DocDeprecated
      case _ if docLine.startsWith(DocDeprecated.label) =>
        DocToken(DocDeprecated, docLine.replaceFirst(DocDeprecated.label, "").trim)
      // DocMigration
      case _ if docLine.startsWith(DocMigration.label) =>
        DocToken(DocMigration, docLine.replaceFirst(DocMigration.label, "").trim)
      // DocInheritDoc
      case _ if docLine.equals(DocInheritDoc.label) =>
        DocToken(DocInheritDoc, "")
      // DocText
      case text => DocToken(DocText, text)
    }
  }

  private[this] def prepareMultipleParameterToken(docKind: DocKind, docLine: String): DocToken = {

    val nameAndDescription = docLine.replaceFirst(s"${docKind.label} ", "")
    val name: String = nameAndDescription.split(' ').head
    val description: String = nameAndDescription.replaceFirst(s"$name ", "")
    DocToken(docKind, name, description)
  }
}
