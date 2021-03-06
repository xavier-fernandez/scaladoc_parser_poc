import org.scalatest.FunSuite

import scala.meta.contrib._
import scala.meta.{Defn, _}
import DocToken._

import scala.meta.tokens.Token.Comment

/**
  * Test for [[ScaladocParser]]
  */
class ScaladocParserTest extends FunSuite {

  private[this] def parseString(commentCode: String): Seq[DocToken] = {
    val code = commentCode.parse[Source].get
    val comments = AssociatedComments(code.tokens)
    val defnClass = code.collectFirst { case t: Defn.Class => t }.get
    val comment: Comment = comments.leading(defnClass).head
    ScaladocParser.parseScaladoc(comment)
  }

  private[this] def generateTestString(docKind: TagKind): String =
    s"${docKind.label} ${(0 until docKind.numberParameters).map(i => s"Test$docKind$i").mkString(" ")}"

  test("example usage") {
    assert(
      parseString(
        """
          | /** Example scaladoc **/
          | case class foo(bar: String)
        """.stripMargin
      ).toString() === "List(Description(Example scaladoc))"
    )
  }

  test("indentation checks") {

    val expectedBody: String = "BODY"
    val expectedResult: Seq[DocToken] = Seq(DocToken(Description, expectedBody))

    assert(
      parseString(
        s"""
         /** $expectedBody*/
          case class foo(bar: String)
         """
      ) === expectedResult
    )
    assert(
      parseString(
        s"""
         /** $expectedBody
          */
          case class foo(bar: String)
         """
      ) === expectedResult
    )
    assert(
      parseString(
        s"""
         /**       $expectedBody
          */
          case class foo(bar: String)
         """
      ) === expectedResult
    )
    assert(
      parseString(
        s"""
         /**
          *$expectedBody
          */
          case class foo(bar: String)
         """
      ) === expectedResult
    )
  }

  test("paragraph parsing") {
    val descriptionBody = "Description Body"
    assert(
      parseString(
        s"""
         /**
          *
          *$descriptionBody
          *
          *$descriptionBody
          *
          */
          case class foo(bar: String)
         """
      ) === Seq(
        DocToken(Description, descriptionBody),
        DocToken(Paragraph),
        DocToken(Description, descriptionBody)
      )
    )
  }

  test("code blocks") {

    val testDescription = "This is a codeblock:"

    val codeBlock1 = "\"HELLO MARIANO\""
    val codeBlock2 = "\"HELLO SORAYA\""
    val complexCodeBlock =
      """
        |ggmqwogmwogmqwomgq
        |gmqwgoiqmgoqmwomw
      """.stripMargin.trim

    val result: Seq[DocToken] =
      parseString(
        s"""
          /**
            * $testDescription {{{ $codeBlock1 }}}
            * $testDescription
            * {{{ $codeBlock2 }}}
            *
            * $testDescription
            *
            * {{{
            *   $complexCodeBlock
            * }}}
            */
            case class foo(bar : String)
       """.stripMargin
      )

    val expectation = Seq(
      DocToken(Description, testDescription),
      DocToken(CodeBlock, codeBlock1),
      DocToken(Description, testDescription),
      DocToken(CodeBlock, codeBlock2),
      DocToken(Paragraph),
      DocToken(Description, testDescription),
      DocToken(Paragraph),
      DocToken(CodeBlock, complexCodeBlock)
    )
    assert(result === expectation)
  }

  test("headings") {
    val headingBody = "Overview"
    val subHeadingBody = "Of the heading"

    val result: Seq[DocToken] =
      parseString(
        s"""
        /**
          * =$headingBody=
          * ==$subHeadingBody==
          */
         case class foo(bar : String)
         """
      )
    val expectation = Seq(
      DocToken(Heading, headingBody),
      DocToken(SubHeading, subHeadingBody)
    )

    assert(result === expectation)
  }

  test("label parsing/merging") {
    val testStringToMerge = "Test DocText"
    val scaladoc: String =
      DocToken.tagTokenKinds
        .flatMap(token => Seq(generateTestString(token), testStringToMerge))
        .mkString("/*\n * ", "\n * ", "\n */")

    val codeToParse: String =
      s"""
         |$scaladoc
         |case class Foo(bar: String)
      """.stripMargin

    val parsedScaladoc: Seq[DocToken] = parseString(codeToParse)

    // Inherit doc does not merge
    assert(parsedScaladoc.size === DocToken.tagTokenKinds.size)

    // Inherit doc does not merge
    assert(
      parsedScaladoc
        .filterNot(_.kind == DocToken.InheritDoc)
        .forall(_.body.getOrElse("").endsWith(testStringToMerge))
    )
  }

  test("references") {

    val reference1 = "Scala.some"
    val reference2 = "java.util.Random"

    val codeToParse: String =
      s"""
         |/**
         | * Random description with references [[$reference1]] and [[$reference2]].
         | */
         |case class Foo(bar: String) extends AnyVal
      """.stripMargin

    assert(
      parseString(codeToParse).head.references ===
        Seq(DocToken.Reference(reference1), DocToken.Reference(reference2))
    )
  }
}
