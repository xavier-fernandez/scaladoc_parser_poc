import DocToken.DocKind

/**
  * Represents a token from a line of a scaladoc.
  */
case class DocToken(kind: DocKind, name: Option[String], body: String) {

  override def toString: String = name match {
    case Some(n) => s"$kind(name=$n, body=$body)"
    case _ => s"$kind($body)"
  }

  // TODO: Encapsulate append method to the contrib package?

  /**
    * Appends a this [[DocToken#body]] to the input [[DocToken#body]].
    */
  def append(docToken: DocToken) : DocToken = copy(body = s"$body\n${docToken.body}")
}

/**
  * Companion object containing the classes required for describing an ScalaDoc token.
  *
  * The available tokens and their documentation are obtained from:
  * @see http://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html
  */
object DocToken {

  /**
    * Helper [[DocToken]] apply method.
    */
  def apply(kind: DocKind, body: String): DocToken =
    new DocToken(kind, None, body)

  /**
    * Helper apply method for named [[DocToken]].
    */
  def apply(kind: DocKind, name: String, body: String): DocToken =
    new DocToken(kind, Option(name), body)

  /**
    * Phantom trait used for representing each type of documentation label.
    */
  sealed trait DocKind

  /**
    * Placed in the class comment will describe the primary constructor.
    */
  case object DocConstructor extends DocKind

  /**
    * Documents a specific value parameter of a method or class constructor.
    */
  case object DocParam extends DocKind

  /**
    * Documents a specific type parameter of a method, class, trait or abstract type.
    */
  case object DocTypeParam extends DocKind
  /**
    * Documents the return value of a method.
    */
  case object DocReturn extends DocKind

  /**
    * Documents an exception type that may be thrown by a method or class constructor.
    */
  case object DocThrows extends DocKind

  /**
    * Points to other sources of information such as external documentation
    * or related entities in the documentation.
    */
  case object DocSee extends DocKind

  /**
    * Documents pre- and post-conditions as well as other notable requirements
    * or restrictions.
    */
  case object DocNote extends DocKind

  /**
    * Provides example code and related descriptions.
    */
  case object DocExample extends DocKind

  /**
    * Documents a use case of a method, class, trait or abstract type.
    */
  case object DocUseCase extends DocKind

  /**
    * Attributes an entity to one author.
    */
  case object DocAuthor extends DocKind

  /**
    * The version of the system or API that a class, trait, object or
    * package is part of.
    */
  case object DocVersion extends DocKind

  /**
    * The version of the system or API that an entity was first defined in.
    */
  case object DocSince extends DocKind

  /**
    * Documents unimplemented features in an entity.
    */
  case object DocTodo extends DocKind

  /**
    * Marks an entity as deprecated. The message should
    * describe replacement implementation.
    */
  case object DocDeprecated extends DocKind

  /**
    * Like [[DocDeprecated]] but provides advanced warning of
    * planned changes ahead of deprecation.
    */
  case object DocMigration extends DocKind

  /**
    * Take comments from a superclass as defaults if comments
    * are not provided locally.
    */
  case object DocInheritDoc extends DocKind

  /**
    * Documents an untagged scaladoc description.
    */
  case object DocText extends DocKind

}
