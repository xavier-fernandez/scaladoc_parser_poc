/**
  * Represents a token from a line of a scaladoc.
  */
sealed trait DocToken

/**
  * Companion object containing the classes required for describing an ScalaDoc token.
  *
  * The available tokens and their documentation are obtained from:
  * @see http://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html
  */
object DocToken {

  /**
    * Placed in the class comment will describe the primary constructor.
    */
  case class DocConstructor(body: String) extends DocToken

  /**
    * TODO
    *
    * Documents a specific value parameter of a method or class constructor.
    * One @param tag allowed per value parameter in comment for method,
    * constructor or class (documents primary constructor). The
    * <name> argument must correspond to an existing value parameter.
    */
  case class DocParam(name: String, body: String) extends DocToken {
    override def toString: String = s"DocParam(name=$name, body=$body)"
  }

  /**
    *
    * TODO
    *
    *
    * Documents a specific type parameter of a method, class, trait or abstract type.
    * One @tparam tag allowed per type parameter in comment for method, class, trait
    * or abstract type. <name> argument must correspond to an existing type parameter.
    */
  case class DocTypeParam(name: String, body: String) extends DocToken {
    override def toString: String = s"DocTypeParam(name=$name, body=$body)"
  }

  /**
    * Documents the return value of a method.
    */
  case class DocReturn(body: String) extends DocToken

  /**
    * Documents an exception type that may be thrown by a method or class constructor.
    */
  case class DocThrows(name: String, body: String) extends DocToken {
    override def toString: String = s"DocThrows(name=$name, body=$body)"
  }

  /**
    * Points to other sources of information such as external documentation
    * or related entities in the documentation.
    */
  case class DocSee(body: String) extends DocToken

  /**
    * Documents pre- and post-conditions as well as other notable requirements
    * or restrictions.
    */
  case class DocNote(body: String) extends DocToken

  /**
    * Provides example code and related descriptions.
    */
  case class DocExample(body: String) extends DocToken

  /**
    * TODO
    */
  case class DocUseCase(simpleDefinition: String) extends DocToken

  /**
    * Attributes an entity to one author.
    */
  case class DocAuthor(author: String) extends DocToken

  /**
    * The version of the system or API that a class, trait, object or
    * package is part of.
    */
  case class DocVersion(version: String) extends DocToken

  /**
    * The version of the system or API that an entity was first defined in.
    */
  case class DocSince(version: String) extends DocToken

  /**
    * Documents unimplemented features in an entity.
    */
  case class DocTodo(body: String) extends DocToken

  /**
    * Marks an entity as deprecated. The message should
    * describe replacement implementation.
    */
  case class DocDeprecated(body: String) extends DocToken

  /**
    * Like [[DocDeprecated]] but provides advanced warning of
    * planned changes ahead of deprecation.
    */
  case class DocMigration(body: String) extends DocToken

  /**
    * Take comments from a superclass as defaults if comments
    * are not provided locally.
    */
  case object DocInheritDoc extends DocToken

  /**
    * Documents an untagged scaladoc description.
    */
  case class DocText(body: String) extends DocToken

}
