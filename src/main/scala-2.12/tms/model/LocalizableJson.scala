package tms.model

import java.util.{NoSuchElementException, UUID}

import tms.importer.JsonImporter
import tms.exporter.JsonStringExporter

import scala.scalajs.js
/**
  * Immutable rich json structure. Rich means that has it has native
  * support for many tipes that json hasn't (not right now :):
  *  - Urls // TODO
  *  - Colors // TODO
  *  - Arrays // TODO
  *
  * Created by markotron on 03/03/2017.
  */
sealed trait LocalizableJson {

  type Path = List[String]

  val id: String

  val value: Map[String, Any]

  def getPath(el: LocalizableJson): Option[Path]

  /**
    * Gets an element if exists
    *
    * @param path to the element
    * @return Option -- if the element doesn't exist it will return <code>None</code>
    */
  def getElement(path: Path): Option[LocalizableJson]

  def getElement(el: LocalizableJson): Option[LocalizableJson]

  /**
    * Insert or updates an element on a given path. It will throw an
    * exception if the type of a json must be changed. For example:
    * {
    * "name" : "Marko",
    * "age" : 22
    * }
    * In the above json legal insert paths are: "address", "address.city.zipCode"
    * and illegal are: "age.juvenile" because the operation changes the type of
    * the "age" json from 'string' to 'object'
    *
    * @param path path to upsert
    * @param el   element to upsert
    * @return new updated json
    */
  def upsertElement(path: Path, el: LocalizableJson): LocalizableJson

  def upsertElement(root: LocalizableJson,
                    el: LocalizableJson,
                    maybeKey: Option[String] = None): LocalizableJson

  /**
    * Deletes the element on a given path. If the path does not exists the
    * structure is unchanged.
    *
    * @param path to delete
    * @return new updated json
    */
  def deleteElement(path: Path): LocalizableJson

  def deleteElement(el: LocalizableJson): LocalizableJson
}

object LocalizableJson {
  def stringify(json: LocalizableJson, lang: String)(
      filter: (String, LocalizableJson) => Boolean): String =
    JsonStringExporter.stringify(json, lang)(filter)

  def stringify(json: LocalizableJson, lang: String, space: Int)(
      filter: (String, LocalizableJson) => Boolean): String =
    JsonStringExporter.stringify(json, lang, space)(filter)

  def prettify(json: LocalizableJson, lang: String): String =
    stringify(json, lang, 4)((_, _) => true)

  def apply(json: js.Dynamic)(implicit locale: String): LocalizableJson =
    JsonImporter.importJson(json)(List())

  def apply(json: String)(implicit locale: String): LocalizableJson =
    JsonImporter.importJson(json)(List())
}

/**
  * A node in a json tree which is not a leaf.
  *
  * @param value underlying structure
  * @param id    object id -- if you want to manually set the id you can!
  */
case class LocalizableObject(value: Map[String, LocalizableJson],
                             id: String = UUID.randomUUID().toString)
    extends LocalizableJson {

  /**
    * If the key does not exist in the current structre adds the <code>(key, e)</code>!
    * If the key exists and <code>e</code> is not the same object as the existing one, returns a new updated object.
    * If the key exists and <code>e</code> is the same object as the existing one, returns this
    *
    * @param key in a map <code>value</code>
    * @param e   LocalizableJson element which should be updated
    * @return
    */
  private def createUpdatedObject(key: String,
                                  e: LocalizableJson): LocalizableJson = {

    val alreadyExists = value.get(key).exists(json => json.eq(e))
    if (alreadyExists) this
    else
      e match {
        case LocalizableNull => this.copy(value = value - key)
        case _ => this.copy(value = value.updated(key, e))
      }
  }

  def getPath(el: LocalizableJson): Option[Path] = {

    def getPathAndAddKey(entry: (String, LocalizableJson)): Option[Path] =
      entry._2.getPath(el) match {
        case None => None
        case Some(path) => Some(entry._1 :: path)
      }

    def operation(maybePath: Option[Path], entry: (String, LocalizableJson)) =
      maybePath match {
        case s: Some[Path] => s
        case None => getPathAndAddKey(entry)
      }

    def findInList(vs: List[(String, LocalizableJson)]): Option[Path] =
      vs.foldLeft[Option[Path]](None)(operation)

    if (this eq el) Some(Nil)
    else
      this match {
        case LocalizableObject(v, _) => findInList(v.toList)
        case _ => None
      }

  }

  override def getElement(path: Path): Option[LocalizableJson] =
    path match {
      case Nil => Some(this)
      case key :: rest => value.get(key).flatMap(json => json.getElement(rest))
    }

  override def getElement(el: LocalizableJson): Option[LocalizableJson] =
    getPath(el).flatMap(getElement(_))

  override def upsertElement(path: Path,
                             el: LocalizableJson): LocalizableJson = {

    def getOrCreateObject(key: String) = {
      value.getOrElse(key, LocalizableObject(Map()))
    }

    path match {
      case Nil => el
      case key :: rest =>
        createUpdatedObject(key,
                            getOrCreateObject(key).upsertElement(rest, el))
    }
  }

  override def upsertElement(root: LocalizableJson,
                             el: LocalizableJson,
                             maybeKey: Option[String] = None) =
    getPath(root)
      .map(_ ::: maybeKey.map(List(_)).getOrElse(Nil))
      .map(upsertElement(_, el))
      .getOrElse(throw new NoSuchElementException)

  override def deleteElement(path: Path): LocalizableJson =
    path match {
      case Nil => LocalizableNull
      case key :: rest =>
        value
          .get(key)
          .map(json => createUpdatedObject(key, json.deleteElement(rest)))
          .getOrElse(this)
    }

  override def deleteElement(el: LocalizableJson) =
    getPath(el)
      .map(deleteElement(_))
      .getOrElse(throw new NoSuchElementException)
}

/**
  * This abstract class implements the basic get/upsert/delete
  * functions on a leaf node
  */
sealed abstract class LocalizableLeaf extends LocalizableJson {

  override def getPath(el: LocalizableJson) =
    if (this eq el) Some(List()) else None

  override def getElement(path: Path): Option[LocalizableJson] = {
    path match {
      case Nil => Some(this)
      case _ => None
    }
  }

  override def getElement(el: LocalizableJson) =
    if (this eq el) Some(this) else None

  override def upsertElement(path: Path,
                             el: LocalizableJson): LocalizableJson = {
    path match {
      case Nil => el
      case _ => throw new Exception(s"[$this] You'are messing with types!")
    }
  }

  override def upsertElement(root: LocalizableJson,
                             el: LocalizableJson,
                             maybeKey: Option[String] = None) =
    if (maybeKey.isDefined)
      throw new Exception(s"[$this] You'are messing with types!")
    else if (this eq root) el
    else throw new NoSuchElementException

  override def deleteElement(path: Path): LocalizableJson = {
    path match {
      case Nil => LocalizableNull
      case _ => this
    }
  }

  override def deleteElement(el: LocalizableJson) =
    if (this eq el) LocalizableNull
    else this
}

/*
        LEAF NODE LIST
 */
case class LocalizableString(value: Map[String, String],
                             id: String = UUID.randomUUID().toString)
    extends LocalizableLeaf

case class LocalizableInt(value: Map[String, Int],
                          id: String = UUID.randomUUID().toString)
    extends LocalizableLeaf

case class LocalizableDouble(value: Map[String, Double],
                             id: String = UUID.randomUUID().toString)
    extends LocalizableLeaf

case class LocalizableBoolean(value: Map[String, Boolean],
                              id: String = UUID.randomUUID().toString)
    extends LocalizableLeaf

case object LocalizableNull extends LocalizableLeaf {
  val id = "null"
  val value: Map[String, Nothing] = Map()
}
