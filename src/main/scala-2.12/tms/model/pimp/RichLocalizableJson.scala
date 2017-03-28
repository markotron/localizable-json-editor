package tms.model.pimp

import tms.model._

import scala.util.{Failure, Try}

/**
  * Created by markotron on 28/03/2017.
  */
case class RichLocalizableJson(json: LocalizableJson) {

  val S = classOf[LocalizableString]
  val D = classOf[LocalizableDouble]
  val B = classOf[LocalizableBoolean]
  val O = classOf[LocalizableObject]

  def convert(c: Class[_ <: LocalizableJson]): Try[LocalizableJson] = c match {
    case S =>
      Try {
        LocalizableString(json.value.map(p => (p._1, p._2.toString)))
      }
    case D =>
      Try {
        LocalizableDouble(json.value.map(p => (p._1, p._2.toString.toDouble)))
      }
    case B =>
      Try {
        LocalizableBoolean(
          json.value.map(p => (p._1, p._2.toString.toBoolean)))
      }
    case O =>
      Failure(new RuntimeException("Cannot convert LocalizableObjects!"))
  }
}

object RichLocalizableJson {

  implicit def json2RichJson(json: LocalizableJson): RichLocalizableJson =
    new RichLocalizableJson(json)
}
