package tms.model.pimp

import tms.model._

import scala.util.{Failure, Try}

/**
  * Created by markotron on 28/03/2017.
  */
case class RichLocalizableJson(json: LocalizableJson) {

  def convert(c: Class[_ <: LocalizableJson]): Try[LocalizableJson] = c match {
    case RichLocalizableJson.S =>
      Try {
        LocalizableString(json.value.map(p => (p._1, p._2.toString)))
      }
    case RichLocalizableJson.D =>
      Try {
        LocalizableDouble(json.value.map(p => (p._1, p._2.toString.toDouble)))
      }
    case RichLocalizableJson.B =>
      Try {
        LocalizableBoolean(
          json.value.map(p => (p._1, p._2.toString.toBoolean)))
      }
    case RichLocalizableJson.O =>
      Failure(new RuntimeException("Cannot convert LocalizableObjects!"))
  }
}

object RichLocalizableJson {

  val S = classOf[LocalizableString]
  val D = classOf[LocalizableDouble]
  val B = classOf[LocalizableBoolean]
  val O = classOf[LocalizableObject]

  implicit def json2RichJson(json: LocalizableJson): RichLocalizableJson =
    new RichLocalizableJson(json)
}
