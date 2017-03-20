package tms.importer

import scala.scalajs.js
import tms.model._

/**
  * Created by markotron on 03/03/2017.
  */
object JsonImporter {

  val concatChar = "."

  def importJson(json: js.Dynamic)(root: List[String])(
      implicit locale: String): LocalizableJson = {

    def determinNumberType: LocalizableJson = {

      val DoubleRegex = """[-+]?[0-9]*\.?[0-9]*""".r
      val IntRegex = """[-+]?\d+""".r

      json.toString match {
        case IntRegex() =>
          LocalizableInt(Map(locale -> json.asInstanceOf[Int]))
        case DoubleRegex() =>
          LocalizableDouble(
                            Map(locale -> json.asInstanceOf[Double]))
      }

    }

    def createObjectMap(keys: List[String]): Map[String, LocalizableJson] =
      Map[String, LocalizableJson](
        keys
          .map(k => (k, json.selectDynamic(k)))
          .map(p => (p._1, importJson(p._2)(p._1 :: root))): _*
      )

    js.typeOf(json) match {
      case "string" =>
        LocalizableString(
                          Map(locale -> json.asInstanceOf[String]))
      case "boolean" =>
        LocalizableBoolean(
                           Map(locale -> json.asInstanceOf[Boolean]))
      case "number" => determinNumberType
      case "object" => {
        val jsonObject = json.asInstanceOf[js.Object]
        val keys = js.Object.keys(jsonObject).toList
        LocalizableObject(createObjectMap(keys))
      }
    }

  }

  def importJson(json: String)(root: List[String])(implicit locale: String): LocalizableJson = {
    val j = js.JSON.parse(json)
    importJson(j)(root)
  }
}
