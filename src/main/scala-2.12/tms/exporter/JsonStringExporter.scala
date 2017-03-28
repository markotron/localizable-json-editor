package tms.exporter

import tms.model._

import scala.scalajs.js

/**
  * Created by markotron on 04/03/2017.
  */
object JsonStringExporter {

  def stringify(json: LocalizableJson, lang: String)(
      filter: (String, LocalizableJson) => Boolean): String = {
    def semiIdentity(v: Any): String = v.toString

    def stringifyLeaf(wrapper: Any => String)(
        value: Map[String, Any]): String =
      value.get(lang) match {
        case Some(s) => wrapper(s)
        case None => "null"
      }

    json match {
      case LocalizableObject(value, _) =>
        val str = value.toList
          .filter(p => filter(p._1, p._2))
          .map(pair => s""""${pair._1}":${stringify(pair._2, lang)(filter)}""")
          .mkString(",")
        s"{$str}"

      case LocalizableString(value, _) =>
        stringifyLeaf(a => s""""$a"""")(value)
      case LocalizableDouble(value, _) => stringifyLeaf(semiIdentity)(value)
      case LocalizableBoolean(value, _) => stringifyLeaf(semiIdentity)(value)
      case LocalizableNull => "null"

    }
  }

  def stringify(json: LocalizableJson, lang: String, space: Int)(
      filter: (String, LocalizableJson) => Boolean): String = {
    val str = stringify(json, lang)(filter)
    val j = js.JSON.parse(str)
    js.JSON.stringify(j, space = space)
  }

}
