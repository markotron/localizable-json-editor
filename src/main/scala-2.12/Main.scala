
import java.util.UUID

import japgolly.scalajs.react
import japgolly.scalajs.react.{Callback, ReactDOM, ReactEventFromInput}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.raw.Props

import scala.scalajs.js.JSApp
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.document
import tms.component.{JsonBackend, JsonProperties, MainBackend, MainState}
import tms.model._

/**
  * Created by markotron on 11/03/2017.
  */
object Main extends JSApp {

  def sampleJson = {

    val jsonString =
      """
        |{
        |   "ime" : "Marko",
        |   "osoba" : {
        |     "ime" : "kajo"
        |    },
        |   "prezime" : "Bozic",
        |   "adresa" : {
        |     "ulica" : "Set V Gortana",
        |     "broj" : 74
        |   },
        |   "sex" : {
        |     "isMale" : true,
        |     "isFemale" : false,
        |     "realSex" : {
        |       "realrealSex" : "male"
        |     }
        |   }
        |}
      """.stripMargin

    val jsonString1 =
      """
        |{
        |   "vjezbe" : {
        |     "vj1" : {
        |       "naziv" : "disanje",
        |       "trajanje" : 2,
        |       "metadata" : {
        |         "waveParam" : 12,
        |         "boolean" : true,
        |         "config" : 7
        |       }
        |     },
        |     "vj2" : { "naziv" : "inner peace", "trajanje" : 12 },
        |     "vj3" : { "naziv" : "body scan", "trajanje" : 11 },
        |     "vj4" : { "naziv" : "molitva", "trajanje" : 13 },
        |     "vj5" : { "naziv" : "hejtanje", "trajanje" : 15 }
        |   }
        |}
      """.stripMargin

    LocalizableJson(jsonString1)("en")
  }

  override def main(): Unit = {
    val Comp = react.ScalaComponent.build[List[String]]("main")
      .initialState(MainState(List(sampleJson)))
      .renderBackend[MainBackend]
      .build

    Comp(List("en", "de")).renderIntoDOM(document.getElementById("json"))
  }
}
