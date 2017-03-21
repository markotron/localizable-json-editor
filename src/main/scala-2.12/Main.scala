
import java.util.UUID

import japgolly.scalajs.react
import japgolly.scalajs.react.{Callback, ReactDOM, ReactEventFromInput}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.raw.Props

import scala.scalajs.js.JSApp
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.document
import tms.component.{JsonBackend, JsonProperties, MainBackend, MainState}
import tms.model.{LocalizableJson, LocalizableNull, LocalizableObject, LocalizableString}

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

    LocalizableJson(jsonString)("en")
  }

  override def main(): Unit = {
    val Comp = react.ScalaComponent.build[List[String]]("main")
      .initialState(MainState(List(sampleJson)))
      .renderBackend[MainBackend]
      .build

    Comp(List("en" )).renderIntoDOM(document.getElementById("json"))

  }
}
