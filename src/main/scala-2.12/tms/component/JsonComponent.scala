package tms.component

import japgolly.scalajs.react
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import tms.model.{LocalizableJson, LocalizableObject}

/**
  * Created by markotron on 19/03/2017.
  */

case class JsonProperties(json: LocalizableJson,
                          langs: List[String],
                          onDelete: (LocalizableJson) => Callback) {

  def updateJson(j: LocalizableJson) = this.copy(json = j)
}

case class JsonState(isExpanded: Boolean)

class JsonBackend(bs: BackendScope[JsonProperties, JsonState]) {

//  val JsonComp = react.ScalaComponent
//    .build[JsonProperties]("Json component")
//    .initialState(JsonState(true))
//    .renderBackend[JsonBackend]
//    .build


  def toggleHidden =
    bs.modState(s =>
      JsonState(!s.isExpanded))

  def render(prop: JsonProperties, state: JsonState): VdomElement = {

    prop.json match {

      case LocalizableObject(v, _) =>
        <.span(
          ^.className := "modify-buttons-span")(
          <.div(
            ^.className := "object-div"
          )(
            v.toVdomArray {
              case (key, json) =>
                JsonWithKeyComponent(JsonWithKeyProperty(key, prop.updateJson(json)))
            }
          )
        )

      case _ =>
        <.span(
          ^.className := "modify-buttons-span")(
          <.div(
            ^.className := "leaf-div")(
            prop.langs.map(l =>
              <.div(
                <.label(l + ": "),
                <.input(^.`type` := "text", ^.value := prop.json.value.getOrElse(l, "").toString))
            ): _*
          )
        )

    }
  }


}

object JsonComponent {

  val Comp = react.ScalaComponent
    .build[JsonProperties]("json")
    .initialState(JsonState(true))
    .renderBackend[JsonBackend]
    .build

  def apply(prop: JsonProperties) = Comp(prop)

}
