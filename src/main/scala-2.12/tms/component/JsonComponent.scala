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
case class JsonProperties(
    json: LocalizableJson,
    langs: List[String],
    onDelete: (LocalizableJson) => Callback,
    onUpdate: (LocalizableJson, LocalizableJson) => Callback,
    onAdd: (LocalizableJson, String, LocalizableJson) => Callback
) {

  def updateJson(j: LocalizableJson): JsonProperties = this.copy(json = j)
}

class JsonBackend(bs: BackendScope[JsonProperties, Unit]) {

  def render(prop: JsonProperties): VdomElement = {

    prop.json match {

      case LocalizableObject(v, _) =>
        <.div(^.className := "object-div")(
          v.filter { // (key, json)
              case _ => true
            }
            .toVdomArray {
              case (key, json) =>
                JsonWithKeyComponent(
                  JsonWithKeyProperty(key, prop.updateJson(json)))
            }
        )

      case _ =>
        JsonWithKeyComponent(
          JsonWithKeyProperty("[root]", prop)
        )

    }
  }
}

object JsonComponent {

  private val Comp = react.ScalaComponent
    .build[JsonProperties]("json")
    .renderBackend[JsonBackend]
    .build

  def apply(prop: JsonProperties) = Comp(prop)

}
