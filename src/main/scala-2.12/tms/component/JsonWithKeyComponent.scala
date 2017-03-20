package tms.component

import japgolly.scalajs.react
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._

/**
  * Created by markotron on 19/03/2017.
  */

case class JsonWithKeyProperty(key: String, jsonProp: JsonProperties)

case class JsonWithKeyState(isExpanded: Boolean)

class JsonWithKeyBackend(bs: BackendScope[JsonWithKeyProperty, JsonWithKeyState]) {

  def toggleHidden =
    bs.modState(s => JsonWithKeyState(!s.isExpanded))

  def render(prop: JsonWithKeyProperty, state: JsonWithKeyState) = {

    val jsonProp = prop.jsonProp
    val json = jsonProp.json

    <.div(
      ^.className := "json-with-key-div",
      <.label(<.i(
        ^.classSet(
          "fa fa-minus-square" -> state.isExpanded,
          "fa fa-plus-square" -> !state.isExpanded
        ),
        ^.onClick --> toggleHidden),
        <.span(^.className := "key-span", prop.key),
        <.i(^.className := "fa fa-trash", ^.onClick --> jsonProp.onDelete(json))
      ),
      <.div(^.classSet("hidden" -> !state.isExpanded))(JsonComponent(jsonProp))
    )
  }
}

object JsonWithKeyComponent {
  val Comp = react.ScalaComponent
    .build[JsonWithKeyProperty]("json-with-key")
    .initialState(JsonWithKeyState(true))
    .renderBackend[JsonWithKeyBackend]
    .build

  def apply(prop: JsonWithKeyProperty)
    = Comp.withKey(prop.jsonProp.json.id)(prop)
}