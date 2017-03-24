package tms.component

import japgolly.scalajs.react
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import tms.model._

/**
  * Created by markotron on 19/03/2017.
  */
case class JsonWithKeyProperty(key: String, jsonProp: JsonProperties)

case class JsonWithKeyState(potentialTypes: List[Class[_ <: LocalizableJson]],
                            isExpanded: Boolean)

class JsonWithKeyBackend(
    bs: BackendScope[JsonWithKeyProperty, JsonWithKeyState]) {

  val S = classOf[LocalizableString]
  val I = classOf[LocalizableInt]
  val D = classOf[LocalizableDouble]
  val B = classOf[LocalizableBoolean]
  val O = classOf[LocalizableObject]

  private def getTypeTag(`type`: Class[_ <: LocalizableJson]) = {

    `type` match {
      case S => <.span(^.className := "type-tag-span", "string")
      case I => <.span(^.className := "type-tag-span", "number")
      case D => <.span(^.className := "type-tag-span", "number")
      case B => <.span(^.className := "type-tag-span", "boolean")
      case O => <.span(^.className := "type-tag-span", "object")
    }
  }

  def toggleHidden =
    bs.modState(s => JsonWithKeyState(s.potentialTypes, !s.isExpanded))

  def render(prop: JsonWithKeyProperty, state: JsonWithKeyState) = {

    val jsonProp = prop.jsonProp
    val json = jsonProp.json

    <.div(
      ^.className := "json-with-key-div",
      <.label(
        <.i(^.classSet(
              "fa fa-minus-square" -> state.isExpanded,
              "fa fa-plus-square" -> !state.isExpanded
            ),
            ^.onClick --> toggleHidden),
        <.span(^.className := "key-span", prop.key)(
          state.potentialTypes
            .filter {
              case O => false
              case _ => true
            }
            .map(getTypeTag): _*),
        <.i(^.className := "fa fa-trash",
            ^.onClick --> jsonProp.onDelete(json))
      ),
      <.div(^.classSet("hidden" -> !state.isExpanded))(json match {
        case _: LocalizableObject => JsonComponent(jsonProp)
        case _ =>
          JsonLeafComponent(
            JsonLeafProperties(
              json,
              jsonProp.langs,
              jsonProp.onUpdate,
              types => bs.modState(s => s.copy(potentialTypes = types))))
      })
    )
  }
}

object JsonWithKeyComponent {
  val Comp = react.ScalaComponent
    .build[JsonWithKeyProperty]("json-with-key")
    .initialState(JsonWithKeyState(List(classOf[LocalizableObject]), true))
    .renderBackend[JsonWithKeyBackend]
    .build

  def apply(prop: JsonWithKeyProperty) =
    Comp.withKey(prop.jsonProp.json.id)(prop)
}
