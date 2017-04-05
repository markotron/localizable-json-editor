package tms.component

import japgolly.scalajs.react
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import tms.model._

/**
  * Created by markotron on 19/03/2017.
  */
case class JsonWithKeyProperty(key: String, jsonProp: JsonProperties)

case class JsonWithKeyState(currentType: Class[_ <: LocalizableJson],
                            potentialTypes: List[Class[_ <: LocalizableJson]],
                            addingNew: Boolean,
                            isExpanded: Boolean)

class JsonWithKeyBackend(
    bs: BackendScope[JsonWithKeyProperty, JsonWithKeyState]) {

  import tms.model.pimp.RichLocalizableJson

  def changeCurrentType(`type`: Class[_ <: LocalizableJson]): Callback =
    bs.modState(s => s.copy(currentType = `type`))

  private def getTypeTag(state: JsonWithKeyState)(
      `type`: Class[_ <: LocalizableJson]) = {

    `type` match {
      case RichLocalizableJson.S =>
        <.span(
          ^.classSet(
            "type-tag-span" -> true,
            "type-tag-span-selected" -> state.currentType.equals(`type`)),
          ^.onClick --> changeCurrentType(RichLocalizableJson.S),
          "string"
        )
      case RichLocalizableJson.D =>
        <.span(
          ^.classSet(
            "type-tag-span" -> true,
            "type-tag-span-selected" -> state.currentType.equals(`type`)),
          ^.onClick --> changeCurrentType(RichLocalizableJson.D),
          "number"
        )
      case RichLocalizableJson.B =>
        <.span(
          ^.classSet(
            "type-tag-span" -> true,
            "type-tag-span-selected" -> state.currentType.equals(`type`)),
          ^.onClick --> changeCurrentType(RichLocalizableJson.B),
          "boolean"
        )
      case RichLocalizableJson.O =>
        <.span(
          ^.classSet(
            "type-tag-span" -> true,
            "type-tag-span-selected" -> state.currentType.equals(`type`)),
          ^.onClick --> changeCurrentType(RichLocalizableJson.O),
          "object"
        )
    }
  }

  def toggleHidden =
    bs.modState(
      s =>
        JsonWithKeyState(s.currentType,
                         s.potentialTypes,
                         s.addingNew,
                         !s.isExpanded))

  def toggleAddNewForm =
    bs.modState(s => s.copy(addingNew = !s.addingNew))

  def render(prop: JsonWithKeyProperty, state: JsonWithKeyState): VdomElement = {

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
              case RichLocalizableJson.O => false
              case _ => true
            }
            .map(getTypeTag(state)): _*),
        <.span(^.className := "buttons-span")(
          <.i(^.className := "fa fa-trash",
              ^.onClick --> jsonProp.onDelete(json)),
          json match {
            case _ : LocalizableObject =>
              <.i(^.classSet(
                    "fa fa-plus" -> !state.addingNew,
                    "fa fa-minus" -> state.addingNew
                  ),
                  ^.onClick --> toggleAddNewForm)
            case _ => <.i
          }
        )
      ),
      JsonAddComponent(
        JsonAddProperties(json,
                          jsonProp.onAdd,
                          toggleAddNewForm,
                          !state.addingNew)),
      <.div(^.classSet("hidden" -> !state.isExpanded))(json match {
        case _: LocalizableObject => JsonComponent(jsonProp)
        case _ =>
          JsonLeafComponent(
            JsonLeafProperties(
              json,
              state.currentType,
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
    .initialState(
      JsonWithKeyState(classOf[LocalizableObject],
                       List(classOf[LocalizableObject]),
                       false,
                       true))
    .renderBackend[JsonWithKeyBackend]
    .componentWillMount { f =>
      val json = f.props.jsonProp.json
      f.modState(
        s =>
          JsonWithKeyState(json.getClass,
                           s.potentialTypes,
                           s.addingNew,
                           s.isExpanded))
    }
    .build

  def apply(prop: JsonWithKeyProperty) =
    Comp.withKey(prop.jsonProp.json.id)(prop)
}
