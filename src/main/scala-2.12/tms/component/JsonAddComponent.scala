package tms.component

import japgolly.scalajs.react
import japgolly.scalajs.react._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import tms.model.pimp.RichLocalizableJson
import tms.model._

/**
  * Created by markotron on 05/04/2017.
  */
case class JsonAddProperties(
    json: LocalizableJson,
    onAdd: (LocalizableJson, String, LocalizableJson) => Callback,
    hide: Callback,
    isHidden: Boolean
)

case class JsonAddState(
    key: String,
    selectedType: Class[_ <: LocalizableJson]
)

class JsonAddBackend(bs: BackendScope[JsonAddProperties, JsonAddState]) {

  def createObject: CallbackTo[_ <: LocalizableJson] = {
    for (state <- bs.state)
      yield
        state.selectedType match {
          case RichLocalizableJson.O => LocalizableObject(Map())
          case RichLocalizableJson.S => LocalizableString(Map())
          case RichLocalizableJson.D => LocalizableDouble(Map())
          case RichLocalizableJson.B => LocalizableBoolean(Map())
        }
  }

  def onChange(event: ReactEventFromInput): Callback = {
    val text = event.target.value
    bs.modState(s => s.copy(key = text))
  }

  private def clearAndAddWithKey(prop: JsonAddProperties, key: String) =
    prop.hide >> (createObject >>= (prop.onAdd(prop.json, key, _))) >> bs
      .setState(JsonAddState("", RichLocalizableJson.O))

  def onKeyUp(prop: JsonAddProperties)(
      event: ReactKeyboardEventFromInput): Callback = {
    val text = event.target.value
    if (event.keyCode == 13 && text.trim != "")
      clearAndAddWithKey(prop, text)
    else
      Callback.empty
  }

  def changeCurrentType(prop: JsonAddProperties, state: JsonAddState)(
      selectedType: Class[_ <: LocalizableJson]): Callback =
    bs.modState(
      s => s.copy(selectedType = selectedType),
      if (state.key.trim == "") Callback.empty
      else clearAndAddWithKey(prop, state.key)
    )

  def render(prop: JsonAddProperties, state: JsonAddState): VdomElement = {
    <.div(^.classSet("add-new-div" -> true, "hidden" -> prop.isHidden))(
      <.label("key: "),
      <.input(^.className := "text-input",
              ^.`type` := "text",
              ^.value := state.key,
              ^.onChange ==> onChange,
              ^.onKeyUp ==> onKeyUp(prop)),
      <.span(
        ^.classSet(
          "type-tag-span" -> true,
          "type-tag-span-selected" -> state.selectedType.equals(
            RichLocalizableJson.O)
        ),
        ^.onClick --> changeCurrentType(prop, state)(RichLocalizableJson.O),
        "object"
      ),
      <.span(
        ^.classSet(
          "type-tag-span" -> true,
          "type-tag-span-selected" -> state.selectedType.equals(
            RichLocalizableJson.S)
        ),
        ^.onClick --> changeCurrentType(prop, state)(RichLocalizableJson.S),
        "string"
      ),
      <.span(
        ^.classSet(
          "type-tag-span" -> true,
          "type-tag-span-selected" -> state.selectedType.equals(
            RichLocalizableJson.D)
        ),
        ^.onClick --> changeCurrentType(prop, state)(RichLocalizableJson.D),
        "number"
      ),
      <.span(
        ^.classSet(
          "type-tag-span" -> true,
          "type-tag-span-selected" -> state.selectedType.equals(
            RichLocalizableJson.B)
        ),
        ^.onClick --> changeCurrentType(prop, state)(RichLocalizableJson.B),
        "boolean"
      )
    )
  }

}

object JsonAddComponent {
  val Comp = react.ScalaComponent
    .build[JsonAddProperties]("json-add")
    .initialState(JsonAddState("", classOf[LocalizableObject]))
    .renderBackend[JsonAddBackend]
    .build

  def apply(prop: JsonAddProperties) = Comp(prop)

}
