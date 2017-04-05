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

  def onKeyUp(event: ReactKeyboardEventFromInput): Callback = {
    val text = event.target.value
    if (event.keyCode == 13 && text.trim != "")
      (bs.props >>= ((p: JsonAddProperties) =>
        p.hide >> createObject >>= (p.onAdd(p.json, text, _)))) >>
        bs.setState(JsonAddState("", RichLocalizableJson.O))
    else
      Callback.empty
  }

  def changeCurrentType(prop: JsonAddProperties)(
      selectedType: Class[_ <: LocalizableJson]): Callback =
    bs.modState(
      s => s.copy(selectedType = selectedType),
      bs.state >>= (
          s =>
            if (s.key.trim == "") Callback.empty
            else
              (prop.hide >> createObject >>= (prop
                .onAdd(prop.json, s.key, _))) >> bs.setState(
                JsonAddState("", RichLocalizableJson.O)))
    )

  def render(prop: JsonAddProperties, state: JsonAddState): VdomElement = {
    <.div(^.classSet("add-new-div" -> true, "hidden" -> prop.isHidden))(
      <.label("key: "),
      <.input(^.className := "text-input",
              ^.`type` := "text",
              ^.value := state.key,
              ^.onChange ==> onChange,
              ^.onKeyUp ==> onKeyUp),
      <.span(
        ^.classSet(
          "type-tag-span" -> true,
          "type-tag-span-selected" -> state.selectedType.equals(
            RichLocalizableJson.O)
        ),
        ^.onClick --> changeCurrentType(prop)(RichLocalizableJson.O),
        "object"
      ),
      <.span(
        ^.classSet(
          "type-tag-span" -> true,
          "type-tag-span-selected" -> state.selectedType.equals(
            RichLocalizableJson.S)
        ),
        ^.onClick --> changeCurrentType(prop)(RichLocalizableJson.S),
        "string"
      ),
      <.span(
        ^.classSet(
          "type-tag-span" -> true,
          "type-tag-span-selected" -> state.selectedType.equals(
            RichLocalizableJson.D)
        ),
        ^.onClick --> changeCurrentType(prop)(RichLocalizableJson.D),
        "number"
      ),
      <.span(
        ^.classSet(
          "type-tag-span" -> true,
          "type-tag-span-selected" -> state.selectedType.equals(
            RichLocalizableJson.B)
        ),
        ^.onClick --> changeCurrentType(prop)(RichLocalizableJson.B),
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
