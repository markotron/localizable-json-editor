package tms.component

import japgolly.scalajs.react._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.{BackendScope, Unmounted}
import japgolly.scalajs.react.vdom.html_<^._
import tms.model.{LocalizableJson, LocalizableString}

/**
  * Created by markotron on 20/03/2017.
  */
case class JsonLeafProperties(json: LocalizableJson,
                              langs: List[String],
                              onUpdate: (LocalizableJson, LocalizableJson) => Callback)

case class JsonLeafState(content: Map[String, String], changed: Map[String, Boolean]) {
  def updateContent(lang: String, text: String) = JsonLeafState(content.updated(lang, text), changed)

  def updateChanged(lang: String, state: Boolean) = JsonLeafState(content, changed.updated(lang, state))
}

class JsonLeafBackend(val bs: BackendScope[JsonLeafProperties, JsonLeafState]) {

  def didChange(lang: String, text: String, prop: JsonLeafProperties) = {
    val persistedText = prop.json.value.getOrElse(lang, "").toString
    if (text == persistedText) false else true
  }

  def onChange(prop: JsonLeafProperties)(event: ReactEventFromInput): Callback = {
    val lang = event.target.name
    val text = event.target.value
    bs.modState { s =>
      val newState = if (didChange(lang, text, prop)) s.updateChanged(lang, true) else s.updateChanged(lang, false)
      newState.updateContent(lang, text)
    }
  }

  def onKeyUp(prop: JsonLeafProperties, state: JsonLeafState)(event: ReactKeyboardEventFromInput): Callback = {

    def createJsonLeaf = {
      // TODO should parse the string and check the type
      LocalizableString(state.content)
    }

    val lang = event.target.name
    val text = event.target.value.trim
    if (event.keyCode == 13 && didChange(lang, text, prop))
      prop.onUpdate(prop.json, createJsonLeaf)
    else Callback.empty
  }


  def render(prop: JsonLeafProperties, state: JsonLeafState) = {
    <.div(
      ^.className := "leaf-div")(
      prop.langs.map(l =>
        <.div(
          <.label(l + ": "),
          <.input(
            ^.classSet(
              "text-input" -> true,
              "text-edited-input" -> state.changed.getOrElse(l, false)),
            ^.name := l,
            ^.`type` := "text",
            ^.value := state.content.getOrElse(l, ""),
            ^.onChange ==> onChange(prop),
            ^.onKeyUp ==> onKeyUp(prop, state))
        )
      ): _*
    )
  }

}

object JsonLeafComponent {

  val Comp = ScalaComponent
    .build[JsonLeafProperties]("json-leaf")
    .initialState(JsonLeafState(Map(), Map()))
    .renderBackend[JsonLeafBackend]
    .componentDidMount { f =>
      val props = f.props
      val strMap = props.json.value.map { case (k, v) => (k, v.toString) }
      f.backend.bs.modState(s => JsonLeafState(strMap, Map()))
    }
    .build

  def apply(prop: JsonLeafProperties) = Comp(prop)
}
