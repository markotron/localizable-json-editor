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

case class JsonLeafState(content: Map[String, String]) {
  def updateLang(lang: String, text: String) = JsonLeafState(content.updated(lang, text))
}

class JsonLeafBackend(val bs: BackendScope[JsonLeafProperties, JsonLeafState]) {


  def onChange(event: ReactEventFromInput): Callback = {
    val lang = event.target.name
    val text = event.target.value
    bs.modState(s => s.updateLang(lang, text))
  }

  def onKeyUp(prop: JsonLeafProperties, state: JsonLeafState)(event: ReactKeyboardEventFromInput): Callback = {

    def createJsonLeaf = {
      // TODO should parse the string and check the type
      LocalizableString(state.content)
    }

    def didChange = {
      val lang = event.target.name
      val text = event.target.value.trim

      val persistedText = prop.json.value.getOrElse(lang, "").toString.trim
      if (text == persistedText) false else true
    }

    if (event.keyCode == 13 && didChange)
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
            ^.name := l,
            ^.`type` := "text",
            ^.value := state.content.getOrElse(l, ""),
            ^.onChange ==> onChange,
            ^.onKeyUp ==> onKeyUp(prop, state))
        )
      ): _*
    )
  }

}

object JsonLeafComponent {

  val Comp = ScalaComponent
    .build[JsonLeafProperties]("json-leaf")
    .initialState(JsonLeafState(Map()))
    .renderBackend[JsonLeafBackend]
    .componentDidMount { f =>
      val props = f.props
      val strMap = props.json.value.map { case (k, v) => (k, v.toString) }
      f.backend.bs.modState(s => JsonLeafState(strMap))
    }
    .build

  def apply(prop: JsonLeafProperties) = Comp(prop)
}
