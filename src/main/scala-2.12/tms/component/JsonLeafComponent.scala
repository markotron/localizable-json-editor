package tms.component

import japgolly.scalajs.react._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import tms.model._

import tms.model.pimp.RichLocalizableJson._

/**
  * Created by markotron on 20/03/2017.
  */
case class JsonLeafProperties(
    json: LocalizableJson,
    jsonType: Class[_ <: LocalizableJson],
    langs: List[String],
    onUpdate: (LocalizableJson, LocalizableJson) => Callback,
    setPotentialTypes: (List[Class[_ <: LocalizableJson]]) => Callback)

case class JsonLeafStateValue(content: String, touched: Boolean)

case class JsonLeafState(langState: Map[String, JsonLeafStateValue]) {

  def isTouched: Boolean = langState.map(_._2.touched).reduce(_ || _)

  def updateContent(prop: JsonLeafProperties)(lang: String, text: String) =
    langState
      .get(lang)
      .map(_.copy(content = text))
      .map(v => JsonLeafState(langState.updated(lang, v)))
      .getOrElse(JsonLeafState(langState
        .updated(lang, JsonLeafStateValue(text, false))))

  def updateTouched(prop: JsonLeafProperties)(lang: String,
                                              isTouched: Boolean) =
    langState
      .get(lang)
      .map(_.copy(touched = isTouched))
      .map(v => JsonLeafState(langState.updated(lang, v)))
      .getOrElse(JsonLeafState(
        langState.updated(lang, JsonLeafStateValue("", isTouched))))

  def getContentOrElse(key: String, els: String) =
    langState.get(key).map(_.content).getOrElse(els)

  def getTouchedOrElse(key: String, els: Boolean) =
    langState.get(key).map(_.touched).getOrElse(els)

  def getPotentialTypes = {
    val NumberRegex = """[-+]?[0-9]*\.?[0-9]*""".r
    val BooleanRegex = """(true|false|0|1)?""".r
    val StringRegex = ".*".r

    val regexs =
      Map(NumberRegex -> classOf[LocalizableDouble],
          BooleanRegex -> classOf[LocalizableBoolean],
          StringRegex -> classOf[LocalizableString])

    langState
      .map(_._2.content) // list of texts in different languages
      .map((_, regexs)) // pairs (text, List(NumberRegex...)
      .map {
        case (t, rs) =>
          rs.filter(r => r._1.pattern.matcher(t).matches).map(r => r._2).toSet
      } // list of sets of classes <: LocalizableJson
      .reduce((s1, s2) => s1.intersect(s2))
      .toList
  }
}

class JsonLeafBackend(val bs: BackendScope[JsonLeafProperties, JsonLeafState]) {

  def didChange(lang: String, text: String, prop: JsonLeafProperties) = {
    val persistedText = prop.json.value.getOrElse(lang, "").toString
    if (text == persistedText) false else true
  }

  def onChange(prop: JsonLeafProperties)(
      event: ReactEventFromInput): Callback = {
    val lang = event.target.name
    val text = event.target.value
    bs.modState { s =>
      prop.setPotentialTypes(s.getPotentialTypes)
      val newState =
        if (didChange(lang, text, prop)) s.updateTouched(prop)(lang, true)
        else s.updateTouched(prop)(lang, false)
      newState.updateContent(prop)(lang, text)
    }
  }

  def onKeyUp(prop: JsonLeafProperties, state: JsonLeafState)(
      event: ReactKeyboardEventFromInput): Callback = {

    def createJsonLeaf = {
      val strRepr = LocalizableString(state.langState.map {
        case (k, v) => (k, v.content)
      })
      strRepr.convert(prop.jsonType)
    }
    val lang = event.target.name
    val text = event.target.value.trim
    if (event.keyCode == 13 && didChange(lang, text, prop))
      createJsonLeaf
        .map { c =>
          prop.onUpdate(prop.json, c)
        }
        .getOrElse(Callback.empty)
    else
      prop.setPotentialTypes(state.getPotentialTypes)

  }

  def render(prop: JsonLeafProperties, state: JsonLeafState) = {
    <.div(^.className := "leaf-div")(
      prop.langs.map(
        l =>
          <.div(
            <.label(l + ": "),
            <.input(
              ^.classSet("text-input" -> true,
                         "text-edited-input" -> state.getTouchedOrElse(l,
                                                                       false)),
              ^.name := l,
              ^.`type` := "text",
              ^.value := state.getContentOrElse(l, ""),
              ^.onChange ==> onChange(prop),
              ^.onKeyUp ==> onKeyUp(prop, state)
            )
        )): _*
    )
  }

}

object JsonLeafComponent {

  val Comp = ScalaComponent
    .build[JsonLeafProperties]("json-leaf")
    .initialState(JsonLeafState(Map()))
    .renderBackend[JsonLeafBackend]
    .componentWillMount { f =>
      val props = f.props
      val strMap = props.json.value.map {
        case (k, v) =>
          (k, JsonLeafStateValue(v.toString, false))
      }

      // NOTE: this does not work
      // f.backend.bs.setState(...) >> f.backend.bs.state >>= (s ...)
      // the set state block is executet somewhere else and
      // concatenating it to the return Callback makes no sense
      f.backend.bs.setState(JsonLeafState(strMap),
                            f.backend.bs.state >>= (s =>
                              f.props.setPotentialTypes(s.getPotentialTypes)))
    }
    .componentWillUpdate { f =>
      if (f.nextProps.jsonType == f.nextProps.json.getClass) Callback.empty
      else if (f.nextState.isTouched) Callback.empty
      else {
        f.nextProps.json
          .convert(f.nextProps.jsonType)
          .map { c =>
            f.nextProps.onUpdate(f.nextProps.json, c)
          }
          .getOrElse(Callback.empty)
      }
    }
    .build

  def apply(prop: JsonLeafProperties) = Comp(prop)
}
