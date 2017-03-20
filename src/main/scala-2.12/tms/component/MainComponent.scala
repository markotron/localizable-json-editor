package tms.component

import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import tms.model.LocalizableJson

/**
  * Created by markotron on 19/03/2017.
  */
class MainBackend(bs: BackendScope[Unit, LocalizableJson]) {

//  val Json = react.ScalaComponent
//    .build[JsonProperties]("json")
//    .initialState(JsonState(true))
//    .renderBackend[JsonBackend]
//    .build

  def render(state: LocalizableJson): VdomElement =
    <.div(
      JsonComponent(
        JsonProperties(
          state,
          List("en", "de"),
          json => bs.modState(s => s.deleteElement(json))
        )
      )
    )
}
