package tms.component

import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import tms.model.LocalizableJson

import scala.collection.mutable

/**
  * Created by markotron on 19/03/2017.
  */
case class MainState(json: LocalizableJson, history: List[LocalizableJson]) {
  def undo = MainState(history(1), history.tail)
}

class MainBackend(bs: BackendScope[List[String], MainState]) {

  def onDelete(json: LocalizableJson): Callback =
    bs.modState { s =>
      val newRoot = s.json.deleteElement(json)
      val newHistory = newRoot :: s.history
      MainState(newRoot, newHistory)
    }

  def onUpdate(oldJson: LocalizableJson, newJson: LocalizableJson): Callback =
    bs.modState { s =>
      val newRoot = s.json.upsertElement(oldJson, newJson)
      val newHistory = newRoot :: s.history
      MainState(newRoot, newHistory)
    }

  def undo: Callback =
    bs.modState { s =>
      if (s.history.size > 1)
        s.undo
      else s
    }

  def render(prop: List[String], state: MainState): VdomElement =
    <.div(
      ^.float := "left",
      <.i(
        ^.className := "fa fa-undo",
        ^.onClick --> undo
      ),
      <.span(
        ^.className := "edited-span",
        s"Edited ${state.history.size - 1} time/times"
      ),
      JsonComponent(
        JsonProperties(
          state.json,
          prop,
          onDelete,
          onUpdate
        )
      )
    )
}
