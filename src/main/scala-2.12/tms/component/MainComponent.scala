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
case class MainState(history: List[LocalizableJson]) {
  def undo = MainState(history.tail)
}

class MainBackend(bs: BackendScope[List[String], MainState]) {

  def onDelete(json: LocalizableJson): Callback =
    bs.modState { s =>
      s.history.headOption
        .map(_.deleteElement(json))
        .map(h => MainState(h :: s.history))
        .getOrElse(s)
    }

  def onUpdate(oldJson: LocalizableJson, newJson: LocalizableJson): Callback =
    bs.modState { s =>
      s.history.headOption
        .map(_.upsertElement(oldJson, newJson))
        .map(h => MainState(h :: s.history))
        .getOrElse(s)
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
          state.history.head,
          prop,
          onDelete,
          onUpdate
        )
      )
    )
}

