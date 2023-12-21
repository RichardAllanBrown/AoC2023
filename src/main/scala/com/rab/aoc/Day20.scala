package com.rab.aoc

import scala.annotation.tailrec

object Day20 {
  sealed trait Module() {
    val id: String
    def handlePulse(pulse: Pulse): (Module, Option[Boolean])
  }
  case class FlipFlopModule(id: String, state: Boolean) extends Module {
    override def handlePulse(pulse: Pulse): (FlipFlopModule, Option[Boolean]) = {
      if pulse.state then (this, None) else (copy(state = !state), Some(!state))
    }
  }
  case class ConjunctionModule(id: String, states: Map[String, Boolean]) extends Module {
    override def handlePulse(pulse: Pulse): (Module, Option[Boolean]) = {
      val newState = states.updated(pulse.sourceModule, pulse.state)
      (copy(states = newState), Some(!newState.values.forall(identity)))
    }
  }
  case object BroadcastModule extends Module {
    val id = "broadcaster"
    override def handlePulse(pulse: Pulse): (Module, Option[Boolean]) = {
      (this, Some(pulse.state))
    }
  }
  case class Output(id: String, receivedLow: Boolean) extends Module {
    override def handlePulse(pulse: Pulse): (Module, Option[Boolean]) = {
      if pulse.state then (this, None) else (copy(receivedLow = true), None)
    }
  }
  case object Button extends Module {
    override val id: String = "button"
    override def handlePulse(pulse: Pulse): (Module, Option[Boolean]) = {
      (this, None)
    }
  }

  case class Wire(sourceModule: String, destModules: Seq[String])
  case class Pulse(sourceModule: String, destModule: String, state: Boolean) {
    override def toString = {
      sourceModule + (if state then "--high->" else "--low-->") + destModule
    }
  }

  case class ModuleState(modules: Map[String, Module], wires: Seq[Wire], highPulses: Long, lowPulses: Long)

  @tailrec
  def stepState(state: ModuleState, pulses: Seq[Pulse]): ModuleState = {
    if pulses.isEmpty then state
    else {
      val pulse = pulses.head
      //println(s"Current pulse: $pulse")
      val (newModuleState, output) = state.modules(pulse.destModule).handlePulse(pulse)
      val wireDestinations = state.wires.find(_.sourceModule == newModuleState.id).map(_.destModules).getOrElse(Seq.empty)
      val newPulses = output.map(s => {
        wireDestinations.map(d => Pulse(newModuleState.id, d, s))
      }).getOrElse(Seq.empty)

      val newHighPulseCount = state.highPulses + newPulses.count(_.state)
      val newLowPulseCount = state.lowPulses + newPulses.count(!_.state)
      val newModulesState = state.modules.updated(newModuleState.id, newModuleState)
      val newState = ModuleState(newModulesState, state.wires, newHighPulseCount, newLowPulseCount)

      stepState(newState, pulses.tail.concat(newPulses))
    }
  }

  def parseInput(lines: List[String]): ModuleState = {
    val wires = lines.map(l => {
      val splitLine = l.split(" -> ")
      val wireDestinations = splitLine(1).split(',').map(_.trim).toSeq
      val wireSource = if splitLine(0).head == '%' || splitLine(0).head == '&' then splitLine(0).tail else splitLine(0)
      Wire(wireSource, wireDestinations)
    })

    val modules = lines.map(l => {
      val moduleTypeAndId = l.split(" -> ").head
      if moduleTypeAndId.head == '%' then FlipFlopModule(moduleTypeAndId.tail, false)
      else if moduleTypeAndId.head == '&' then
        val moduleId =  moduleTypeAndId.tail
        val initState =  wires.filter(_.destModules.contains(moduleId)).map(_.sourceModule -> false)
        ConjunctionModule(moduleId, initState.toMap)
      else if moduleTypeAndId == "broadcaster" then BroadcastModule
      else throw new IllegalArgumentException(s"Failed to parse module from '$l'")
    })

    val outputs = wires.flatMap(_.destModules).toSet -- modules.map(_.id)
    val allModules = outputs.map(m => Output.apply(m, false)) ++ modules
    ModuleState(allModules.map(m => m.id -> m).toMap, wires, 0, 0)
  }

  def pushButton(state: ModuleState): ModuleState = {
    val newState = state.copy(lowPulses = state.lowPulses + 1)
    val pulses = Seq(Pulse(Button.id, BroadcastModule.id, false))
    stepState(newState, pulses)
  }

  def solvePart1(lines: List[String]): Long = {
    val initState = parseInput(lines)
    val endState = (1 to 1000).foldLeft(initState)((state, _) => pushButton(state))
    endState.lowPulses * endState.highPulses
  }
}
