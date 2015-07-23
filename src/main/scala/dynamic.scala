package com.cra.summerschool.solutions

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.language.Flip
import com.cra.figaro.language.Constant
import com.cra.figaro.language.Element
import com.cra.figaro.language.Apply
import com.cra.figaro.library.compound.If
import com.cra.figaro.language.Universe
import com.cra.figaro.language.Select
import com.cra.figaro.algorithm.filtering.ParticleFilter
import com.cra.figaro.language.Observation
import com.cra.figaro.language.NamedEvidence

class SoccerModelPF {

  //Initial model
  val initial = Universe.createNew //Create the initial universe
  val winning = Constant('none)("winning", initial) //Name each element and add it to the universe
  val confident = Flip(0.4)("confident", initial)
  val scoreDifferential = Constant(0)("scoreDifferential", initial)
  val ourPossession = Constant(false)("ourPosession", initial)
  val goal = Constant(false)("goal", initial)

  //Transition model
  def trans(previousState: Universe): Universe = {
    val newState = Universe.createNew //Create the new universe

    val previousConfident = previousState.get[Boolean]("confident") //Get elements from the previous universe
    val previousScoreDifferential = previousState.get[Int]("scoreDifferential")

    val winning =
      Apply(previousScoreDifferential, (i: Int) =>
        if (i > 0) 'us else if (i < 0) 'them else 'none)("winning", newState)
    val confident = //Transition model defining a distribution for each hidden state variable based on the one before in the sequence
      CPD(previousConfident, winning,
        (true, 'us) -> Flip(0.9),
        (true, 'none) -> Flip(0.7),
        (true, 'them) -> Flip(0.5),
        (false, 'us) -> Flip(0.5),
        (false, 'none) -> Flip(0.3),
        (false, 'them) -> Flip(0.1))("confident", newState)
    val ourPossession = If(confident, Flip(0.7), Flip(0.3))("ourPossession", newState) //Observation model defining a distribution for each observation variable given its corresponding hidden state variable
    val goal =
      CPD(ourPossession, confident,
        (true, true) -> Flip(0.04),
        (true, false) -> Flip(0.01),
        (false, true) -> Flip(0.045),
        (false, false) -> Flip(0.02))("goal", newState)
    val scoreDifferential =
      If(goal,
        Apply(ourPossession, previousScoreDifferential,
          (poss: Boolean, diff: Int) =>
            if (poss) (diff + 1).min(5) else (diff - 1).max(-5)),
        previousScoreDifferential)("scoreDifferential", newState)
    newState //Return the new state
  }

}

object SoccerGamePF {

  def createModel(): SoccerModelPF = {
    new SoccerModelPF
  }

  def predict(model: SoccerModelPF, length: Int) {
    val pf = ParticleFilter(model.initial, model.trans, 10000)
    pf.start
    for (t <- 1 until length) {
      pf.advanceTime(List(NamedEvidence("ourPossession", Observation(true))))
      println("Probability we are confident at timestep " + t + ": " + pf.currentProbability("confident", true))
    }
    pf.kill
  }

  def main(args: Array[String]) {
    val game = createModel()
    val length = 10
    predict(game, length)
  }
}