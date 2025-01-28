package ru.otus.module1.DataCollection1

import scala.util.Random

class BallsExperiment {
  private val boxWithBalls: List[Int] = Random.shuffle(List(1, 1, 1, 0, 0, 0))

  def isFirstBlackSecondWhite: Boolean = {
    val firstTwoBalls = boxWithBalls.take(2)
    firstTwoBalls.head == 0 && firstTwoBalls.last == 1
  }

  def isFirstBlack: Boolean = boxWithBalls.head == 0
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = List.fill(count)(new BallsExperiment())
    val countOfExperiments = listOfExperiments.map(_.isFirstBlackSecondWhite)
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / listOfExperiments.map(_.isFirstBlack).count(_ == true))
  }
}