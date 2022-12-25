package module1

import scala.util.Random

class BallsExperiment {

  val urna = List(1,0,0,1,1,0)
  def isFirstBlackSecondWhite(urna:List[Int]): Boolean = {
    val blackIndex = Random.nextInt(urna.size)
    val blackBall = urna(blackIndex)
    val urna_new = urna.slice(0, blackIndex).concat(urna.slice(blackIndex+1, urna.size))
    val whiteIndex = Random.nextInt(urna_new.size)
    val whiteBall = urna_new(whiteIndex)
    if (blackBall == 0 && whiteBall == 1) true else false
  }


}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = List.range(0, count)
      .map(_ => new BallsExperiment())
    val countOfExperiments = listOfExperiments.map(x => x.isFirstBlackSecondWhite(x.urna))
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}

