package it.unibo.pps.ex2

import ConferenceReviewing.*
import it.unibo.pps.ex2.ConferenceReviewing.Question.FINAL

trait ConferenceReviewing:

  import ConferenceReviewing.Question

  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  def orderedScores(article: Int, question: Question): List[Int]

  def averageFinalScore(article: Int): Double

  def acceptedArticles(): Set[Int]

  def sortedAcceptedArticles(): List[(Int, Double)]

  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:

  def apply() = new ConferenceReviewingImpl();

  enum Question:
    case RELEVANCE
    case SIGNIFICANCE
    case CONFIDENCE
    case FINAL

class ConferenceReviewingImpl extends ConferenceReviewing {

  private var reviews: List[(Int, Map[Question, Int])] = List();

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    val scores: Map[ConferenceReviewing.Question, Int] = Map(
      Question.RELEVANCE -> relevance,
      Question.SIGNIFICANCE -> significance,
      Question.CONFIDENCE -> confidence,
      Question.FINAL -> fin
    )
    loadReview(article, scores);

  override def loadReview(article: Int, scores: Map[ConferenceReviewing.Question, Int]): Unit =
    reviews = reviews :+ (article, scores)

  override def orderedScores(article: Int, question: ConferenceReviewing.Question): List[Int] = reviews.filter({
    case (artId, scores) => artId == article
  }).map({
    case (_, scores) => scores(question)
  }).sorted()

  override def sortedAcceptedArticles(): List[(Int, Double)] = acceptedArticles()
    .toList
    .map(article => (article, averageFinalScore(article)))
    .sortBy({
      case (_, avgFinalScore) => avgFinalScore
    })

  override def acceptedArticles(): Set[Int] = reviews.filter({
      case (article, _) => averageFinalScore(article) > 5.0 && reviews.filter({
        case (artId, _) => artId == article
      }).exists {
        case (artId, scores) => scores(Question.RELEVANCE) >= 8
      }
    }).map({
      case (article, _) => article
    })
    .toSet

  override def averageFinalScore(article: Int): Double =
    val finalScores = reviews.filter({
      case (artId, _) => artId == article
    }).map({
      case (_, scores) => scores(FINAL)
    })
    if (finalScores.nonEmpty) {
      finalScores.sum.toDouble / finalScores.size
    } else {
      0.0
    }

  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    def averageWeightedFinalScore(article: Int): Double =
      val weightedScores = reviews
        .filter({
          case (artId, _) => artId == article
        })
        .map {
          case (_, scores) => scores(ConferenceReviewing.Question.FINAL) * scores(ConferenceReviewing.Question.CONFIDENCE) / 10.0
        }

      if weightedScores.isEmpty then
        0.0
      else
        weightedScores.sum / weightedScores.size

    reviews.map({
        case (article, _) => article
      })
      .distinct
      .map({ article => article -> averageWeightedFinalScore(article)
      })
      .toMap
}

object Test extends App {

  import ConferenceReviewing.*

  val cr: ConferenceReviewing = ConferenceReviewing()

  cr.loadReview(1, 8, 8, 6, 8)
  cr.loadReview(1, 9, 9, 6, 9)
  cr.loadReview(2, 9, 9, 10, 9)
  cr.loadReview(2, 4, 6, 10, 6)
  cr.loadReview(3, 3, 3, 3, 3)
  cr.loadReview(3, 4, 4, 4, 4)
  cr.loadReview(4, 6, 6, 6, 6)
  cr.loadReview(4, 7, 7, 8, 7)

  val map = Map(
    Question.RELEVANCE -> 8,
    Question.SIGNIFICANCE -> 8,
    Question.CONFIDENCE -> 7,
    Question.FINAL -> 8
  )

  cr.loadReview(4, map)

  cr.loadReview(5, 6, 6, 6, 10)
  cr.loadReview(5, 7, 7, 7, 10)

  println(cr.orderedScores(2, Question.RELEVANCE)) // List(4, 9)
  println(cr.orderedScores(4, Question.CONFIDENCE)) // List(6, 7, 8)
  println(cr.orderedScores(5, Question.FINAL)) // List(10, 10)

  println(cr.averageFinalScore(1)) // 8.5
  println(cr.averageFinalScore(2)) // 7.5
  println(cr.averageFinalScore(3)) // 3.5
  println(cr.averageFinalScore(4)) // 7.0
  println(cr.averageFinalScore(5)) // 10.0

  println(cr.acceptedArticles()) // Set(1, 2, 4)
  
  println(cr.sortedAcceptedArticles()) // List((4,7.0), (2,7.5), (1,8.5))
  
  val mapRes = cr.averageWeightedFinalScoreMap()

  println(mapRes.get(1)) // 5.1
  println(mapRes.get(2)) // 7.5
  println(mapRes.get(3)) // 1.25
  println(mapRes.get(4)) // 4.933...
  println(mapRes.get(5)) // 6.5
  println(mapRes.size) // 5

}