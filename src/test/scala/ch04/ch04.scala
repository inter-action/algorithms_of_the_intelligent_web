package iweb.ch04

import iweb.ch04.models.{Attribute, Cluster, NumericDataPoint}
import iweb.ch04.rock.SimilarCluster
import iweb.ch04.utils.TermFrequencyBuilder
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class TermFrequencyBuilderSpec extends FlatSpec with Matchers {

  "TermFrequencyBuilder" should "works correctly" in {

    val x = Array("cat", "dog")
    val y = Array("dog", "mouse")

    val result = (Array(1.0, 1.0, 0), Array(0, 1.0, 1.0))
    TermFrequencyBuilder.buildTermFrequencyVectors(x, y) match  {
      case (x, y) =>
        x shouldBe(result._1)
        y shouldBe(result._2)

      case _ =>
        fail("test failed")
    }


  }
}

class OtherSpec extends FlatSpec with Matchers {
  "Cluster copy " should "also copy its elements" in {
    val elements = mutable.Set(
      NumericDataPoint("cat-1", Array(
        Attribute("height", 30),
        Attribute("weight", 20)
      )),

      NumericDataPoint("cat-2", Array(
        Attribute("height", 40),
        Attribute("weight", 30)
      ))
    )

    val cluster = Cluster[NumericDataPoint] ("test-1", elements)
    val ncluster = cluster.clone()
    ncluster.elements += NumericDataPoint("cat-3", Array(
      Attribute("height", 30),
      Attribute("weight", 20)
    ))


    cluster.elements.size shouldBe(2)
  }

  "NumericDataPoint copy " should "also copy its attributes value" in {
    val dp = NumericDataPoint("cat-1", Array(
      Attribute("height", 30),
      Attribute("weight", 20)
    ))

    val ndp = dp.clone.copy("new")
    ndp.attrs(0) = Attribute("height", 40)

    dp.attrs(0).value shouldBe 30
  }

  "sort" should "work for List SimilarityCluster" in {
    val ls = List(SimilarCluster(2, 3.0), SimilarCluster(3, 4.0))
    val nls = ls.sorted
    nls(0).clusterKey shouldBe 3
  }
}
