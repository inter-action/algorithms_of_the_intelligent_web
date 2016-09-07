package iweb.ch04

import iweb.ch04.models.{Attribute, NumbericDataPoint, Cluster}
import iweb.ch04.utils.TermFrequencyBuilder
import org.scalatest.{Matchers, FlatSpec}

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
      NumbericDataPoint("cat-1", Array(
        Attribute("height", 30),
        Attribute("weight", 20)
      )),

      NumbericDataPoint("cat-2", Array(
        Attribute("height", 40),
        Attribute("weight", 30)
      ))
    )

    val cluster = Cluster[Double, NumbericDataPoint] ("test-1", elements)
    val ncluster = cluster.clone()
    ncluster.elements += NumbericDataPoint("cat-3", Array(
      Attribute("height", 30),
      Attribute("weight", 20)
    ))


    cluster.elements.size shouldBe(2)
  }
}
