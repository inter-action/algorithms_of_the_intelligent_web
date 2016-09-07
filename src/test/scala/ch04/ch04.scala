package iweb.ch04

import iweb.ch04.utils.TermFrequencyBuilder
import org.scalatest.{Matchers, FlatSpec}



class TermFrequencyBuilderSpec extends FlatSpec with Matchers {

  "TermFrequencyBuilder" should "works correctly" in {

    val x = Array("cat", "dog")
    val y = Array("dog", "mouse")

    TermFrequencyBuilder.buildTermFrequencyVectors(x, y) should be (
      Array(Array(1.0, 1.0, 0), Array(0, 1.0, 1.0))
    )


  }
}