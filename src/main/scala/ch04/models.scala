package iweb.ch04.models

import iweb.ch04.similarity.EuclideanDistance

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.reflect.ClassTag

case class Attribute[T](name: String, value: T)

object Attribute{
  def createAttributes[T](attrValues: Array[T]): Array[Attribute[T]] =
    attrValues.zipWithIndex.map{ case (t, idx) => Attribute("a-"+idx, t)}


  def getNames[T](attrs: Array[Attribute[T]]): Array[String] = attrs.map(_.name)

  // this just suggest using Array in scala is pretty much bad idea
  // http://stackoverflow.com/questions/12837799/scala-array-map-returns-arrayseq
  // painful as fuck !, had to hate scala this way
  def getValues[T: ClassTag](attrs: Array[Attribute[T]]): Array[T] = attrs.map(_.value)
}


trait DataPoint[T]{
  val label: String
  val attrs: Array[Attribute[T]]
  val attrNames: Array[String] = Attribute.getNames(attrs)
  val values: Array[T]

  def getR: Double

  def getAttributeCount:Int = attrs.length
  // only print values of attrs
  def toShortString = s"${label} ( ${attrs.map(_.value.toString).mkString("\n")} )"
  override def toString = s"${label} ( ${attrs.map(_.toString()).mkString("\n")} )"
}

case class NumbericDataPoint(label: String, attrs: Array[Attribute[Double]]) extends DataPoint[Double]{
  override val values = Attribute.getValues(attrs)
  override def getR: Double = EuclideanDistance.getDistance(new Array[Double](attrs.length), values)
}



// todo: whether change elements to immutable objects
case class Cluster[T <: DataPoint[_]](
                    label: String = "",
                    elements: scala.collection.mutable.Set[T] = mutable.LinkedHashSet.empty[T]){


  def this(label: String, elements: Seq[T]){
    this(label, mutable.LinkedHashSet.empty[T] ++ elements)
  }

  def this(c1: Cluster[T], c2: Cluster[T]){
    this("", c1.elements ++ c2.elements)
  }

  def this(c1: Cluster[T]*){
    this("",
      c1.foldLeft(mutable.LinkedHashSet.empty[T]){ (acc, c) =>
        acc ++ c.elements
      }
    )
  }

  def size = elements.size

  def getDimensionCount = if (elements.isEmpty) 0 else elements.iterator.next().getAttributeCount

  // def copy: Cluster ?, elements need to be copied?

  def add(cluster: Cluster[T]):Unit = {
    elements ++= cluster.elements
  }

  def add(e: T): Unit ={
    elements += e
  }

  def contains(c: Cluster[T]) = elements.intersect(c.elements).size != 0

  def contains(e: T) = elements.contains(e)

  def getElements: mutable.Set[T] = mutable.LinkedHashSet.empty[T] ++ elements

  override def toString = s"${elements.mkString(",\n")} \n ${label}"

}