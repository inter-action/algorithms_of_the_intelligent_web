package iweb.ch04.models

import iweb.ch04.similarity.EuclideanDistance

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


trait DataPoint{
  type T
  val label: String
  val attrs: Array[Attribute[T]]
  val attrNames: Array[String] = Attribute.getNames(attrs)
  val values: Array[T]

  def getAttributeCount:Int = attrs.length
  // only print values of attrs
  def toShortString = s"${label} ( ${attrs.map(_.value.toString).mkString("\n")} )"
  override def toString = s"${label} ( ${attrs.map(_.toString()).mkString("\n")} )"
}

case class NumericDataPoint(label: String, attrs: Array[Attribute[Double]]) extends DataPoint{
  type T = Double
  def this(label: String, values: Array[Double]){
    this(label, Attribute.createAttributes(values))
  }

  def getR: Double = EuclideanDistance.getDistance(new Array[Double](attrs.length), values)

  override val values:Array[Double] = Attribute.getValues(attrs)

  override def clone(): NumericDataPoint = NumericDataPoint(label, attrs.map( _.copy[Double]() ))
}
//todo:
case class WordDataPoint(label: String, attrs: Array[Attribute[String]]) extends DataPoint{
  type T = String

  def this(label: String, values: Array[String]){
    this(label, Attribute.createAttributes(values))
  }

  override val values:Array[String] = Attribute.getValues(attrs)
  override def clone(): WordDataPoint = WordDataPoint(label, attrs.map( _.copy[T]() ))
}



// todo: whether change elements to immutable objects
case class Cluster[U <: DataPoint](
                    label: String = "",
                    elements: scala.collection.mutable.Set[U] = mutable.LinkedHashSet.empty[U]){


  def this(label: String, elements: Seq[U]){
    this(label, mutable.LinkedHashSet.empty[U] ++ elements)
  }

  def this(c1: Cluster[U]*){
    this("",
      c1.foldLeft(mutable.LinkedHashSet.empty[U]){ (acc, c) =>
        acc ++ c.elements
      }
    )
  }

  def size = elements.size

  def getDimensionCount = if (elements.isEmpty) 0 else elements.iterator.next().getAttributeCount

  def add(cluster: Cluster[U]):Unit = {
    elements ++= cluster.elements
  }

  def add(e: U): Unit ={
    elements += e
  }

  def contains(c: Cluster[U]) = elements.intersect(c.elements).size != 0

  def contains(e: U) = elements.contains(e)

  def getElements: mutable.Set[U] = mutable.LinkedHashSet.empty[U] ++ elements

  override def toString = s"${elements.mkString(",\n")} \n ${label}"

  // def copy: Cluster ?, elements need to be copied?
  // can copy be overrided ?
  override def clone(): Cluster[U] = Cluster(label, mutable.LinkedHashSet.empty ++ elements)

}