# About This Repo
I gonna re-write some code at [https://github.com/CrazyFork/yooreeka](https://github.com/CrazyFork/yooreeka)
with scala as means to help me understand more of the algorithms in that book(*the algorithms of intelligent web*)
also be more familiar with scala language.

the work starts from chapter 04.



### scala traphole


Generic Array with Map return ArraySeq

    // this just suggest using Array in scala is pretty much bad idea
    // http://stackoverflow.com/questions/12837799/scala-array-map-returns-arrayseq
    // painful as fuck !, had to hate scala this way
    def getValues[T: ClassTag](attrs: Array[Attribute[T]]): Array[T] = attrs.map(_.value)











