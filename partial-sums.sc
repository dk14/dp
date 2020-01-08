import scala.collection.mutable
import scala.util.Random


type KeyF[T] = Array[T]
type Cache = mutable.Map[List[Int], Stream[Int]]

val N = 1000
val M = 30

val data = Array.fill(N)(Array.fill(M)(Random.nextInt(100)).sorted.reverse)

def mergeSort(subsums: KeyF[Stream[Int]]): Stream[Int] = {
  lazy val (max, i) = subsums.zipWithIndex.maxBy(_._1.headOption.getOrElse(Integer.MIN_VALUE))
  max.headOption.getOrElse(Integer.MIN_VALUE) #:: mergeSort(subsums.zipWithIndex.map{
    case (substream, j) if j == i => substream.tail
    case (substream, _) => substream
  })
}

def memoize(f: KeyF[Int] => Stream[Int], cache: Cache)(key: KeyF[Int]): Stream[Int] = {
  //println(key.toList)
  //if (cache.contains(key.toList)) println("Hit! : " + key.toList)
  cache.getOrElseUpdate(key.toList, f(key))
}

def sums(cursors: KeyF[Int], n: Int, k: Int, data: Array[Array[Int]], cache: Cache) : Stream[Int] = scala.util.Try {
  lazy val column = cursors.zipWithIndex.map{case (j, i) => (i, data(i)(j), data(i)(j) - data(i)(j + 1))}
  lazy val sum = column.map(_._2).sum
  lazy val min = column.map(_._3).min
  lazy val selectedRows = column.filter(_._3 == min).map(_._1)

  lazy val subSums = selectedRows
    .map(i => cursors.updated(i, cursors(i) + 1))
    .map(memoize(sums(_, n, k * selectedRows.size, data, cache), cache))

  if (k > n) Stream(sum) else
    sum #:: mergeSort(subSums)
}.getOrElse(Stream(Integer.MIN_VALUE))

def computeSums(data: Array[Array[Int]], n: Int) : Stream[Int] = {
  sums(data.map(_ => 0), n, 1, data, mutable.Map[List[Int], Stream[Int]]()).take(n).takeWhile(_ != Integer.MIN_VALUE)
}


//assert(computeSums(data, 100).toList.take(10) == computeSums(data, 10).toList)
//assert(computeSums(data, 1000).toList.take(105) == computeSums(data, 105).toList)

val result = computeSums(data, 25).toList

println(result)

