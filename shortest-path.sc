val g = Map(
  1 -> Set(9, 8, 2),
  2 -> Set(3, 7, 4),
  3 -> Set(1, 5),
  5 -> Set(6),
  6 -> Set(4),
  7 -> Set(1, 3),
  8 -> Set(9),
  9 -> Set(4)
)

type PathKey = (Int, Int)
type Path = List[Int]
type PathAcc = List[Path]

val EmptyPath = List.empty[Int]
val EmptyPathCache = Map.empty[PathKey, Path]
val EmptyPathAcc = List.empty[Path]

def path(g: Map[Int, Set[Int]], 
         source: Int, 
         target: Int, 
         visited: Set[Int] = Set.empty[Int], 
         known: Map[PathKey, Path] = EmptyPathCache,
         p: Path = EmptyPath): Path = {
    
  if (known.nonEmpty && known.values.minBy(_.length).length <= p.length) {
      println("Backtracing: cut-off at " + p.reverse.mkString(","))
      EmptyPath
  } else if (visited.contains(source)) {
      println("loop: " + source)
      EmptyPath
  } else {
    println("Visited: " + source)
    g.get(source).map{ neighbors =>
      val pathes = neighbors.foldLeft(known -> EmptyPathAcc){ case ((known, acc), neighbor) => 
          println("Looked at: " + neighbor)
          if (neighbor == target) {
            println("Arrived at: " + neighbor)
            val optimal = target :: source :: p
            known -> (optimal :: acc)
          } else {
            known.get(neighbor -> target) map {optimal => 
              println("Cache hit: " + neighbor -> target + " = " + optimal)
              known -> (optimal :: acc)
            } getOrElse {
              val optimal = path(g, neighbor, target, visited + source, known, source :: p)
              val cacheUpdated = known + ((neighbor -> target) -> optimal)
              cacheUpdated -> (optimal :: acc)
            }
          }
      }._2.filter(_.nonEmpty)
      if (pathes.nonEmpty) pathes.minBy(_.length).toList else EmptyPath
    }.getOrElse{
        println("terminal node: " + source)
        EmptyPath
    }
  }
}
      
path(g, 1, 4).reverse //== List(1,2,4)
