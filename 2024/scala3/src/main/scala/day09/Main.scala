package day09

@main
def main(filepath: String): Unit =

  val input      = aoc.readInput(filepath)
  val filesystem = input.head.toList.map(_.toString().toInt)
  val score1     = compact(expand(filesystem)).zipWithIndex.map { case (a, b) => BigInt(a) * BigInt(b) }.reduce(_ + _)
  val score2     = defrag(expand(filesystem)).zipWithIndex.map { case (a, b) => BigInt(a) * BigInt(b) }.reduce(_ + _)

  println(s"${"=" * 8} Day 09 ${"=" * 8}")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)

// fill . with -1
def expand(filesystem: List[Int]): List[Int] =
  val groups =
    for (n, idx) <- filesystem.zipWithIndex
    yield if idx % 2 == 0 then List.fill(n)(idx / 2) else List.fill(n)(-1)
  groups.toList.flatten

def compact(filesystem: List[Int]): List[Int] =
  val fs    = filesystem.toVector
  val array = Array.fill(filesystem.count(_ >= 0))(-1)
  var left  = 0
  var right = filesystem.length - 1
  while left < right do
    if fs(left) >= 0 then array(left) = fs(left)
    else
      array(left) = fs(right)
      right -= 1
      while fs(right) < 0 && right > left do right -= 1
    left += 1
  array.toList

def defrag(filesystem: List[Int]): List[Int] =
  val fs    = filesystem.toVector
  val array = fs.toArray
  var left  = 0
  var right = filesystem.length - 1
  while left < right do
    if fs(left) >= 0 then
      array(left) = fs(left)
      left += 1
    else
      var idx = left
      while fs(idx) < 0 do idx += 1
      val freeSpace = idx - left
      idx = right
      while fs(idx) >= 0 do idx -= 1
      val requiredSpace = right - idx
      if freeSpace >= requiredSpace then
        while fs(right) >= 0 do
          array(left) = fs(right)
          left += 1
          right -= 1
  array.toList
