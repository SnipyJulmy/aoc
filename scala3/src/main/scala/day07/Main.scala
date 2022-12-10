package day07

import scala.util.Using
import scala.collection.mutable

enum TermOutput:
  case File(val name: String, val size: Long)
  case Directory(val name: String)
  case Cd(val name: String)
  case Ls

enum Command:
  case Cd(val name: String)
  case Ls(val output: List[TermOutput.File | TermOutput.Directory])

def parse(lines: List[String]): List[TermOutput] =
  val cd   = raw"\$$ cd ([\S\\]+)".r
  val ls   = raw"\$$ ls".r
  val dir  = raw"dir (\S+)".r
  val file = raw"(\d+) (\S+)".r
  lines.map { (line: String) =>
    line match {
      case cd(id, _*)           => TermOutput.Cd(id)
      case ls(_*)               => TermOutput.Ls
      case dir(name, _*)        => TermOutput.Directory(name)
      case file(size, name, _*) => TermOutput.File(name, size.toLong)
    }
  }

def interpret(output: List[TermOutput]): List[Command] = output match
  case TermOutput.Cd(name) :: xs => Command.Cd(name) :: interpret(xs)
  case TermOutput.Ls :: xs =>
    val (ys, zs) = xs.span(_.isInstanceOf[TermOutput.File | TermOutput.Directory])
    Command.Ls(ys.asInstanceOf[List[TermOutput.File | TermOutput.Directory]]) :: interpret(zs)
  case Nil => Nil
  case _ =>
    assert(false)

case class Root()
case class File(name: String, size: Long)
case class Directory(
    name: String,
    files: mutable.Set[File] = mutable.Set[File](),
    directories: mutable.Set[Directory] = mutable.Set[Directory]()
) {
  private var _parent: Option[Directory] = None

  def setParent(parent: Directory): Unit = _parent match
    case None => _parent = Some(parent)
    case Some(value) =>
      if value.name != parent.name then throw new IllegalStateException(s"Directory $name already has a parent : (${value.name} != ${parent.name})")

  def parent: Option[Directory] = _parent

  def size: Long = files.map(_.size).sum + directories.map(_.size).sum
}

def generate(commands: List[Command]): mutable.Map[String, Directory] =
  val directories = mutable.Map[String, Directory]()

  def directoryPath(name: String, parent: Directory): String =
    if parent == null then s"$name" else s"${parent.name}-$name"

  def getOrCreateDirectory(name: String, parent: Directory): Directory =
    val directoryName = directoryPath(name, parent)
    directories.get(directoryName) match
      case None =>
        directories.addOne(directoryName, Directory(directoryName))
        directories(directoryName)
      case Some(value) =>
        value

  val _ = commands.foldLeft[Directory](null) { (currentDirectory, command) =>
    command match
      case Command.Cd("..") =>
        currentDirectory.parent.getOrElse(throw new IllegalStateException(s"Can't go to the parent of $currentDirectory"))
      case Command.Cd(name) =>
        val newCurrentDirectory = getOrCreateDirectory(name, currentDirectory)
        if currentDirectory != null then currentDirectory.directories.add(newCurrentDirectory)
        newCurrentDirectory.setParent(currentDirectory)
        newCurrentDirectory
      case Command.Ls(outputs) =>
        outputs.foreach {
          case TermOutput.Directory(name) =>
            val directory = getOrCreateDirectory(name, currentDirectory)
            directory.setParent(currentDirectory)
            currentDirectory.directories.add(directory)
          case TermOutput.File(name, size) =>
            currentDirectory.files.add(File(name, size))
        }
        currentDirectory
  }
  directories

@main def main(filepath: String): Unit =
  val input: List[String] = Using(scala.io.Source.fromFile(filepath)) { source =>
    source.getLines().toList
  }.getOrElse { println(s"Unable to read $filepath"); sys.exit(1) }

  val termOuputs = parse(input)
  val commands   = interpret(termOuputs)
  val fs         = generate(commands)

  val directories = fs.values.toList

  val totalMemory    = 70000000L
  val neededMemory   = 30000000L
  val usedMemory     = fs("/").size
  val unusedMemory   = totalMemory - usedMemory
  val memoryToDelete = neededMemory - unusedMemory

  println(s"Total memory : $totalMemory")
  println(s"Used memory : $usedMemory")
  println(s"Unused memory : $unusedMemory")
  println(s"Needed memory : $neededMemory")
  println(s"Memory to delete : $memoryToDelete")

  val score1 = directories.map(_.size).filter(_ <= 100000).sum
  val score2 = directories.map(_.size).filter(_ > memoryToDelete).min

  println(s"======== Day 07 ========")
  println(s"Score part 1 : $score1")
  println(s"Score part 2 : $score2")
  println("=" * 24)
