package breeze.codegen

import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.ListMap
import scala.meta.Term.Block
import scala.meta._

object CodegenExpand {

  def codegenFile(dialect: Dialect, inputFile: Path, outputFile: Path): Path = {
    val input = Input.File(inputFile)
    val exampleTree: Source = dialect(input).parse[Source].get
    val outputTree: Tree = processTree(exampleTree)(dialect)

    Files.createDirectories(outputFile.getParent)
    Files.write(
      outputFile,
      dialect(outputTree).syntax.getBytes("UTF-8")
    )
  }

  def processTree(tree: Tree)(implicit dialect: Dialect): Tree = {
    flatTransformDefns(tree) {
      case Defn.Def(mods, name, targs, vargs, rtype, rhs) if hasExpand(mods) =>
        val (typesToExpand, typesLeftAbstract) = targs.partition(shouldExpand)

        val exclusions: Seq[Map[String, String]] = getExclusions(mods, targs.map(_.name))
        val shouldValify = checkValify(mods)

        val typesToUnrollAs: ListMap[String, List[String]] = ListMap.empty ++ typesToExpand.map { td =>
          td.name.value -> typeMappings(td)
        }

        val (valsToExpand, valsToLeave) = vargs.map(_.partition(shouldExpandVarg)).unzip
        val valsToExpand2 = valsToExpand.flatten

        val configurations: Seq[Map[String, String]] = transposeListMap(typesToUnrollAs).filterNot(exclusions.toSet)
        val valExpansions = valsToExpand2.map { v =>
          v.name.value -> solveSequence(v, typesToUnrollAs)
        }.toMap

        val newDefs = configurations.map { stringlyTypedMap =>
          val typeMap = for ((k, v) <- stringlyTypedMap) yield (k, v.parse[Type].get)
          val grounded = substitute(typeMap, valExpansions, rhs).asInstanceOf[Term]
          val newvargs =
            valsToLeave.filterNot(_.isEmpty).map(_.map(substitute(typeMap, valExpansions, _).asInstanceOf[Term.Param]))
          val newtpt = rtype.map(substitute(typeMap, valExpansions, _).asInstanceOf[Type])
          val newName = mkName(name, typesToExpand.map(t => typeMap(t.name.value).toString))
          val newMods = stripOurAnnotations(mods)
          if (shouldValify) {
            if (typesLeftAbstract.nonEmpty)
              error(tree.pos, "Can't valify: Not all types were grounded: " + typesLeftAbstract.mkString(", "))
            if (newvargs.exists(_.nonEmpty))
              error(
                tree.pos,
                "Can't valify: Not all arguments were grounded: " + newvargs
                  .map(_.mkString(", "))
                  .mkString("(", ")(", ")"))
            Defn.Val(
              mods = newMods,
              pats = List(Pat.Var(name = Term.Name(newName))),
              decltpe = newtpt,
              rhs = grounded
            )
          } else {
            val newTargs = typesLeftAbstract.map(substitute(typeMap, valExpansions, _)).asInstanceOf[List[Type.Param]]
            Defn.Def(newMods, Term.Name(newName), newTargs, newvargs, newtpt, grounded)
          }
        }

        newDefs

    }
  }

  private def hasExpand(mods: List[Mod]): Boolean = mods.exists{
    case Mod.Annot(Init(Type.Name("expand"), _, _)) => true
    case _ => false
  }

  private def error(pos: Position, message: String) = throw new RuntimeException(s"$pos $message")

  private def flatTransformDefns(tree: Tree)(pf: PartialFunction[Defn, Seq[Defn]]): Tree = {
    tree.transform {
      case cls: Defn.Class =>
        val newStats = cls.templ.stats.flatMap {
          case defn: Defn => pf.applyOrElse(defn, (_: Any) => Seq(defn))
          case x => Seq(x)
        }
        cls.copy(templ=cls.templ.copy(stats=newStats))
      case cls: Defn.Trait =>
        val newStats = cls.templ.stats.flatMap {
          case defn: Defn => pf.applyOrElse(defn, (_: Any) => Seq(defn))
          case x => Seq(x)
        }
        cls.copy(templ=cls.templ.copy(stats=newStats))
      case cls: Source =>
        val newStats = cls.stats.flatMap {
          case defn: Defn => pf.applyOrElse(defn, (_: Any) => Seq(defn))
          case x => Seq(x)
        }
        cls.copy(stats=newStats)
      case obj: Defn.Object =>
        val newStats = obj.templ.stats.flatMap {
          case defn: Defn => pf.applyOrElse(defn, (_: Any) => Seq(defn))
          case x => Seq(x)
        }
        obj.copy(templ=obj.templ.copy(stats=newStats))
    }
  }

  private object ExType {
    def unapply(q: Type): Option[String] = q match {
      case n@Type.Select(_, _) => Some(n.syntax)
      case n@Type.Name(_) => Some(n.syntax)
      case _ => None

    }
  }

  /** for a valdef with a [[breeze.macros.expand.sequence]] annotation, converts the sequence of associations to a Map.
    * The return value is the name of the associated abstract type and the sequence of concrete values to sub in*/
  private def solveSequence(v: Term.Param, typeMappings: Map[String, List[String]]): (String, Map[String, Term]) = {
    v.mods.collectFirst {
      case m@Mod.Annot(i@Init(Type.Apply(ExType("expand.sequence"), Seq(correspondingType)), _, args)) =>
        val name = coerceNameFromType(correspondingType)
        if (args.flatten.length != typeMappings(name).length) {
          error(m.pos, s"@sequence arguments list does not match the expand.args for name")
        }
        name -> typeMappings(name).zip(args.flatten).toMap.map { case (k, v) => k -> v}
    }.get
  }

  /**
    * Returns the set of all types that this type should be unrolled as.
    * @
    * param c
    * @param td
    * @return
    */
  private def typeMappings(td: Type.Param): List[String] = {
    val mods = td.mods.collect {
      case Mod.Annot(Init(ExType("expand.args"), _, args)) =>
        args.flatten.map { tree => tree.syntax}
    }.flatten
    mods
  }

  private def transposeListMap[A, B](types: ListMap[A, Seq[B]]): Seq[Map[A, B]] = {
    types.foldLeft(Seq(Map.empty[A, B])) { (acc, pair) =>
      val (nme, types) = pair
      for (t <- types; map <- acc) yield map + (nme -> t)
    }
  }

  private def getExclusions(mods: List[Mod], targs: Seq[Name]): Seq[Map[String, String]] = {
    mods.collect {
      case  t@Mod.Annot(i@Init(ExType("expand.exclude"), _, List(args))) =>
        if (args.length != targs.length)
          error(t.pos, "arguments to @exclude does not have the same arity as the type symbols!")
        targs.map(_.syntax).zip(args.map(_.syntax)).toMap
    }
  }

  private def checkValify(mods: List[Mod]) = {
    mods.collectFirst {
      case Mod.Annot(i@Init(ExType("expand.valify"), _, _)) => true
    }.getOrElse(false)
  }

  private def shouldExpand(td: Type.Param): Boolean = {
    td.mods.exists {
      case Mod.Annot(Init(ExType("expand.args"), _, args)) =>
        true
      case _ => false
    }
  }

  private def shouldExpandVarg(td: Term.Param): Boolean = {
    td.mods.exists {
      case Mod.Annot(i@Init(Type.Apply(ExType("expand.sequence"), targs), _, args)) => true
      case _ => false
    }
  }

  private def termNameToType(aa: Term): Type.Name = {
    Type.Name(aa.asInstanceOf[Term.Name].value)
  }

  private def mkName(name: Name, groundedTypes: Seq[String]): String = {
    groundedTypes.map { _.reverse.takeWhile(_ != '.').reverse }.mkString(name.toString + "_", "_", "")
  }

  private def substitute(typeMap: Map[String, Type], valExpansions: Map[String, (String, Map[String, Term])], body: Tree): Tree = {
    body.transform {
      // substitute identifiers for identifiers
      case Type.Name(x) if typeMap.contains(x) =>
        typeMap(x)
      case Term.Name(x) if typeMap.contains(x) =>
        val nme = coerceNameFromType(typeMap(x))
        Term.Name(nme)
      case Term.Name(x) if valExpansions.contains(x) =>
        val (tname, tmap) = valExpansions(x)
        tmap(coerceNameFromType(typeMap(tname)))
      // inline f(args...) if f is supposed to be expanded.
      case ap@Term.Apply(Term.Name(x), args) if valExpansions.contains(x) =>
        val (tname, tmap) = valExpansions(x)
        val mappedTree = tmap(coerceNameFromType(typeMap(tname)))
        // TODO: this is super fragile. macro annotations handled this fairly well since scala had already
        // done the _ + _ --> (a$1, b$2) => a$1 + b$2 transform
        val withoutBlock = mappedTree match {
          case Block(List(stat)) => stat
          case Block(_) => error(mappedTree.pos, "Can't substitute in this expression")
          case x => x
        }
        withoutBlock match {
          case Term.Function(fargs, body) =>
            // TODO: this produces a StackOverflowError if the identifier being replaced also occurs in the
            // replaced body. E.g. Complex.zero and zero
            // super annoying
            body.transform {
              case n@Term.Name(name) =>
                val pos = fargs.indexWhere(_.name.value == name)
                if (pos >= 0) {
                  args(pos)
                } else {
                  n
                }
            }
          case _ =>
            var i = 0
            val res = withoutBlock.transform {
              case x: Term.Placeholder =>
                i += 1
                args(i - 1)
            }
            if (i != args.length) {
              error(ap.pos, "Mismatch in number of arguments")
            }

            res
        }
    }
  }

  private def coerceNameFromType(tpe: Type) = {
    tpe.asInstanceOf[Type.Name].value
  }

  private def stripOurAnnotations(mods: List[Mod]): List[Mod] = {
    mods.filter {
      case Mod.Annot(Init(ExType("expand"), _, args)) => false
      case Mod.Annot(Init(Type.Select(Term.Name("expand"), _), _, args)) => false
      case _ => true
    }
  }

  def main(args: Array[String]): Unit = {
    val baseDir = Paths.get(args(0))
    val outDir = Paths.get(args(1))

    val filePairs = args.drop(2).map(Paths.get(_)).map { inFile =>
      (inFile, outputFilePathFor(baseDir, outDir, inFile))
    }

    for ( (in, out) <- filePairs) {
      codegenFile(Dialect.current, in, out)
    }

  }

  def outputFilePathFor(inputBaseDir: Path, outDir: Path, inFile: Path): Path = {
    val relPath = inputBaseDir.relativize(inFile)
    val outFileName = relPath.toString.replaceAll("\\.scala$", ".expanded.scala")
    val outFile = outDir.resolve(outFileName)
    outFile
  }
}