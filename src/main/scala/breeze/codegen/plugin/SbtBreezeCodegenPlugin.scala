/*
 * Copyright 2011 Steffen Fritzsche.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package breeze.codegen.plugin

import sbt._

import Process._
import Keys._
import breeze.codegen.CodegenExpand
import scala.meta._

import scala.collection.JavaConversions._

object SbtBreezeCodegenPlugin extends AutoPlugin {

  val BreezeCodegen = config("expand")
  val generate = TaskKey[Seq[File]]("generate")
  val dialect = SettingKey[scala.meta.Dialect]("dialect")

  lazy val breezeCodegenSettings: Seq[Def.Setting[_]] = inConfig(BreezeCodegen)(Seq(
    dialect := {
      CrossVersion.partialVersion( (scalaVersion in Compile).value) match {
        case Some((2, 11)) => dialects.Scala211
        case Some((2, 12)) => dialects.Scala212
        case Some((2, 13)) => dialects.Scala213
        case Some((3, _)) => dialects.Scala3
        case _ => ???
      }
    },
    sourceDirectory := (sourceDirectory in Compile).value / "codegen",
    generate := {
      val inDir = sourceDirectory.in(Compile).value / "codegen"
      val outDir = target.value
      val out = streams.value
      out.log.info(s"Scala version: ${scalaVersion.value} ${dialect.value eq dialects.Scala3}")
      val cachedCompile = FileFunction.cached(out.cacheDirectory / "codegen") { (inFiles: Set[File]) =>
        out.log.info(s"Loading ${inFiles}")
        val outputFiles = for (inFile <- inFiles) yield {
          val outFile = CodegenExpand.outputFilePathFor(inDir.toPath, outDir.toPath, inFile.toPath)
          out.log.info(s"Writing ${inFile} to ${outFile}")
          CodegenExpand.codegenFile(dialect.value, inFile.toPath, outFile)
          outFile.toFile
        }
        outputFiles
      }
      for (f <- (inDir ** "*.scala").get.distinct;
           out <- cachedCompile(Set(f)).toSeq) yield {
        out
      }
    },
    target := (sourceManaged in Compile).value
  )) ++ Seq(
    watchSources += {
      val path = (sourceDirectory in BreezeCodegen).value
      new WatchSource(path, "*.scala", NothingFilter, recursive = true)
    },
    sourceGenerators in Compile += (generate in BreezeCodegen).taskValue,
    cleanFiles += (target in BreezeCodegen).value
  )
}
