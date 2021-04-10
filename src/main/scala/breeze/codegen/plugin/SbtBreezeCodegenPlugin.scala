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

import scala.collection.JavaConversions._

object SbtBreezeCodegenPlugin extends AutoPlugin {

  val BreezeCodegen = config("expand")
  val generate = TaskKey[Seq[File]]("generate")

  /**
    * use this if you don't want jflex to run automatically (because, e.g., you're checking it in)
    * you'll want to set [[target]] in [[jflex]] using [[unmanagedJflexSettings]] or your own variant
    */
  lazy val commonJflexSettings: Seq[Def.Setting[_]] = inConfig(BreezeCodegen)(Seq(
    sourceDirectory := (sourceDirectory in Compile).value / "codegen",
    generate := {
      val inDir = sourceDirectory.in(Compile).value / "codegen"
      val outDir = target.value
      val out = streams.value
      // TODO: make this a real plugin or something
      val cachedCompile = FileFunction.cached(out.cacheDirectory / "codegen") { (inFiles: Set[File]) =>
        val outputFiles = for (inFile <- inFiles) yield {
          val outFile = CodegenExpand.outputFilePathFor(inDir.toPath, outDir.toPath, inFile.toPath)
          CodegenExpand.codegenFile(inFile.toPath, outFile)
          outFile.toFile
        }
        outputFiles
      }
      cachedCompile((inDir ** "*.scala").get.toSet).toSeq
    },
    target := (sourceManaged in Compile).value
  )) ++ Seq(
    sourceGenerators in Compile += (generate in BreezeCodegen).taskValue,
    cleanFiles += (target in BreezeCodegen).value
  )
}
