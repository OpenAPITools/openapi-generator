/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.codegen.config.scala

import com.wordnik.swagger.codegen.LibraryCodeGenerator
import com.wordnik.swagger.codegen.config.LanguageConfiguration
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider
import com.wordnik.swagger.codegen.exception.CodeGenerationException
import com.wordnik.swagger.codegen.util.FileUtil

import java.io.File

object ScalaLibCodeGen {
  def main(args: Array[String]) = {
    val codeGenerator = args.length match {
      case 1 => new ScalaLibCodeGen(args(0))
      case 4 => {
        var apiServerURL = args(0)
        if (!apiServerURL.endsWith("/")) {
          apiServerURL = apiServerURL + "/"
        }
        val apiKey = args(1);
        val packageName = args(2)
        var libraryHome = args(3)
        if (libraryHome.endsWith("/")) {
          libraryHome = libraryHome.substring(0, libraryHome.length() - 1)
        }
        val modelPackageName = packageName + ".model"
        val apiPackageName = packageName + ".api"
        val classOutputDir = libraryHome + "/src/main/scala/" + packageName.replace(".", "/")
        new ScalaLibCodeGen(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome)
      }
      case _ => throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for config json")
    }
    codeGenerator.generateCode()
  }
}

class ScalaLibCodeGen(
  apiServerURL: String,
  apiKey: String,
  modelPackageName: String,
  apiPackageName: String,
  classOutputDir: String,
  libraryHome: String,
  configPath: String) extends LibraryCodeGenerator {

  this.reservedWordMapper = new ScalaReservedWordMapper
  if (null != configPath) {
    initializeWithConfigPath(configPath)
    this.setDataTypeMappingProvider(new ScalaDataTypeMappingProvider())
    this.setNameGenerator(new ScalaNamingPolicyProvider())
  } else {
    initialize(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome)
    setDataTypeMappingProvider(new ScalaDataTypeMappingProvider())
    setNameGenerator(new ScalaNamingPolicyProvider())
  }

  def this(apiServerURL: String, apiKey: String, modelPackageName: String, apiPackageName: String, classOutputDir: String, libraryHome: String) = this(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome, null)
  def this(configPath: String) = this(null, null, null, null, null, null, configPath)

  override def initializeLangConfig(config: LanguageConfiguration): LanguageConfiguration = {
    config.setClassFileExtension(".scala");
    config.setTemplateLocation("conf/scala/templates");
    config.setStructureLocation("conf/scala/structure");
    config.setExceptionPackageName("com.wordnik.swagger.exception");
    config.setAnnotationPackageName("com.wordnik.swagger.annotations");

    //create ouput directories
    FileUtil.createOutputDirectories(config.getModelClassLocation(), config.getClassFileExtension());
    FileUtil.createOutputDirectories(config.getResourceClassLocation(), config.getClassFileExtension());
    FileUtil.clearFolder(config.getModelClassLocation());
    FileUtil.clearFolder(config.getResourceClassLocation());
    FileUtil.clearFolder(config.getLibraryHome() + "/src/main/java/com/wordnik/swagger/runtime");
    FileUtil.createOutputDirectories(config.getLibraryHome() + "/src/main/java/com/wordnik/swagger/runtime", "java");
    FileUtil.copyDirectoryFromUrl(this.getClass.getClassLoader.getResource("conf/scala/structure/src/main/java"), new File(config.getLibraryHome() + "/src/main/java"));
    config
  }

}
