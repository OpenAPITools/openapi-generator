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

package com.wordnik.swagger.codegen.config.ruby

import com.wordnik.swagger.codegen.LibraryCodeGenerator
import com.wordnik.swagger.codegen.config.LanguageConfiguration
import com.wordnik.swagger.codegen.config.common.UnderscoreNamingPolicyProvider
import com.wordnik.swagger.codegen.exception.CodeGenerationException
import com.wordnik.swagger.codegen.util.FileUtil
import java.io.File
import com.wordnik.swagger.codegen.resource.EndpointOperation

object RubyLibCodeGen {
  def main(args: Array[String]) = {
    val codeGenerator = args.length match {
      case 1 => new RubyLibCodeGen(args(0))
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
        val modelPackageName = packageName + ".models"
        val apiPackageName = packageName + ".resources"
        val classOutputDir = libraryHome + "/src/main/ruby/lib/" + packageName.replace(".", "/")
        new RubyLibCodeGen(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome)
      }
      case _ => throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for config json")
    }
    codeGenerator.generateCode()
  }
}

class RubyLibCodeGen (
  apiServerURL: String,
  apiKey: String,
  modelPackageName: String,
  apiPackageName: String,
  classOutputDir: String,
  libraryHome: String,
  configPath: String) extends LibraryCodeGenerator {
  
  //	don't want to generate input models
  EndpointOperation.setArgCountForInputModel(10000)

  if (null != configPath) {
    initializeWithConfigPath(configPath)
    this.setDataTypeMappingProvider(new RubyDataTypeMappingProvider())
    this.setNameGenerator(new UnderscoreNamingPolicyProvider())
  }
  else{
    initialize(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome)
    setDataTypeMappingProvider(new RubyDataTypeMappingProvider())
    setNameGenerator(new UnderscoreNamingPolicyProvider())
  }

  def this(apiServerURL: String, apiKey: String, modelPackageName: String, apiPackageName: String, classOutputDir: String, libraryHome: String) = this(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome, null)
  def this(configPath: String) = this(null, null, null, null, null, null, configPath)

  override def initializeLangConfig(config: LanguageConfiguration): LanguageConfiguration = {
    config.setClassFileExtension(".rb");
    config.setTemplateLocation("conf/ruby/templates");
    config.setStructureLocation("conf/ruby/structure");
    config.setExceptionPackageName("com.wordnik.swagger.exception");
    config.setAnnotationPackageName("com.wordnik.swagger.annotations");
    
    config.setOutputDirectory(classOutputDir, "models", "resources");

    //create ouput directories
    FileUtil.createOutputDirectories(config.getModelClassLocation(), config.getClassFileExtension());
    FileUtil.createOutputDirectories(config.getResourceClassLocation(), config.getClassFileExtension());
    FileUtil.clearFolder(config.getModelClassLocation());
    FileUtil.clearFolder(config.getResourceClassLocation());
    FileUtil.clearFolder(config.getLibraryHome() + "/src/main/ruby/com/wordnik/swagger/runtime");
    FileUtil.createOutputDirectories(config.getLibraryHome() + "/src/main/ruby/lib", "ruby");
    FileUtil.copyDirectoryFromUrl(this.getClass.getClassLoader.getResource(config.getStructureLocation()), new File(classOutputDir));
    config
  }
}
