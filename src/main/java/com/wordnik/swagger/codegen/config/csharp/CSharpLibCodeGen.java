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
package com.wordnik.swagger.codegen.config.csharp;

import com.wordnik.swagger.codegen.LibraryCodeGenerator;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.exception.CodeGenerationException;
import com.wordnik.swagger.codegen.util.FileUtil;

import java.io.File;

/**
 * User: marek-stoj
 * Date: 5/11/12
 * Time: 5:56 PM
 */
public class CSharpLibCodeGen extends LibraryCodeGenerator {

  public static void main(String[] args) {
    if (args.length < 1) {
      throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for config json");
    }
    
    if (args.length == 1) {
      String configPath = args[0];
      CSharpLibCodeGen codeGenerator = new CSharpLibCodeGen(configPath);
      
      codeGenerator.generateCode();
    }
    
    if (args.length == 4) {
      String apiServerURL = args[0];
      
      if (!apiServerURL.endsWith("/")) {
        apiServerURL = apiServerURL + "/";
      }
      
      String apiKey = args[1];
      String packageName = args[2];
      String libraryHome = args[3];
      
      if (libraryHome.endsWith("/")) {
        libraryHome = libraryHome.substring(0, libraryHome.length() - 1);
      }
      
      String modelPackageName = packageName + ".Model";
      String apiPackageName = packageName + ".Api";
      String classOutputDir = libraryHome + "/" + packageName.replace(".", "/");
      
      CSharpLibCodeGen codeGenerator =
        new CSharpLibCodeGen(
          apiServerURL, 
          apiKey,
          modelPackageName,
          apiPackageName,
          classOutputDir,
          libraryHome);
      
      ApiConfiguration config = codeGenerator.getConfig();
      
      config.setDefaultServiceBaseClass("ApiBase");
      
      codeGenerator.generateCode();
    }
  }

  public CSharpLibCodeGen(String apiServerURL, String apiKey, String modelPackageName, String apiPackageName, String classOutputDir, String libraryHome) {
    super(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, "Model", "Api", libraryHome);
    
    this.setDataTypeMappingProvider(new CSharpDataTypeMappingProvider());
    this.setNameGenerator(new CSharpNamingPolicyProvider());
  }

  public CSharpLibCodeGen(String configPath) {
    super(configPath);
    
    this.setDataTypeMappingProvider(new CSharpDataTypeMappingProvider());
    this.setNameGenerator(new CSharpNamingPolicyProvider());
  }

  @Override
  protected LanguageConfiguration initializeLangConfig(LanguageConfiguration cSharpConfiguration) {
    cSharpConfiguration.setClassFileExtension(".cs");
    cSharpConfiguration.setTemplateLocation("conf/csharp/templates");
    cSharpConfiguration.setStructureLocation("conf/csharp/structure");
    cSharpConfiguration.setExceptionPackageName("Swagger.Exceptions");
    cSharpConfiguration.setAnnotationPackageName("Swagger.Attributes");
    cSharpConfiguration.setMethodOverloadingSupported(true);
    
    FileUtil.createOutputDirectories(cSharpConfiguration.getModelClassLocation(), cSharpConfiguration.getClassFileExtension());
    FileUtil.createOutputDirectories(cSharpConfiguration.getResourceClassLocation(), cSharpConfiguration.getClassFileExtension());
    
    FileUtil.clearFolder(cSharpConfiguration.getModelClassLocation());
    FileUtil.clearFolder(cSharpConfiguration.getResourceClassLocation());

    FileUtil.copyDirectoryFromUrl(
      this.getClass().getClassLoader().getResource(cSharpConfiguration.getStructureLocation()),
      new File(cSharpConfiguration.getResourceClassLocation()).getParentFile().getParentFile());
    
    return cSharpConfiguration;
  }

  @Override
  protected boolean canEnumNameStartsWithNumber() {
    return false;
  }
  
}
