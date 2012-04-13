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

package com.wordnik.swagger.codegen.config.php;

import com.wordnik.swagger.codegen.LibraryCodeGenerator;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.codegen.exception.CodeGenerationException;
import com.wordnik.swagger.codegen.util.FileUtil;

import java.io.File;

/**
 * User: russ
 * Date: 9/1/11
 * Time: 11:00 PM
 */
public class PHPLibCodeGen extends LibraryCodeGenerator {

    public static void main(String[] args) {
        if(args.length < 1){
            throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for config json");
        }
        if(args.length == 1) {
            String configPath = args[0];
            PHPLibCodeGen codeGenerator = new PHPLibCodeGen(configPath);
            codeGenerator.generateCode();
        }
        if(args.length == 4) {
            String apiServerURL = args[0];
            if(!apiServerURL.endsWith("/")){
                apiServerURL = apiServerURL + "/";
            }
            String apiKey = args[1];
            String packageName = args[2];
            String libraryHome = args[3];
            if(libraryHome.endsWith("/")){
                libraryHome = libraryHome.substring(0, libraryHome.length()-1);
            }
            String modelPackageName = packageName+".model";
            String apiPackageName = packageName+".api";
            String classOutputDir = libraryHome + '/' + packageName.replace(".","/");
            PHPLibCodeGen codeGenerator = new PHPLibCodeGen(apiServerURL, apiKey, modelPackageName,
                    apiPackageName, classOutputDir, libraryHome);
            codeGenerator.generateCode();
        }

    }

    public PHPLibCodeGen(String apiServerURL, String apiKey, String modelPackageName, String apiPackageName,
                          String classOutputDir, String libraryHome){
        super(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome);
        this.setDataTypeMappingProvider(new PHPDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    public PHPLibCodeGen(String configPath){
        super(configPath);
        this.setDataTypeMappingProvider(new PHPDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    @Override
    protected LanguageConfiguration initializeLangConfig(LanguageConfiguration PHPConfiguration) {
        PHPConfiguration.setClassFileExtension(".php");
        PHPConfiguration.setTemplateLocation("conf/php/templates");
        PHPConfiguration.setStructureLocation("conf/php/structure");
        PHPConfiguration.setExceptionPackageName("com.wordnik.swagger.exception");
        PHPConfiguration.setAnnotationPackageName("com.wordnik.swagger.annotations");

        //create ouput directories
        FileUtil.createOutputDirectories(PHPConfiguration.getModelClassLocation(), PHPConfiguration.getClassFileExtension());
        FileUtil.createOutputDirectories(PHPConfiguration.getResourceClassLocation(), PHPConfiguration.getClassFileExtension());
        FileUtil.clearFolder(PHPConfiguration.getModelClassLocation());
        FileUtil.clearFolder(PHPConfiguration.getResourceClassLocation());
        FileUtil.copyDirectoryFromUrl(this.getClass().getClassLoader().getResource(PHPConfiguration.getStructureLocation()), new File(PHPConfiguration.getResourceClassLocation()));

        return PHPConfiguration;
    }

}
