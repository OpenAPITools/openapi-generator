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

package com.wordnik.swagger.codegen.config.python;

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
public class PythonLibCodeGen extends LibraryCodeGenerator {

    public static void main(String[] args) {
        if(args.length < 1){
            throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for config json");
        }
        if(args.length == 1) {
            String configPath = args[0];
            PythonLibCodeGen codeGenerator = new PythonLibCodeGen(configPath);
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
            PythonLibCodeGen codeGenerator = new PythonLibCodeGen(apiServerURL, apiKey, modelPackageName,
                    apiPackageName, classOutputDir, libraryHome);
            codeGenerator.generateCode();
        }

    }

    public PythonLibCodeGen(String apiServerURL, String apiKey, String modelPackageName, String apiPackageName,
                          String classOutputDir, String libraryHome){
        super(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome);
        this.setDataTypeMappingProvider(new PythonDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    public PythonLibCodeGen(String configPath){
        super(configPath);
        this.setDataTypeMappingProvider(new PythonDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    @Override
    protected LanguageConfiguration initializeLangConfig(LanguageConfiguration PythonConfiguration) {
        PythonConfiguration.setClassFileExtension(".py");
        PythonConfiguration.setTemplateLocation("conf/python/templates");
        PythonConfiguration.setStructureLocation("conf/python/structure");
        PythonConfiguration.setExceptionPackageName("com.wordnik.swagger.exception");
        PythonConfiguration.setAnnotationPackageName("com.wordnik.swagger.annotations");

        //create ouput directories
        FileUtil.createOutputDirectories(PythonConfiguration.getModelClassLocation(), PythonConfiguration.getClassFileExtension());
        FileUtil.createOutputDirectories(PythonConfiguration.getResourceClassLocation(), PythonConfiguration.getClassFileExtension());
        FileUtil.clearFolder(PythonConfiguration.getModelClassLocation());
        FileUtil.clearFolder(PythonConfiguration.getResourceClassLocation());
        FileUtil.copyDirectoryFromUrl(this.getClass().getClassLoader().getResource(PythonConfiguration.getStructureLocation()), new File(PythonConfiguration.getResourceClassLocation()));

        File initFile = new File(PythonConfiguration.getResourceClassLocation() + "__init__.py");
        File newInitFile = new File(PythonConfiguration.getModelClassLocation() + "__init__.py");
        initFile.renameTo(newInitFile);
                // try {
                //     initFile.createNewFile();
                // } catch (java.io.IOException e) {
                //     e.printStackTrace();
                //     throw new CodeGenerationException("Creating model/__init__.py failed");
                // }
        return PythonConfiguration;
    }

}
