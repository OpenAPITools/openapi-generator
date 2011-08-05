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

package com.wordnik.swagger.codegen.config.java;

import com.wordnik.swagger.codegen.LibraryCodeGenerator;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.CodeGenRulesProvider;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.codegen.util.FileUtil;
import com.wordnik.swagger.exception.CodeGenerationException;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.File;
import java.io.IOException;

/**
 * User: ramesh
 * Date: 6/16/11
 * Time: 1:31 PM
 */
public class JavaLibCodeGen extends LibraryCodeGenerator {

    public static void main(String[] args) {
        if(args.length < 1){
            throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for config json");
        }
        String configPath = args[0];
        JavaLibCodeGen codeGenerator = new JavaLibCodeGen(configPath);
        codeGenerator.generateCode();
    }

    public JavaLibCodeGen(String configPath){
        super(configPath);
        this.setDataTypeMappingProvider(new JavaDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    @Override
    protected LanguageConfiguration initializeLangConfig(LanguageConfiguration javaConfiguration) {
        javaConfiguration.setClassFileExtension(".java");
        javaConfiguration.setTemplateLocation("conf/java/templates");
        javaConfiguration.setStructureLocation("conf/java/structure");
        javaConfiguration.setExceptionPackageName("com.wordnik.swagger.exception");
        javaConfiguration.setAnnotationPackageName("com.wordnik.swagger.annotations");

        //create ouput directories
        FileUtil.createOutputDirectories(javaConfiguration.getModelClassLocation(), javaConfiguration.getClassFileExtension());
        FileUtil.createOutputDirectories(javaConfiguration.getResourceClassLocation(), javaConfiguration.getClassFileExtension());
        FileUtil.clearFolder(javaConfiguration.getLibraryHome() + "/src/main/java/com/wordnik/swagger/common");
        FileUtil.clearFolder(javaConfiguration.getLibraryHome() + "/src/main/java/com/wordnik/swagger/exception");
        FileUtil.clearFolder(javaConfiguration.getLibraryHome() + "/src/main/java/com/wordnik/swagger/annotations");
        FileUtil.copyDirectory(new File(javaConfiguration.getStructureLocation()), new File(javaConfiguration.getLibraryHome()));
        return javaConfiguration;
    }

}
