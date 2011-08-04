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
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.codegen.util.FileUtil;
import com.wordnik.swagger.exception.CodeGenerationException;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.File;
import java.io.FilenameFilter;
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

        final ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);

        final File configFile = new File(configPath);
        this.setApiConfig(readApiConfiguration(configPath, mapper, configFile));
        this.setCodeGenRulesProvider(readRulesProviderConfig(configPath, mapper, configFile));
        this.setLanguageConfig( initializeLangConfig(readLanguageConfiguration(configPath, mapper, configFile)) );

        this.setDataTypeMappingProvider(new JavaDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    private JavaCodeGenRulesProvider readRulesProviderConfig(String rulesProviderLocation, ObjectMapper mapper, File configFile) {
        JavaCodeGenRulesProvider javaCodeGenRules = null;
        try {
            javaCodeGenRules = mapper.readValue(configFile, JavaCodeGenRulesProvider.class);
        } catch (IOException e) {
            throw new CodeGenerationException("Java codegen rules configuration could not be read from the location : " + rulesProviderLocation);
        }

        return javaCodeGenRules;
    }

    private ApiConfiguration readApiConfiguration(String apiConfigLocation, ObjectMapper mapper, File configFile) {
        ApiConfiguration configuration = null;
        try {
            configuration = mapper.readValue(configFile, ApiConfiguration.class);
        } catch (IOException e) {
            throw new CodeGenerationException("Api configuration could not be read from the location : " + apiConfigLocation);
        }

        return configuration;
    }

    private LanguageConfiguration readLanguageConfiguration(String langConfigLocation, ObjectMapper mapper, File configFile) {
        LanguageConfiguration langConfig = null;
        try {
            langConfig = mapper.readValue(configFile, LanguageConfiguration.class);
        } catch (IOException e) {
            throw new CodeGenerationException("Language configuration value could not be read from the location : " + langConfigLocation);
        }

        return langConfig;
    }

    private LanguageConfiguration initializeLangConfig(LanguageConfiguration javaConfiguration) {
        javaConfiguration.setClassFileExtension(".java");
        javaConfiguration.setTemplateLocation("conf/java/templates");
        javaConfiguration.setStructureLocation("conf/java/structure");
        javaConfiguration.setExceptionPackageName("com.wordnik.swagger.exception");
        javaConfiguration.setAnnotationPackageName("com.wordnik.swagger.annotations");

        //create ouput directories
        FileUtil.createOutputDirectories(javaConfiguration.getModelClassLocation(), javaConfiguration.getClassFileExtension());
        FileUtil.createOutputDirectories(javaConfiguration.getResourceClassLocation(), javaConfiguration.getClassFileExtension());
        FileUtil.cleanFiles(javaConfiguration.getApiServerRootLocation()+ "/src/main/java/com/wordnik/swagger/common");
        FileUtil.cleanFiles(javaConfiguration.getApiServerRootLocation()+ "/src/main/java/com/wordnik/swagger/exception");
        FileUtil.cleanFiles(javaConfiguration.getApiServerRootLocation()+ "/src/main/java/com/wordnik/swagger/annotations");
        FileUtil.copyDirectory(new File(javaConfiguration.getStructureLocation()), new File(javaConfiguration.getApiServerRootLocation()));
        return javaConfiguration;
    }

}
