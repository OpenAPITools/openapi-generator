package com.wordnik.swagger.codegen.config.java;

import com.wordnik.swagger.codegen.LibraryCodeGenerator;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.exception.CodeGenerationException;

import java.util.ArrayList;
import java.util.List;

/**
 * User: ramesh
 * Date: 6/16/11
 * Time: 1:31 PM
 */
public class JavaLibCodeGen extends LibraryCodeGenerator {

    public static void main(String[] args) {
        if(args.length < 1){
            throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for output path");
        }

        String outputPath = args[0];
        JavaLibCodeGen codeGenerator = new JavaLibCodeGen(outputPath);
        codeGenerator.generateCode();
    }
    
    public JavaLibCodeGen(String outputPath){

        this.setApiConfig(initializeApiConfig());
        this.setLanguageConfig(initializeLangConfig(outputPath));

        this.setDataTypeMappingProvider(new JavaDataTypeMappingProvider());
        this.setCodeGenRulesProvider(new JavaCodeGenRulesProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    private ApiConfiguration initializeApiConfig() {
        ApiConfiguration apiConfiguration = new ApiConfiguration();
        apiConfiguration.setServiceBaseClass("WordAPI","AbstractWordAPI");
        //default base class for services if not specified for a service
        apiConfiguration.setServiceBaseClass("WordnikAPI");
        apiConfiguration.setModelBaseClass("WordnikObject");

        List<String> defaultModelImports = new ArrayList<String>();
        defaultModelImports.add("com.wordnik.swagger.common.WordListType");
        defaultModelImports.add("com.wordnik.swagger.common.StringValue");
        defaultModelImports.add("com.wordnik.swagger.common.Size");
        defaultModelImports.add("com.wordnik.swagger.common.WordnikObject");

        List<String> defaultServiceImports = new ArrayList<String>();
        defaultServiceImports.add("com.wordnik.swagger.model.Long");
        defaultServiceImports.add("com.wordnik.swagger.common.*");
        defaultServiceImports.add("com.wordnik.swagger.common.ext.*");

        apiConfiguration.setDefaultModelImports(defaultModelImports);
        apiConfiguration.setDefaultServiceImports(defaultServiceImports);

        apiConfiguration.setModelPackageName("com.wordnik.swagger.model");
        apiConfiguration.setApiPackageName("com.wordnik.swagger.api");

        apiConfiguration.setApiKey("myKey");
        apiConfiguration.setApiUrl("http://swagr.api.wordnik.com/v4/");
        apiConfiguration.setApiListResource("/list");

        return apiConfiguration;
    }

    private LanguageConfiguration initializeLangConfig(String outputPath) {
        LanguageConfiguration javaConfiguration = new LanguageConfiguration();
        javaConfiguration.setClassFileExtension(".java");
        javaConfiguration.setOutputDirectory(outputPath);
        javaConfiguration.setTemplateLocation("conf/java/templates");
        javaConfiguration.setExceptionPackageName("com.wordnik.swagger.exception");
        javaConfiguration.setAnnotationPackageName("com.wordnik.swagger.annotations");
        return javaConfiguration;
    }

}
