package com.wordnik.swagger.codegen.config.java;

import com.wordnik.swagger.codegen.LibraryCodeGenerator;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.exception.CodeGenerationException;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * User: ramesh
 * Date: 6/16/11
 * Time: 1:31 PM
 */
public class JavaLibCodeGen extends LibraryCodeGenerator {

    public static void main(String[] args) {
        if(args.length < 3){
            throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for output path");
        }

        String outputPath = args[0];
        String configPath = args[1];
        String rulesConfigPath = args[2];
        JavaLibCodeGen codeGenerator = new JavaLibCodeGen(outputPath, configPath, rulesConfigPath);
        codeGenerator.generateCode();
    }
    
    public JavaLibCodeGen(String outputPath, String configPath, String rulesConfigPath){

        this.setApiConfig(readApiConfiguration(configPath));
        this.setCodeGenRulesProvider(readRulesProviderConfig(rulesConfigPath));
        this.setLanguageConfig(initializeLangConfig(outputPath));

        this.setDataTypeMappingProvider(new JavaDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    private JavaCodeGenRulesProvider readRulesProviderConfig(String rulesProviderLocation) {
        ObjectMapper mapper = new ObjectMapper();
        JavaCodeGenRulesProvider javaCodeGenRules = null;
        try {
            javaCodeGenRules = mapper.readValue(new File(rulesProviderLocation), JavaCodeGenRulesProvider.class);
        } catch (IOException e) {
            e.printStackTrace();
            throw new CodeGenerationException("Java codegen rules configuration could not be read from the location : " + rulesProviderLocation);
        }

        return javaCodeGenRules;
    }

    private ApiConfiguration readApiConfiguration(String apiConfigLocation) {
        ObjectMapper mapper = new ObjectMapper();
        ApiConfiguration configuration = null;
        try {
            configuration = mapper.readValue(new File(apiConfigLocation), ApiConfiguration.class);
        } catch (IOException e) {
            e.printStackTrace();
            throw new CodeGenerationException("Api configuration could not be read from the location : " + apiConfigLocation);
        }

        return configuration;
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
