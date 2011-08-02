package com.wordnik.swagger.codegen.config.java;

import com.wordnik.swagger.codegen.LibraryCodeGenerator;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
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

        final ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        this.setApiConfig(readApiConfiguration(configPath, mapper));
        this.setCodeGenRulesProvider(readRulesProviderConfig(configPath, mapper));
        this.setLanguageConfig( initializeLangConfig(readLanguageConfiguration(configPath, mapper)) );

        this.setDataTypeMappingProvider(new JavaDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    private JavaCodeGenRulesProvider readRulesProviderConfig(String rulesProviderLocation, ObjectMapper mapper) {
        JavaCodeGenRulesProvider javaCodeGenRules = null;
        try {
            javaCodeGenRules = mapper.readValue(new File(rulesProviderLocation), JavaCodeGenRulesProvider.class);
        } catch (IOException e) {
            throw new CodeGenerationException("Java codegen rules configuration could not be read from the location : " + rulesProviderLocation);
        }

        return javaCodeGenRules;
    }

    private ApiConfiguration readApiConfiguration(String apiConfigLocation, ObjectMapper mapper) {
        ApiConfiguration configuration = null;
        try {
            configuration = mapper.readValue(new File(apiConfigLocation), ApiConfiguration.class);
        } catch (IOException e) {
            throw new CodeGenerationException("Api configuration could not be read from the location : " + apiConfigLocation);
        }

        return configuration;
    }

    private LanguageConfiguration readLanguageConfiguration(String langConfigLocation, ObjectMapper mapper) {
        LanguageConfiguration langConfig = null;
        try {
            langConfig = mapper.readValue(new File(langConfigLocation), LanguageConfiguration.class);
        } catch (IOException e) {
            throw new CodeGenerationException("Language configuration value could not be read from the location : " + langConfigLocation);
        }

        return langConfig;
    }

    private LanguageConfiguration initializeLangConfig(LanguageConfiguration javaConfiguration) {
        javaConfiguration.setClassFileExtension(".java");
        javaConfiguration.setTemplateLocation("conf/java/templates");
        javaConfiguration.setExceptionPackageName("com.wordnik.swagger.exception");
        javaConfiguration.setAnnotationPackageName("com.wordnik.swagger.annotations");
        return javaConfiguration;
    }

}
