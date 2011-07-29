package com.wordnik.codegen.config.java;

import com.wordnik.codegen.config.GenerationEnvironmentConfig;
import com.wordnik.exception.CodeGenerationException;

/**
 * User: deepakmichael
 * Date: 23/07/11
 * Time: 8:09 AM
 */
public class JavaGenEnvConfig extends GenerationEnvironmentConfig{

    public JavaGenEnvConfig(String outputPath) {
        if(outputPath == null){
            throw new CodeGenerationException("Error creating output path : Output path was null ");
        }

        outputPath = outputPath.endsWith("/") ? outputPath.substring(0, outputPath.lastIndexOf("/")) : outputPath;

        this.setModelClassLocation(outputPath + "/model/");
        this.setResourceClassLocation(outputPath + "/api/");
        this.setTemplateLocation("conf/templates/java");
    }
}
