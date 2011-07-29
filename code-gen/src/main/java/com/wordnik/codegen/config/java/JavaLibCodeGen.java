package com.wordnik.codegen.config.java;

import com.wordnik.codegen.DriverCodeGenerator;
import com.wordnik.exception.CodeGenerationException;

/**
 * User: ramesh
 * Date: 6/16/11
 * Time: 1:31 PM
 */
public class JavaLibCodeGen extends DriverCodeGenerator {

    public static void main(String[] args) {
        if(args.length < 1){
            throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for output path");
        }

        String outputPath = args[0];
        JavaLibCodeGen codeGenerator = new JavaLibCodeGen(outputPath);
        codeGenerator.generateCode();
    }
    
    public JavaLibCodeGen(String outputPath){
        this.setConfig(new JavaCodeGenConfig());
        this.setEnvConfig(new JavaGenEnvConfig(outputPath));
    }
}
