package com.wordnik.codegen.java;

import com.wordnik.codegen.DriverCodeGenerator;

/**
 * User: ramesh
 * Date: 6/16/11
 * Time: 1:31 PM
 */
public class JavaLibCodeGen extends DriverCodeGenerator {

    public static void main(String[] args) {
        JavaLibCodeGen codeGenerator = new JavaLibCodeGen();
        codeGenerator.generateCode();
    }
    
    public JavaLibCodeGen(){
        this.setConfig(new JavaCodeGenConfig());
    }
}
