package com.wordnik.codegen.java;

import com.wordnik.codegen.config.CodeGenConfig;

import java.util.ArrayList;
import java.util.List;

/**
 * User: ramesh
 * Date: 5/31/11
 * Time: 7:04 AM
 */
public class JavaCodeGenConfig  extends CodeGenConfig {

    public JavaCodeGenConfig() {
        this.setClassFileExtension(".java");
        List<String> defaultModelImports = new ArrayList<String>();
        defaultModelImports.add("com.wordnik.common.WordListType");
        defaultModelImports.add("com.wordnik.common.StringValue");
        defaultModelImports.add("com.wordnik.common.Size");
        defaultModelImports.add("com.wordnik.common.WordnikObject");
        List<String> defaultServiceImports = new ArrayList<String>();
        defaultServiceImports.add("com.wordnik.model.Long");
        this.setDefaultModelImports(defaultModelImports);
        this.setDefaultServiceImports(defaultServiceImports);
        this.setCodeGenOverridingRules(new JavaCodeGenPverridingRules());
        this.setDataTypeMapper(new JavaDataTypeMapper());
        this.setNameGenerator(new JavaServiceAndMethodNameGenerator());
    }
}
