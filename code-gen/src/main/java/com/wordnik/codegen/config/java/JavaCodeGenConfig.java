package com.wordnik.codegen.config.java;

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
        defaultServiceImports.add("com.wordnik.common.*");
        defaultServiceImports.add("com.wordnik.common.ext.*");
        this.setDefaultModelImports(defaultModelImports);
        this.setDefaultServiceImports(defaultServiceImports);
        this.setModelPackageName("com.wordnik.model");
        this.setApiPackageName("com.wordnik.api");
        this.setExceptionPackageName("com.wordnik.exception");
        this.setAnnotationPackageName("com.wordnik.annotations");
        this.setCodeGenOverridingRules(new JavaCodeGenPverridingRules());
        this.setDataTypeMapper(new JavaDataTypeMapper());
        this.setNameGenerator(new JavaServiceAndMethodNameGenerator());
    }
}
