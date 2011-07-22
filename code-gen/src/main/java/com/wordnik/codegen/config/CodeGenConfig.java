package com.wordnik.codegen.config;

import java.util.List;

/**
 * Sets the configurations required for the code generation
 *
 * User: ramesh
 * Date: 5/25/11
 * Time: 8:39 AM
 */
public class CodeGenConfig {

    private String templateLocation; //lang config

    private String classFileExtension; //lang config

    private String modelClassLocation; //output config

    private String resourceClassLocation; //output config

    /**
     *  Default model imports that we need to include in all service classes. This is needed because some times,
     *  we may need to write custom classes and those classes will not be known to code generation. To import those
     *  classes in service classes we use this property
     */
    private List<String> defaultModelImports;  //code gen helper config

    /**
     *  Default service imports that we need to include in all service classes. This is needed because some times,
     *  we may need to write custom classes ans those classes will not be known to code generation. To import those
     *  classes in service classes we use this property
     */
    private List<String> defaultServiceImports; //code gen helper config

    private CodeGenOverridingRules codeGenOverridingRules; //code gen helper config

    private DataTypeMapper dataTypeMapper; //code gen helper

    private ServiceAndMethodNameGenerator nameGenerator; //code gen helper

    public String getTemplateLocation() {
        return templateLocation;
    }

    public void setTemplateLocation(String templateLocation) {
        this.templateLocation = templateLocation;
    }

    public String getClassFileExtension() {
        return classFileExtension;
    }

    public void setClassFileExtension(String classFileExtension) {
        this.classFileExtension = classFileExtension;
    }

    public String getModelClassLocation() {
        return modelClassLocation;
    }

    public void setModelClassLocation(String modelClassLocation) {
        this.modelClassLocation = modelClassLocation;
    }

    public String getResourceClassLocation() {
        return resourceClassLocation;
    }

    public void setResourceClassLocation(String resourceClassLocation) {
        this.resourceClassLocation = resourceClassLocation;
    }

    public List<String> getDefaultModelImports() {
        return defaultModelImports;
    }

    public void setDefaultModelImports(List<String> defaultModelImports) {
        this.defaultModelImports = defaultModelImports;
    }

    public List<String> getDefaultServiceImports() {
        return defaultServiceImports;
    }

    public void setDefaultServiceImports(List<String> defaultServiceImports) {
        this.defaultServiceImports = defaultServiceImports;
    }

    public CodeGenOverridingRules getCodeGenOverridingRules() {
        return codeGenOverridingRules;
    }

    public void setCodeGenOverridingRules(CodeGenOverridingRules codeGenOverridingRules) {
        this.codeGenOverridingRules = codeGenOverridingRules;
    }

    public DataTypeMapper getDataTypeMapper() {
        return dataTypeMapper;
    }

    public void setDataTypeMapper(DataTypeMapper dataTypeMapper) {
        this.dataTypeMapper = dataTypeMapper;
    }

    public ServiceAndMethodNameGenerator getNameGenerator() {
        return nameGenerator;
    }

    public void setNameGenerator(ServiceAndMethodNameGenerator nameGenerator) {
        this.nameGenerator = nameGenerator;
    }
}
