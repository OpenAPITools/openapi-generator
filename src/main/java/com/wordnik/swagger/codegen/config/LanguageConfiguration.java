package com.wordnik.swagger.codegen.config;

import com.wordnik.swagger.exception.CodeGenerationException;

/**
 * User: deepakmichael
 * Date: 23/07/11
 * Time: 8:01 AM
 */
public class LanguageConfiguration {

    private String classFileExtension;

    private String templateLocation;

    private String modelClassLocation;

    private String resourceClassLocation;

    private String exceptionPackageName;

    private String annotationPackageName;

    public String getClassFileExtension() {
        return classFileExtension;
    }

    public void setClassFileExtension(String classFileExtension) {
        this.classFileExtension = classFileExtension;
    }

    public String getTemplateLocation() {
        return templateLocation;
    }

    public void setTemplateLocation(String templateLocation) {
        this.templateLocation = templateLocation;
    }

    public void setOutputDirectory(String outputDirectory) {

        if(outputDirectory == null || outputDirectory.length() == 0){
            throw new CodeGenerationException("Error creating output path : Output path was null ");
        }
        outputDirectory = outputDirectory.endsWith("/") ? outputDirectory.substring(0, outputDirectory.lastIndexOf("/")) : outputDirectory;


        this.modelClassLocation = outputDirectory + "/model/";
        this.resourceClassLocation = outputDirectory + "/api/";
    }

    public String getModelClassLocation() {
        return modelClassLocation;
    }

    public String getResourceClassLocation() {
        return resourceClassLocation;
    }

    public String getExceptionPackageName() {
        return exceptionPackageName;
    }

    public void setExceptionPackageName(String exceptionPackageName) {
        this.exceptionPackageName = exceptionPackageName;
    }

    public String getAnnotationPackageName() {
        return annotationPackageName;
    }

    public void setAnnotationPackageName(String annotationPackageName) {
        this.annotationPackageName = annotationPackageName;
    }



}
