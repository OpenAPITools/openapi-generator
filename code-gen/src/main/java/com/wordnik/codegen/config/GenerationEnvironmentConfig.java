package com.wordnik.codegen.config;

/**
 * User: deepakmichael
 * Date: 23/07/11
 * Time: 8:01 AM
 */
public class GenerationEnvironmentConfig {

    private String templateLocation; //lang config

    private String modelClassLocation; //output config

    private String resourceClassLocation; //output config

    public String getTemplateLocation() {
        return templateLocation;
    }

    public void setTemplateLocation(String templateLocation) {
        this.templateLocation = templateLocation;
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

}
