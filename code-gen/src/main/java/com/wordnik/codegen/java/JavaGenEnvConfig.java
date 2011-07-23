package com.wordnik.codegen.java;

import com.wordnik.codegen.config.GenerationEnvironmentConfig;

/**
 * User: deepakmichael
 * Date: 23/07/11
 * Time: 8:09 AM
 */
public class JavaGenEnvConfig extends GenerationEnvironmentConfig{

    public JavaGenEnvConfig() {
        this.setModelClassLocation("../java/src/main/java/com/wordnik/model/");
        this.setResourceClassLocation("../java/src/main/java/com/wordnik/api/");
        this.setTemplateLocation("conf/templates/java");
    }
}
