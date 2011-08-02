package com.wordnik.swagger.codegen.config.java;

import com.wordnik.swagger.codegen.config.RulesProvider;

import java.util.ArrayList;
import java.util.List;

/**
 * User: ramesh
 * Date: 5/31/11
 * Time: 7:04 AM
 */
public class JavaCodeGenRulesProvider implements RulesProvider {

    private List<String> ignoreMethods = new ArrayList<String>();
    private List<String> ignoreModels = new ArrayList<String>();

    public JavaCodeGenRulesProvider() {

    }

    public boolean isMethodIgnored(String serviceName, String methodName){
        return (ignoreMethods.contains(serviceName+"."+methodName));
    }

    public boolean isModelIgnored(String modelName) {
        return ignoreModels.contains(modelName);
    }

    public List<String> getIgnoreMethods() {
        return ignoreMethods;
    }

    public void setIgnoreMethods(List<String> ignoreMethods) {
        this.ignoreMethods = ignoreMethods;
    }

    public List<String> getIgnoreModels() {
        return ignoreModels;
    }

    public void setIgnoreModels(List<String> ignoreModels) {
        this.ignoreModels = ignoreModels;
    }

}
