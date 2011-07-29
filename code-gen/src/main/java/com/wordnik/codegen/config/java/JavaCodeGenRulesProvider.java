package com.wordnik.codegen.config.java;

import com.wordnik.codegen.config.RulesProvider;

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
        ignoreMethods.add("WordAPI.getWordFrequency");
        ignoreMethods.add("WordAPI.getAudio");
        ignoreMethods.add("WordAPI.getWordStats");
        ignoreModels.add("wordStats");
    }

    public boolean isMethodIgnored(String serviceName, String methodName){
        return (ignoreMethods.contains(serviceName+"."+methodName));
    }

    public boolean isModelIgnored(String modelName) {
        return ignoreModels.contains(modelName);
    }

}
