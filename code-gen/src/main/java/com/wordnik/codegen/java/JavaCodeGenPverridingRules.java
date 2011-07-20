package com.wordnik.codegen.java;

import com.wordnik.codegen.config.CodeGenOverridingRules;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * User: ramesh
 * Date: 5/31/11
 * Time: 7:04 AM
 */
public class JavaCodeGenPverridingRules implements CodeGenOverridingRules {

    private Map<String, String> extendedClassNames = new HashMap<String, String>();
    private List<String> ignoreMethods = new ArrayList<String>();
    private List<String> ignoreModels = new ArrayList<String>();

    public JavaCodeGenPverridingRules() {
        extendedClassNames.put("WordAPI","AbstractWordAPI");
        ignoreMethods.add("WordAPI.getWordFrequency");
        ignoreMethods.add("WordAPI.getAudio");
        ignoreMethods.add("WordAPI.getWordStats");
        ignoreModels.add("wordStats");
    }

    public String getServiceExtendingClass(String serviceName) {
        if(extendedClassNames.containsKey(serviceName)){
            return extendedClassNames.get(serviceName);
        }
        return "WordnikAPI";
    }

    public boolean isMethodIgnored(String serviceName, String methodName){
        return (ignoreMethods.contains(serviceName+"."+methodName));
    }

    public boolean isModelIgnored(String modelName) {
        return ignoreModels.contains(modelName);
    }

}
