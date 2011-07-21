package com.wordnik.codegen.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Maintains the overriding rules that we should use while generating the code.
 *
 * Example; If we need to ignore any REST methods or if we need special service extention classes they can be
 * supplied through this configuration
 *
 * User: ramesh
 * Date: 4/26/11
 * Time: 8:01 AM
 */
public interface CodeGenOverridingRules {

    public String getServiceExtendingClass(String serviceName);

    public String getModelExtendingClass();

    public boolean isMethodIgnored(String serviceName, String methodName);

    public boolean isModelIgnored(String modelName);
}
