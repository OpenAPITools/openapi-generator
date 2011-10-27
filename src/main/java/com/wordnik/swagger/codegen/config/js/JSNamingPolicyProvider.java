package com.wordnik.swagger.codegen.config.js;

import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;

/**
 * @author ayush
 * @since oct 24 2011
 */
public class JSNamingPolicyProvider extends CamelCaseNamingPolicyProvider {

    /**
     * Gets the signature of the method that gets value for give attribute name.
     *
     * Example: If class name is user and attibute name is email the out in java language will be
     *  <code>user.getEmail()</code>
     *
     * @param className
     * @param attributeName
     * @return
     */
    public String createGetterMethodName(String className, String attributeName) {
        return className+"."+ attributeName;
    }
}
