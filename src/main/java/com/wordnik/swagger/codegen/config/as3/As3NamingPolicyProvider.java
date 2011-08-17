package com.wordnik.swagger.codegen.config.as3;

import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;

/**
 * User: deepakmichael
 * Date: 16/08/11
 * Time: 11:01 AM
 */
public class As3NamingPolicyProvider extends CamelCaseNamingPolicyProvider {

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
