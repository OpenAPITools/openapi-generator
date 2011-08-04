/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.codegen.config.common;

import com.wordnik.swagger.codegen.resource.Model;
import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
import com.wordnik.swagger.exception.CodeGenerationException;

/**
 * User: ramesh
 * Date: 5/31/11
 * Time: 7:03 AM
 */
public class CamelCaseNamingPolicyProvider implements NamingPolicyProvider {

    /**
     * gets the name of class that is responsible for tracking current library version
     * @return
     */
    public String getVersionCheckerClassName() {
        return "VersionChecker";
    }

    /**
     * Converts the first character of the input into string.
     * Example: If the input is word, the return value will be Word
     * @param input
     * @return
     */
    public String applyClassNamingPolicy(String input) {
    	if(input != null && input.length() > 0) {
    		return input.substring(0,1).toUpperCase() + input.substring(1);
    	}else{
    		throw new CodeGenerationException("Error converting input to first letter caps becuase of null input");
    	}
    }

    /**
     * Converts the first character of the input into string.
     * Example: If the input is word, the return value will be Word
     * @param input
     * @return
     */
    public String applyMethodNamingPolicy(String input) {
    	if(input != null && input.length() > 0) {
    		return input.substring(0,1).toLowerCase() + input.substring(1);
    	}else{
    		throw new CodeGenerationException("Error converting input to first letter to lower because of null input");
    	}
    }

     public String getServiceName(String resourcePath) {
         String className = null;
         int index = resourcePath.indexOf(".");
         if(index >= 0) {
             String resourceName = resourcePath.substring(1,index);
             className = applyClassNamingPolicy(resourceName);
         }else{
             String[] paths = resourcePath.split("/");
             for(String path : paths) {
                 if(path != null && path.length() > 0) {
                     className = applyClassNamingPolicy(path);
                     break;
                 }
             }
         }
         return className+ "API";
     }

    /**
     * Generates the name of service methods.
     *
     * Resource documentation provides suggested names. Individual language can choose to use suggested name or
     * generate the name based on end point path. Example: IN java we use suggested name
     *
     * @param endPoint
     * @param suggestedName
     * @return
     */
    public String getMethodName(String endPoint, String suggestedName) {
        return suggestedName;
    }


    public String getInputObjectName(String serviceName, String resourcePath) {

        String inputobjectName = serviceName.substring(0, serviceName.length() - 3);

        String[] pathElements = resourcePath.split("/");
        StringBuilder urlPath = new StringBuilder("");
        if(pathElements != null){
            for(int i=0; i < pathElements.length; i++){
                String pathElement  = pathElements[i];
                if(pathElement != null && pathElement.length() > 0) {
                    int position = pathElement.indexOf("{");
                    if(position < 0) {
                        inputobjectName = inputobjectName + applyClassNamingPolicy(pathElement) + Model.INPUT_OBJECT_SUFFIX;
                    }
                }
            }
        }
        return inputobjectName;
    }

    /**
     * Generates a name for an enum for the param or field name.
     * <p/>
     * Example: for a param source the return could be SourceEnum
     *
     * @param input
     * @return
     */
    public String getEnumName(String input) {
        if (input != null && input.length() > 0) {
            return this.applyClassNamingPolicy(input).concat("Values");
        } else {
            throw new CodeGenerationException("Error getting Enum name becuase of null input");
        }
    }

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
        return className+".get"+ applyClassNamingPolicy(attributeName)+"()";
    }

}
