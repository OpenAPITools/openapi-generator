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
package com.wordnik.swagger.codegen.config.csharp;

import com.wordnik.swagger.codegen.exception.CodeGenerationException;
import com.wordnik.swagger.codegen.config.NamingPolicyProvider;

/**
 * User: marek-stoj
 * Date: 5/11/12
 * Time: 5:56 PM
 */
public class CSharpNamingPolicyProvider implements NamingPolicyProvider {

  public static String INPUT_OBJECT_SUFFIX = "Input";

  /**
   * gets the name of class that is responsible for tracking current library version
   * @return
   */
  @Override
  public String getVersionCheckerClassName() {
    return "VersionChecker";
  }

  /**
   * Converts the first character of the input into upper case .
   * Example: If the input is word, the return value will be Word
   * @param input
   * @return
   */
  @Override
  public String applyClassNamingPolicy(String input) {
    if (input != null && input.length() > 0) {
      if ("string".equalsIgnoreCase(input)) {
        return "string";
      }
      
      String output = input.substring(0, 1).toUpperCase() + input.substring(1);
      // class name can't have . so if dot exists remove the same
      output = output.replace(".", "");
      
      return output;
    }
    else {
      throw new CodeGenerationException("Error converting input to first letter caps becuase of null or empty input");
    }
  }

  /**
   * Converts the first character of the input into upper case.
   * Example: If the input is getWord, the return value will be GetWord
   * @param input
   * @return
   */
  @Override
  public String applyMethodNamingPolicy(String input) {
    if (input != null && input.length() > 0) {
      return input.substring(0, 1).toUpperCase() + input.substring(1);
    }
    else {
      throw new CodeGenerationException("Error converting input to first letter to upper because of null or empty input");
    }
  }

  /**
   * Generate name of the service from resource path.
   *
   * Example: if input is /user.json the generated name for this path will be UserAPI
   * If the input is /user.json/{userId}, the service name will still be generated as UserAPI
   *
   * @param resourcePath
   * @return
   */
  @Override
  public String getServiceName(String resourcePath) {
    String className = null;
    int index = resourcePath.indexOf(".");
    
    if (index >= 0) {
      String resourceName = resourcePath.substring(1, index);
      
      className = applyClassNamingPolicy(resourceName);
    }
    else {
      String[] paths = resourcePath.split("/");
      
      for (String path : paths) {
        if (path != null && path.length() > 0) {
          className = applyClassNamingPolicy(path);
          break;
        }
      }
    }
    
    return className + "Api";
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
  @Override
  public String getMethodName(String endPoint, String suggestedName) {
    return suggestedName;
  }

  /**
   * For input UserAPI and resource path /findUserById the suggested input object name will be: UserFindUserByIdInput
   *
   * If the input path is /{userId}/delete the suggested name will be UserDeleteInput. The path parameters are ignored
   * in generating the input object name
   *
   * Note: Input objects are only created when the number of input arguments in a method exceeds certain number so <br/> that the method signatures are clean
   *
   * 
   * @param serviceName
   * @param resourcePath
   * @return
   */
  @Override
  public String getInputObjectName(String serviceName, String resourcePath) {
    // Since service name has Api at the end remove that format he name
    String inputobjectName = serviceName.substring(0, serviceName.length() - 3);
    String[] pathElements = resourcePath.split("/");
    StringBuilder urlPath = new StringBuilder("");
    
    if (pathElements != null) {
      for (int i = 0; i < pathElements.length; i++) {
        String pathElement = pathElements[i];
        
        if (pathElement != null && pathElement.length() > 0) {
          int position = pathElement.indexOf("{");
          
          if (position < 0) {
            inputobjectName = inputobjectName + applyClassNamingPolicy(pathElement) + INPUT_OBJECT_SUFFIX;
          }
        }
      }
    }
    return inputobjectName;
  }

  /**
   * Generate the name of the wrapper class used as a wrapper for a list of items returned by a service
   * <p/>
   * Example: get definitions API returns a list of definition objects as the result. This will be wrapped by an
   * object. The object's name will be determined by invoking this service.
   * eg. DefinitionList for a wrapper of 'definition's
   *
   * @param wrapperItemName
   * @return
   */
  @Override
  public String getListWrapperName(String wrapperItemName) {
    return applyClassNamingPolicy(wrapperItemName) + "List";
  }

  /**
   * Generates a name for an enum for the param or field name.
   * <p/>
   * Example: for a param source the return could be SourceEnum
   *
   * @param input
   * @return
   */
  @Override
  public String getEnumName(String input) {
    if (input != null && input.length() > 0) {
      return this.applyClassNamingPolicy(input).concat("Values");
    }
    else {
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
  @Override
  public String createGetterMethodName(String className, String attributeName) {
    return className + "." + applyClassNamingPolicy(attributeName);
  }
  
}
