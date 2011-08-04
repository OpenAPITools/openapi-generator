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

package com.wordnik.swagger.codegen.config;

import com.wordnik.swagger.exception.CodeGenerationException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * User: ramesh
 * Date: 5/31/11
 * Time: 7:04 AM
 */
public class ApiConfiguration {

    private Map<String, String> baseClassNames = new HashMap<String, String>();

    private String defaultServiceBaseClass = "Object";

    private String modelBaseClass = "Object";
    /**
     *  Default model imports that we need to include in all service classes. This is needed because some times,
     *  we may need to write custom classes and those classes will not be known to code generation. To import those
     *  classes in service classes we use this property
     */
    private List<String> defaultModelImports;
    /**
     *  Default service imports that we need to include in all service classes. This is needed because some times,
     *  we may need to write custom classes ans those classes will not be known to code generation. To import those
     *  classes in service classes we use this property
     */
    private List<String> defaultServiceImports;
    private String modelPackageName;
    private String apiPackageName;

    private String apiUrl;
    private String apiKey;
    private String apiListResource;

    public ApiConfiguration() {

    }

    public String getDefaultServiceBaseClass() {
        return defaultServiceBaseClass;
    }

    public void setDefaultServiceBaseClass(String defaultServiceBaseClass) {
        this.defaultServiceBaseClass = defaultServiceBaseClass;
    }

    public Map<String, String> getBaseClassNames() {

        return baseClassNames;
    }

    public void setBaseClassNames(Map<String, String> baseClassNames) {
        this.baseClassNames = baseClassNames;
    }


    public void setServiceBaseClass(String defaultServiceBaseClass) {
        this.defaultServiceBaseClass = defaultServiceBaseClass;
    }

    public void setServiceBaseClass(String serviceName, String className) {
        if(serviceName == null || serviceName.length() == 0){
            throw new CodeGenerationException("Error setting base class for service: service name was not provided");
        }

        if(className == null || className.length() == 0) {
            throw new CodeGenerationException("Error settting base class for service: class name was not provided");
        }

        baseClassNames.put(serviceName, className);
    }

    public String getServiceBaseClass(String serviceName) {
        if(baseClassNames.containsKey(serviceName)){
            return baseClassNames.get(serviceName);
        }
        return defaultServiceBaseClass;
    }

    public String getModelBaseClass() {
        return modelBaseClass;
    }

    public void setModelBaseClass(String modelBaseClass) {
        this.modelBaseClass = modelBaseClass;
    }


    public List<String> getDefaultModelImports() {
        return defaultModelImports;
    }

    public void setDefaultModelImports(List<String> defaultModelImports) {
        this.defaultModelImports = defaultModelImports;
    }

    public List<String> getDefaultServiceImports() {
        return defaultServiceImports;
    }

    public void setDefaultServiceImports(List<String> defaultServiceImports) {
        this.defaultServiceImports = defaultServiceImports;
    }

    public String getModelPackageName() {
        return modelPackageName;
    }

    public void setModelPackageName(String modelPackageName) {
        this.modelPackageName = modelPackageName;
    }

    public String getApiPackageName() {
        return apiPackageName;
    }

    public void setApiPackageName(String apiPackageName) {
        this.apiPackageName = apiPackageName;
    }

    public String getApiUrl() {
        return apiUrl;
    }

    public void setApiUrl(String apiUrl) {
        this.apiUrl = apiUrl;
    }

    public String getApiKey() {
        return apiKey;
    }

    public void setApiKey(String apiKey) {
        this.apiKey = apiKey;
    }

    public String getApiListResource() {
        return apiListResource;
    }

    public void setApiListResource(String apiListResource) {
        this.apiListResource = apiListResource;
    }
}
