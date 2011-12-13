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

package com.wordnik.swagger.codegen.resource;

import com.wordnik.swagger.codegen.ResourceMethod;
import com.wordnik.swagger.codegen.config.DataTypeMappingProvider;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
import org.codehaus.jackson.annotate.JsonCreator;
import org.codehaus.jackson.annotate.JsonProperty;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * User: ramesh
 * Date: 3/30/11
 * Time: 7:01 PM
 */
public class Resource {
	private String apiVersion;

    @JsonProperty("swaggerVersion")
    private String swaggerVersion;

    @JsonProperty("resourcePath")
    private String resourcePath;

    @JsonProperty("apis")
    private List<Endpoint> endPoints = new ArrayList<Endpoint>();

    @JsonProperty("models")
    private ApiModelListWrapper modelListWrapper;

    private List<Model> models = new ArrayList<Model>();    
    private String generatedClassName;
    private List<ResourceMethod> methods;

    @JsonCreator
    public Resource() {

    }
    
	public String getApiVersion() {
		return apiVersion;
	}

	public void setApiVersion(String apiVersion) {
		this.apiVersion = apiVersion;
	}

    @JsonProperty("swaggerVersion")
    public String getSwaggerVersion() {
        return swaggerVersion;
    }

    @JsonProperty("swaggerVersion")
    public void setSwaggerVersion(String swaggerVersion) {
        this.swaggerVersion = swaggerVersion;
    }

    @JsonProperty("resourcePath")
    public String getResourcePath() {
        return resourcePath;
    }

    @JsonProperty("resourcePath")
    public void setResourcePath(String resourcePath) {
        this.resourcePath = resourcePath;
    }

    @JsonProperty("apis")
	public List<Endpoint> getEndPoints() {
		return endPoints;
	}

    @JsonProperty("apis")
	public void setEndPoints(List<Endpoint> endPoints) {
		this.endPoints = endPoints;
	}

    @JsonProperty("models")
    public ApiModelListWrapper getModelListWrapper() {
        return modelListWrapper;
    }

    @JsonProperty("models")
    public void setModelListWrapper(ApiModelListWrapper modelListWrapper) {
        this.modelListWrapper = modelListWrapper;
    }

	public List<Model> getModels() {
		return models;
	}

	/*public void setModels(List<Model> models) {
		this.models = models;
	}*/
    
	public String generateClassName(NamingPolicyProvider nameGenerator) {
		if (generatedClassName == null && endPoints.size() > 0) {
			String endPointPath = endPoints.get(0).getPath();
			generatedClassName = nameGenerator.getServiceName(endPointPath);
		}
		return generatedClassName;
	}

	public List<ResourceMethod> generateMethods(Resource resource, DataTypeMappingProvider dataTypeMapper,
                                                NamingPolicyProvider nameGenerator, LanguageConfiguration languageConfig) {
		if(methods == null){
			methods = new ArrayList<ResourceMethod>();
            List<ResourceMethod> newMethods = new ArrayList<ResourceMethod>();
            List<String> endPointMethodNames = new ArrayList<String>();
			if(getEndPoints() != null) {
				for(Endpoint endpoint: getEndPoints()){
                    newMethods = endpoint.generateMethods(resource, dataTypeMapper, nameGenerator, languageConfig);

                    if (!languageConfig.isMethodOverloadingSupported()) {
                        for(ResourceMethod newMethod: newMethods){
                            if(endPointMethodNames.contains( newMethod.getName() )) {
                                endpoint.handleOverloadedMethod(newMethod, endPointMethodNames);
                            }
                            endPointMethodNames.add(newMethod.getName());
                        }
                    }
                    methods.addAll(newMethods);
				}
			}
		}
		return methods;
	}

    public void generateModelsFromWrapper(NamingPolicyProvider nameGenerator) {
        String modelName;
        ApiModelDefn modelDefn;
        if (modelListWrapper != null) {
            for (Map.Entry<String, ApiModelDefn> entry : modelListWrapper.getModelList().entrySet()) {
                modelName = entry.getKey();
                modelDefn = entry.getValue();
                models.add (modelDefn.toModel(modelName, nameGenerator) );
            }
        }
    }
}
