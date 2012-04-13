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

import java.util.ArrayList;
import java.util.List;

/**
 * User: ramesh
 * Date: 3/30/11
 * Time: 7:01 PM
 */
public class Endpoint {

    private String path;

    private String description;

    private List<String> pathParameters;

    private List<EndpointOperation> operations;

    private List<ResourceMethod> methods;

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public List<String> getPathParameters() {
		return pathParameters;
	}

	public void setPathParameters(List<String> pathParameters) {
		this.pathParameters = pathParameters;
	}

	public List<EndpointOperation> getOperations() {
		return operations;
	}

	public void setOperations(List<EndpointOperation> operations) {
		this.operations = operations;
	}

    public List<ResourceMethod> generateMethods(Resource resource, DataTypeMappingProvider dataTypeMapper,
                                                NamingPolicyProvider nameGenerator, LanguageConfiguration languageConfig) {
		if(methods == null){
			methods = new ArrayList<ResourceMethod>();
            ResourceMethod newMethod;
            List<String> endPointMethodNames = new ArrayList<String>();
			if(getOperations() != null) {
				for(EndpointOperation operation: getOperations()) {
                    //Note: Currently we are generating methods for depricated APIs also, We should provide this deprecation info on generated APIs also.
					if(areModelsAvailable(operation.getParameters(), resource, dataTypeMapper)) {

                        newMethod = operation.generateMethod(this, resource, dataTypeMapper, nameGenerator);
                        if (!endPointMethodNames.contains(newMethod.getName())) {
                            methods.add(newMethod);
                        }
                        else{
                            //handleOverloadingSupport
                            if(!languageConfig.isMethodOverloadingSupported()){
                                handleOverloadedMethod(newMethod, endPointMethodNames);
                            }
                        }
                        endPointMethodNames.add(newMethod.getName());
					}else{
                        System.out.println("Method not generated for resource " + resource.getResourcePath() + " and method " +
                                operation.getNickname() + " because of un-available model objects specified in the post " );
                    }
				}
			}
		}
		return methods;
	}

    void handleOverloadedMethod(ResourceMethod method, List<String> methods) {
        //handleOverloadingSupport
        int counter = 1;
        String newMethodName;
        boolean generatedNewName = false;
        do{
            newMethodName = method.getName() + counter;
            if (!methods.contains(newMethodName)){
                method.setName(newMethodName);
                generatedNewName = true;
            }
            System.out.println("Handling overloaded method for " + method.getName());
            counter++;

        }while (!generatedNewName);
        System.out.println("Handling overloaded method : New method name - " + method.getName());

    }

    private boolean areModelsAvailable(List<ModelField> modelFields, Resource resource, DataTypeMappingProvider dataTypeMapper) {
        Boolean isParamSetAvailable = true;
        if(modelFields == null) return true;
        for(ModelField modelField : modelFields){
            if (modelField.getParamType().equalsIgnoreCase(EndpointOperation.PARAM_TYPE_BODY) ){
                isParamSetAvailable = false;
                for(Model model : resource.getModels()){
                    if(dataTypeMapper.isPrimitiveType(modelField.getGenericType())){
                        isParamSetAvailable = true;
                        break;
                    }
                    if(model.getName().equalsIgnoreCase(modelField.getGenericType())){
                        isParamSetAvailable = true;
                        break;
                    }
                }
                if(!isParamSetAvailable){
                    return false;
                }
            }
        }
        return isParamSetAvailable;
    }
}
