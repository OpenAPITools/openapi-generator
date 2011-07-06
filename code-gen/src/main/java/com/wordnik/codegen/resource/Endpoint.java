package com.wordnik.codegen.resource;

import com.wordnik.codegen.Method;
import com.wordnik.codegen.config.CodeGenConfig;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: ramesh
 * Date: 3/30/11
 * Time: 7:01 PM
 * To change this template use File | Settings | File Templates.
 */
public class Endpoint {

    private String path;

    private String description;

    private List<String> pathParameters;

    private List<EndpointOperation> operations;

    private List<Method> methods;    
    
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
    
	public List<Method> generateMethods(Resource resource, CodeGenConfig config) {
		if(methods == null){
			methods = new ArrayList<Method>();
			if(getOperations() != null) {
				for(EndpointOperation operation: getOperations()) {
					if(!operation.isDeprecated()) {
						methods.add(operation.generateMethod(this, resource, config));
					}
				}
			}
		}
		return methods;
	}    
}
