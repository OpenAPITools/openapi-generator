package com.wordnik.codegen.resource;

import com.wordnik.codegen.Method;
import com.wordnik.codegen.config.CodeGenConfig;

import java.util.ArrayList;
import java.util.List;

/**
 * User: ramesh
 * Date: 3/30/11
 * Time: 7:01 PM
 */
public class Resource {
	
	private String version;
	
    private List<Endpoint> endPoints = new ArrayList<Endpoint>();
    
    private List<Model> models = new ArrayList<Model>();    

    private String generatedClassName;
    
    private List<Method> methods;    
    
	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public List<Endpoint> getEndPoints() {
		return endPoints;
	}

	public void setEndPoints(List<Endpoint> endPoints) {
		this.endPoints = endPoints;
	}

	public List<Model> getModels() {
		return models;
	}

	public void setModels(List<Model> models) {
		this.models = models;
	}
    
	public String generateClassName(CodeGenConfig config) {
		if (generatedClassName == null) {
			String endPointPath = endPoints.get(0).getPath();
			generatedClassName = config.getNameGenerator().getServiceName(endPointPath);
		}	
		return generatedClassName;
	}
	
	public List<Method> generateMethods(Resource resource, CodeGenConfig config) {
		if(methods == null){
			methods = new ArrayList<Method>();
			if(getEndPoints() != null) {
				for(Endpoint endpoint: getEndPoints()){
					methods.addAll(endpoint.generateMethods(resource, config));
				}
			}
		}
		return methods;
	}
	
}
