package com.wordnik.swagger.codegen;

import java.util.ArrayList;
import java.util.List;

public class FieldDefinition {

	private String returnType;
	
	private String name;
	
	private String initialization;
	
	private List<String> importDefinitions = new ArrayList<String>();
	
	public String getReturnType() {
		return returnType;
	}
	
	public void setReturnType(String returnType) {
		this.returnType = returnType;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public String getInitialization() {
		return initialization;
	}
	
	public void setInitialization(String initialization) {
		this.initialization = initialization;
	}

	public List<String> getImportDefinitions() {
		return importDefinitions;
	}

    public String getNameForMethod() {
        return name.substring(0,1).toUpperCase() + name.substring(1);
    }


}
