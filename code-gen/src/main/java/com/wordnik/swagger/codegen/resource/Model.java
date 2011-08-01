package com.wordnik.swagger.codegen.resource;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: ramesh
 * Date: 3/31/11
 * Time: 8:31 AM
 * To change this template use File | Settings | File Templates.
 */
public class Model {

	public static String INPUT_OBJECT_SUFFIX = "Input";
	
    private String name;
    
    private String description;

    private List<ModelField> fields;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<ModelField> getFields() {
		return fields;
	}

	public void setFields(List<ModelField> fields) {
		this.fields = fields;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
    
    public String getGenratedClassName() {
    	return name.substring(0,1).toUpperCase() + name.substring(1);
    }
}
