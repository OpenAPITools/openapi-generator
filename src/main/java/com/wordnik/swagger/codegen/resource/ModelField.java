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

import com.wordnik.swagger.codegen.FieldDefinition;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.DataTypeMappingProvider;
import com.wordnik.swagger.codegen.config.NamingPolicyProvider;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * User: ramesh
 * Date: 3/31/11
 * Time: 7:57 AM
 */
public class ModelField {
	
    private String name;

    private String wrapperName;
    
    private String description = "";

    private String defaultValue;

    private boolean required = false;

    private boolean allowMultiple = false;

    private List<String> allowableValues = null;

    private String paramType;

    private String dataType;

    private String internalDescription;

    private String paramAccess;

    private FieldDefinition fieldDefinition;
    
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
    
	public String getWrapperName() {
		return wrapperName;
	}

	public void setWrapperName(String wrapperName) {
		this.wrapperName = wrapperName;
	}
	
	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	public boolean isRequired() {
		return required;
	}

	public void setRequired(boolean required) {
		this.required = required;
	}

	public List<String> getAllowableValues() {
		return allowableValues;
	}

    public boolean isAllowMultiple() {
        return allowMultiple;
    }

    public void setAllowMultiple(boolean allowMultiple) {
        this.allowMultiple = allowMultiple;
    }

	public void setAllowableValues(List<String> allowableValues) {
		this.allowableValues = allowableValues;
	}

    public String getAllowedValuesString() {
        String result = "";
        if (this.allowableValues != null) {
            for(String allowedValue: this.allowableValues){
                result += (allowedValue +",");
            }
        }
        if(result.length() == 0)
            return null;
        else
            return result.substring(0, result.length() - 1);
    }

    public void setAllowedValues(String csvAlowedValue) {
        List<String> allowedValues = new ArrayList<String>();
        if (csvAlowedValue != null) {
            StringTokenizer tokenizer = new StringTokenizer( csvAlowedValue, "," );
            while(tokenizer.hasMoreTokens()){
                tokenizer.nextToken(",");
            }
        }
        this.setAllowableValues(allowedValues);
    }

	public String getParamType() {
		return paramType;
	}

	public void setParamType(String paramType) {
		this.paramType = paramType;
	}

	public String getInternalDescription() {
		return internalDescription;
	}

	public void setInternalDescription(String internalDescription) {
		this.internalDescription = internalDescription;
	}

	public String getParamAccess() {
		return paramAccess;
	}

	public void setParamAccess(String paramAccess) {
		this.paramAccess = paramAccess;
	}

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public FieldDefinition getFieldDefinition(){
        return fieldDefinition;
    }

    public FieldDefinition getFieldDefinition(DataTypeMappingProvider dataTypeMapper, ApiConfiguration config, NamingPolicyProvider nameGenerator) {
    	if(fieldDefinition == null) {
    		fieldDefinition = new FieldDefinition();
	    	String type = paramType.trim();
	    	if(type.contains("date")||type.contains("Date") ){
	    		fieldDefinition.getImportDefinitions().addAll(dataTypeMapper.getDateIncludes());
	    	}
	    	if(type.startsWith("List[")){
	    		fieldDefinition.getImportDefinitions().addAll(dataTypeMapper.getListIncludes());
	    		String entryType = type.substring(5, type.length()-1);
                if (dataTypeMapper.isPrimitiveType(entryType)) {
                    fieldDefinition.setCollectionItemType(entryType);
                    fieldDefinition.setCollectionItemName(entryType);
                } else {
                    fieldDefinition.setCollectionItemType(config.getModelPackageName() + "." + nameGenerator.applyClassNamingPolicy(entryType));
                    fieldDefinition.setCollectionItemName(nameGenerator.applyMethodNamingPolicy(entryType));
                }
	    		entryType =  dataTypeMapper.getClassType(entryType, true);
	    		String returnType = dataTypeMapper.getListReturnTypeSignature(entryType);
	    		fieldDefinition.setReturnType(returnType);
	    		fieldDefinition.setInitialization(" = " + dataTypeMapper.generateListInitialization(entryType));
	    		if(this.getWrapperName() != null){
	    			fieldDefinition.setName(this.getWrapperName());
	    		}else{
	    			fieldDefinition.setName(this.getName());
	    		}

	    	}else if(type.startsWith("Set[")){
	    		fieldDefinition.getImportDefinitions().addAll(dataTypeMapper.getSetIncludes());
	    		String entryType = type.substring(4, type.length()-1);
	    		entryType =  dataTypeMapper.getClassType(entryType, true);
	    		String returnType = dataTypeMapper.getSetReturnTypeSignature(entryType);
	    		fieldDefinition.setReturnType(returnType);
	    		fieldDefinition.setInitialization(" = " + dataTypeMapper.generateSetInitialization(entryType));
	    		if(this.getWrapperName() != null){
	    			fieldDefinition.setName(this.getWrapperName());
	    		}else{
	    			fieldDefinition.setName(this.getName());
	    		}

	    	}else if (type.startsWith("Map[")) {
                fieldDefinition.getImportDefinitions().addAll(dataTypeMapper.getMapIncludes());
                String keyClass, entryClass = "";
	    		String entryType = type.substring(4, type.length()-1);
                keyClass = entryType.substring(0, entryType.indexOf(",") );
                entryClass = entryType.substring(entryType.indexOf(",") + 1, entryType.length());
	    		//entryType =  dataTypeMapper.getClassType(entryType, true);
	    		entryType =  dataTypeMapper.getClassType(keyClass, true) + "," + dataTypeMapper.getClassType(entryClass, true);
	    		String returnType = dataTypeMapper.getMapReturnTypeSignature(entryType);
	    		fieldDefinition.setReturnType(returnType);
	    		fieldDefinition.setInitialization("= " + dataTypeMapper.generateMapInitialization(entryType));
	    		if(this.getWrapperName() != null){
	    			fieldDefinition.setName(this.getWrapperName());
	    		}else{
	    			fieldDefinition.setName(this.getName());
	    		}
	    	}else{
	    		fieldDefinition.setInitialization(dataTypeMapper.generateVariableInitialization(type));
	    		fieldDefinition.setReturnType(dataTypeMapper.getClassType(type, false));
	    		fieldDefinition.setName(this.getName());
	    	}
    	}
    	return fieldDefinition;
    }
}
