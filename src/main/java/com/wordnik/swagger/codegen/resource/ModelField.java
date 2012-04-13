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

import com.wordnik.swagger.codegen.config.ReservedWordMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    private AllowableValues allowableValues = null;
    private String paramType;
    private String dataType;
    private String internalDescription;
    private String paramAccess;
    private String valueTypeInternal;
    private String genericType;
    private FieldDefinition fieldDefinition;

    Logger logger = LoggerFactory.getLogger(ModelField.class);

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

	public AllowableValues getAllowableValues() {
		return allowableValues;
	}

    public boolean isAllowMultiple() {
        return allowMultiple;
    }

    public void setAllowMultiple(boolean allowMultiple) {
        this.allowMultiple = allowMultiple;
    }

	public void setAllowableValues(AllowableValues allowableValues) {
		this.allowableValues = allowableValues;
	}

    public String getAllowedValuesString() {
        if(this.allowableValues != null){
            return this.allowableValues.toString();
        }else{
            return null;
        }
    }

    public void setAllowedValues(String csvAlowedValue) {
        this.setAllowableValues(AllowableValues.ConvertAllowableValuesStringToObject(csvAlowedValue));
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

    public String getValueTypeInternal() {
        return valueTypeInternal;
    }

    public void setValueTypeInternal(String valueTypeInternal) {
        this.valueTypeInternal = valueTypeInternal;
    }

    public String getGenericType() {
        if(genericType == null){
            if(dataType.startsWith("List[")){
                genericType = dataType.substring(5, dataType.length()-1);
            } else if(dataType.startsWith("Set[")){
                genericType = dataType.substring(4, dataType.length()-1);
            } else if(dataType.startsWith("Array[")){
                genericType = dataType.substring(6, dataType.length()-1);
            } else if(dataType.startsWith("Map[")){
                genericType = dataType.substring(4, dataType.length()-1);
            } else {
                genericType = dataType;
            }
        }
        return genericType;
    }


    public FieldDefinition getFieldDefinition(){
        return fieldDefinition;
    }

    public FieldDefinition getFieldDefinition(DataTypeMappingProvider dataTypeMapper, ApiConfiguration config, NamingPolicyProvider nameGenerator, ReservedWordMapper reservedWordMapper) {
        try{
            if(fieldDefinition == null) {
                fieldDefinition = new FieldDefinition();
                String type = paramType.trim();
                if(type.contains("date")||type.contains("Date") ){
                    fieldDefinition.getImportDefinitions().addAll(dataTypeMapper.getDateIncludes());
                    fieldDefinition.setHasDateResponse(true);
                }
                if(type.startsWith("List[")){
                    fieldDefinition.getImportDefinitions().addAll(dataTypeMapper.getListIncludes());
                    String entryType = type.substring(5, type.length()-1);
                    if (dataTypeMapper.isPrimitiveType(entryType)) {
                        fieldDefinition.setCollectionItemType(entryType);
                        fieldDefinition.setCollectionItemName(entryType);
                    } else {
                        final String collectionItemType = config.getModelPackageName().length() == 0 ? nameGenerator.applyClassNamingPolicy(entryType) : config.getModelPackageName() + "." + nameGenerator.applyClassNamingPolicy(entryType);
                        fieldDefinition.setCollectionItemType(collectionItemType);
                        fieldDefinition.setCollectionItemName(nameGenerator.applyMethodNamingPolicy(entryType));
                    }
                    entryType =  dataTypeMapper.getClassType(entryType, true);
                    fieldDefinition.setHasPrimitiveType(dataTypeMapper.isPrimitiveType(entryType));
                    fieldDefinition.setHasListResponse(true);
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
                    fieldDefinition.setHasPrimitiveType(dataTypeMapper.isPrimitiveType(entryType));
                    fieldDefinition.setHasSetResponse(true);
                    String returnType = dataTypeMapper.getSetReturnTypeSignature(entryType);
                    fieldDefinition.setReturnType(returnType);
                    fieldDefinition.setInitialization(" = " + dataTypeMapper.generateSetInitialization(entryType));
                    if(this.getWrapperName() != null){
                        fieldDefinition.setName(this.getWrapperName());
                    }else{
                        fieldDefinition.setName(this.getName());
                    }

                }else if(type.startsWith("Array[")){
                    fieldDefinition.getImportDefinitions().addAll(dataTypeMapper.getSetIncludes());
                    String entryType = type.substring(6, type.length()-1);
                    entryType =  dataTypeMapper.getClassType(entryType, true);
                    fieldDefinition.setHasPrimitiveType(dataTypeMapper.isPrimitiveType(entryType));
                    fieldDefinition.setHasArrayResponse(true);
                    String returnType = dataTypeMapper.getArrayReturnTypeSignature(entryType);
                    fieldDefinition.setReturnType(returnType);
                    fieldDefinition.setInitialization(" = " + dataTypeMapper.generateArrayInitialization(entryType));
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
                    fieldDefinition.setHasPrimitiveType(dataTypeMapper.isPrimitiveType(entryClass));
                    fieldDefinition.setHasMapResponse(true);
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
                    fieldDefinition.setHasPrimitiveType(dataTypeMapper.isPrimitiveType(fieldDefinition.getReturnType()));
                }
            }
            fieldDefinition.setOriginalName(reservedWordMapper.retranslate(fieldDefinition.getName()));
            return fieldDefinition;
        }catch(RuntimeException t){
            logger.error("Error generating field definition for object " + this.getName() + " data type " + this.getDataType());
            throw t;
        }

    }
}
