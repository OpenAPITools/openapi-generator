package io.swagger.codegen;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

public class CodegenParameter {
    public Boolean isFormParam, isQueryParam, isPathParam, isHeaderParam,
            isCookieParam, isBodyParam, isFile, notFile, hasMore, isContainer, 
            secondaryParam, isBinary, isCollectionFormatMulti;
    public String baseName, paramName, dataType, collectionFormat, description, baseType, defaultValue;
    public String jsonSchema;
    public boolean isEnum;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    public Map<String, Object> vendorExtensions;

    /**
     * Determines whether this parameter is mandatory. If the parameter is in "path",
     * this property is required and its value MUST be true. Otherwise, the property
     * MAY be included and its default value is false.
     */
    public Boolean required;

	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor17.
	 */
    public Number maximum;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor17
	 */
    public Boolean exclusiveMaximum;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor21
	 */
    public Number minimum;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor21
	 */
    public Boolean exclusiveMinimum;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor26
	 */
    public Integer maxLength;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor29
	 */
    public Integer minLength;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor33
	 */
    public String pattern;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor42
	 */
    public Integer maxItems;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor45
	 */
    public Integer minItems;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor49
	 */
    public Boolean uniqueItems;
	/**
	 * See http://json-schema.org/latest/json-schema-validation.html#anchor14
	 */
    public Number multipleOf;
    
    public CodegenParameter copy() {
        CodegenParameter output = new CodegenParameter();
        output.isFile = this.isFile;
        output.notFile = this.notFile;
        output.hasMore = this.hasMore;
        output.isContainer = this.isContainer;
        output.secondaryParam = this.secondaryParam;
        output.baseName = this.baseName;
        output.paramName = this.paramName;
        output.dataType = this.dataType;
        output.collectionFormat = this.collectionFormat;
        output.isCollectionFormatMulti = this.isCollectionFormatMulti;
        output.description = this.description;
        output.baseType = this.baseType;
        output.isFormParam = this.isFormParam;
        output.isQueryParam = this.isQueryParam;
        output.isPathParam = this.isPathParam;
        output.isHeaderParam = this.isHeaderParam;
        output.isCookieParam = this.isCookieParam;
        output.isBodyParam = this.isBodyParam;
        output.required = this.required;
        output.maximum = this.maximum;
        output.exclusiveMaximum = this.exclusiveMaximum;
        output.minimum = this.minimum;
        output.exclusiveMinimum = this.exclusiveMinimum;
        output.maxLength = this.maxLength;
        output.minLength = this.minLength;
        output.pattern = this.pattern;
        output.maxItems = this.maxItems;
        output.minItems = this.minItems;
        output.uniqueItems = this.uniqueItems;
        output.multipleOf = this.multipleOf;
        output.jsonSchema = this.jsonSchema;
        output.defaultValue = this.defaultValue;
        output.isEnum = this.isEnum;
        if (this._enum != null) {
            output._enum = new ArrayList<String>(this._enum);
        }
        if (this.allowableValues != null) {
            output.allowableValues = new HashMap<String, Object>(this.allowableValues);
        }
        output.vendorExtensions = this.vendorExtensions;
        output.isBinary = this.isBinary;

        return output;
    }
}

