package io.swagger.codegen;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

public class CodegenParameter {
    public Boolean isFormParam, isQueryParam, isPathParam, isHeaderParam,
            isCookieParam, isBodyParam, hasMore, isContainer,
            secondaryParam, isCollectionFormatMulti, isPrimitiveType;
    public String baseName, paramName, dataType, datatypeWithEnum, dataFormat, collectionFormat, description, unescapedDescription, baseType, defaultValue, enumName;
    public String example; // example value (x-example)
    public String jsonSchema;
    public Boolean isString, isInteger, isLong, isFloat, isDouble, isByteArray, isBinary, isBoolean, isDate, isDateTime;
    public Boolean isListContainer, isMapContainer;
    public Boolean isFile, notFile;
    public boolean isEnum;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    public CodegenProperty items;
    public Map<String, Object> vendorExtensions;
    public Boolean hasValidation;

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
        output.datatypeWithEnum = this.datatypeWithEnum;
        output.enumName = this.enumName;
        output.dataFormat = this.dataFormat;
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
        output.example = this.example;
        output.isEnum = this.isEnum;
        if (this._enum != null) {
            output._enum = new ArrayList<String>(this._enum);
        }
        if (this.allowableValues != null) {
            output.allowableValues = new HashMap<String, Object>(this.allowableValues);
        }
        if (this.items != null) {
            output.items = this.items;
        }
        output.vendorExtensions = this.vendorExtensions;
        output.hasValidation = this.hasValidation;
        output.isBinary = this.isBinary;
        output.isByteArray = this.isByteArray;
        output.isString = this.isString;
        output.isInteger = this.isInteger;
        output.isLong = this.isLong;
        output.isDouble = this.isDouble;
        output.isFloat = this.isFloat;
        output.isBoolean = this.isBoolean;
        output.isDate = this.isDate;
        output.isDateTime = this.isDateTime;
        output.isListContainer = this.isListContainer;
        output.isMapContainer = this.isMapContainer;

        return output;
    }

    @Override
    public String toString() {
        return String.format("%s(%s)", baseName, dataType);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenParameter that = (CodegenParameter) o;

        if (isEnum != that.isEnum) return false;
        if (isFormParam != null ? !isFormParam.equals(that.isFormParam) : that.isFormParam != null)
            return false;
        if (isQueryParam != null ? !isQueryParam.equals(that.isQueryParam) : that.isQueryParam != null)
            return false;
        if (isPathParam != null ? !isPathParam.equals(that.isPathParam) : that.isPathParam != null)
            return false;
        if (isHeaderParam != null ? !isHeaderParam.equals(that.isHeaderParam) : that.isHeaderParam != null)
            return false;
        if (isCookieParam != null ? !isCookieParam.equals(that.isCookieParam) : that.isCookieParam != null)
            return false;
        if (isBodyParam != null ? !isBodyParam.equals(that.isBodyParam) : that.isBodyParam != null)
            return false;
        if (hasMore != null ? !hasMore.equals(that.hasMore) : that.hasMore != null)
            return false;
        if (isContainer != null ? !isContainer.equals(that.isContainer) : that.isContainer != null)
            return false;
        if (secondaryParam != null ? !secondaryParam.equals(that.secondaryParam) : that.secondaryParam != null)
            return false;
        if (isCollectionFormatMulti != null ? !isCollectionFormatMulti.equals(that.isCollectionFormatMulti) : that.isCollectionFormatMulti != null)
            return false;
        if (isPrimitiveType != null ? !isPrimitiveType.equals(that.isPrimitiveType) : that.isPrimitiveType != null)
            return false;
        if (baseName != null ? !baseName.equals(that.baseName) : that.baseName != null)
            return false;
        if (paramName != null ? !paramName.equals(that.paramName) : that.paramName != null)
            return false;
        if (dataType != null ? !dataType.equals(that.dataType) : that.dataType != null)
            return false;
        if (datatypeWithEnum != null ? !datatypeWithEnum.equals(that.datatypeWithEnum) : that.datatypeWithEnum != null)
            return false;
        if (enumName != null ? !enumName.equals(that.enumName) : that.enumName != null)
            return false;
        if (dataFormat != null ? !dataFormat.equals(that.dataFormat) : that.dataFormat != null)
            return false;
        if (collectionFormat != null ? !collectionFormat.equals(that.collectionFormat) : that.collectionFormat != null)
            return false;
        if (description != null ? !description.equals(that.description) : that.description != null)
            return false;
        if (unescapedDescription != null ? !unescapedDescription.equals(that.unescapedDescription) : that.unescapedDescription != null)
            return false;
        if (baseType != null ? !baseType.equals(that.baseType) : that.baseType != null)
            return false;
        if (defaultValue != null ? !defaultValue.equals(that.defaultValue) : that.defaultValue != null)
            return false;
        if (example != null ? !example.equals(that.example) : that.example != null)
            return false;
        if (jsonSchema != null ? !jsonSchema.equals(that.jsonSchema) : that.jsonSchema != null)
            return false;
        if (isString != null ? !isString.equals(that.isString) : that.isString != null)
            return false;
        if (isInteger != null ? !isInteger.equals(that.isInteger) : that.isInteger != null)
            return false;
        if (isLong != null ? !isLong.equals(that.isLong) : that.isLong != null)
            return false;
        if (isFloat != null ? !isFloat.equals(that.isFloat) : that.isFloat != null)
            return false;
        if (isDouble != null ? !isDouble.equals(that.isDouble) : that.isDouble != null)
            return false;
        if (isByteArray != null ? !isByteArray.equals(that.isByteArray) : that.isByteArray != null)
            return false;
        if (isBinary != null ? !isBinary.equals(that.isBinary) : that.isBinary != null)
            return false;
        if (isBoolean != null ? !isBoolean.equals(that.isBoolean) : that.isBoolean != null)
            return false;
        if (isDate != null ? !isDate.equals(that.isDate) : that.isDate != null)
            return false;
        if (isDateTime != null ? !isDateTime.equals(that.isDateTime) : that.isDateTime != null)
            return false;
        if (isListContainer != null ? !isListContainer.equals(that.isListContainer) : that.isListContainer != null)
            return false;
        if (isMapContainer != null ? !isMapContainer.equals(that.isMapContainer) : that.isMapContainer != null)
            return false;
        if (isFile != null ? !isFile.equals(that.isFile) : that.isFile != null)
            return false;
        if (notFile != null ? !notFile.equals(that.notFile) : that.notFile != null)
            return false;
        if (_enum != null ? !_enum.equals(that._enum) : that._enum != null)
            return false;
        if (allowableValues != null ? !allowableValues.equals(that.allowableValues) : that.allowableValues != null)
            return false;
        if (items != null ? !items.equals(that.items) : that.items != null)
            return false;
        if (vendorExtensions != null ? !vendorExtensions.equals(that.vendorExtensions) : that.vendorExtensions != null)
            return false;
        if (hasValidation != null ? !hasValidation.equals(that.hasValidation) : that.hasValidation != null)
            return false;
        if (required != null ? !required.equals(that.required) : that.required != null)
            return false;
        if (maximum != null ? !maximum.equals(that.maximum) : that.maximum != null)
            return false;
        if (exclusiveMaximum != null ? !exclusiveMaximum.equals(that.exclusiveMaximum) : that.exclusiveMaximum != null)
            return false;
        if (minimum != null ? !minimum.equals(that.minimum) : that.minimum != null)
            return false;
        if (exclusiveMinimum != null ? !exclusiveMinimum.equals(that.exclusiveMinimum) : that.exclusiveMinimum != null)
            return false;
        if (maxLength != null ? !maxLength.equals(that.maxLength) : that.maxLength != null)
            return false;
        if (minLength != null ? !minLength.equals(that.minLength) : that.minLength != null)
            return false;
        if (pattern != null ? !pattern.equals(that.pattern) : that.pattern != null)
            return false;
        if (maxItems != null ? !maxItems.equals(that.maxItems) : that.maxItems != null)
            return false;
        if (minItems != null ? !minItems.equals(that.minItems) : that.minItems != null)
            return false;
        if (uniqueItems != null ? !uniqueItems.equals(that.uniqueItems) : that.uniqueItems != null)
            return false;
        return multipleOf != null ? multipleOf.equals(that.multipleOf) : that.multipleOf == null;

    }

    @Override
    public int hashCode() {
        int result = isFormParam != null ? isFormParam.hashCode() : 0;
        result = 31 * result + (isQueryParam != null ? isQueryParam.hashCode() : 0);
        result = 31 * result + (isPathParam != null ? isPathParam.hashCode() : 0);
        result = 31 * result + (isHeaderParam != null ? isHeaderParam.hashCode() : 0);
        result = 31 * result + (isCookieParam != null ? isCookieParam.hashCode() : 0);
        result = 31 * result + (isBodyParam != null ? isBodyParam.hashCode() : 0);
        result = 31 * result + (hasMore != null ? hasMore.hashCode() : 0);
        result = 31 * result + (isContainer != null ? isContainer.hashCode() : 0);
        result = 31 * result + (secondaryParam != null ? secondaryParam.hashCode() : 0);
        result = 31 * result + (isCollectionFormatMulti != null ? isCollectionFormatMulti.hashCode() : 0);
        result = 31 * result + (isPrimitiveType != null ? isPrimitiveType.hashCode() : 0);
        result = 31 * result + (baseName != null ? baseName.hashCode() : 0);
        result = 31 * result + (paramName != null ? paramName.hashCode() : 0);
        result = 31 * result + (dataType != null ? dataType.hashCode() : 0);
        result = 31 * result + (datatypeWithEnum != null ? datatypeWithEnum.hashCode() : 0);
        result = 31 * result + (enumName != null ? enumName.hashCode() : 0);
        result = 31 * result + (dataFormat != null ? dataFormat.hashCode() : 0);
        result = 31 * result + (collectionFormat != null ? collectionFormat.hashCode() : 0);
        result = 31 * result + (description != null ? description.hashCode() : 0);
        result = 31 * result + (unescapedDescription != null ? unescapedDescription.hashCode() : 0);
        result = 31 * result + (baseType != null ? baseType.hashCode() : 0);
        result = 31 * result + (defaultValue != null ? defaultValue.hashCode() : 0);
        result = 31 * result + (example != null ? example.hashCode() : 0);
        result = 31 * result + (jsonSchema != null ? jsonSchema.hashCode() : 0);
        result = 31 * result + (isString != null ? isString.hashCode() : 0);
        result = 31 * result + (isInteger != null ? isInteger.hashCode() : 0);
        result = 31 * result + (isLong != null ? isLong.hashCode() : 0);
        result = 31 * result + (isFloat != null ? isFloat.hashCode() : 0);
        result = 31 * result + (isDouble != null ? isDouble.hashCode() : 0);
        result = 31 * result + (isByteArray != null ? isByteArray.hashCode() : 0);
        result = 31 * result + (isBinary != null ? isBinary.hashCode() : 0);
        result = 31 * result + (isBoolean != null ? isBoolean.hashCode() : 0);
        result = 31 * result + (isDate != null ? isDate.hashCode() : 0);
        result = 31 * result + (isDateTime != null ? isDateTime.hashCode() : 0);
        result = 31 * result + (isListContainer != null ? isListContainer.hashCode() : 0);
        result = 31 * result + (isMapContainer != null ? isMapContainer.hashCode() : 0);
        result = 31 * result + (isFile != null ? isFile.hashCode() : 0);
        result = 31 * result + (notFile != null ? notFile.hashCode() : 0);
        result = 31 * result + (isEnum ? 1 : 0);
        result = 31 * result + (_enum != null ? _enum.hashCode() : 0);
        result = 31 * result + (allowableValues != null ? allowableValues.hashCode() : 0);
        result = 31 * result + (items != null ? items.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
        result = 31 * result + (hasValidation != null ? hasValidation.hashCode() : 0);
        result = 31 * result + (required != null ? required.hashCode() : 0);
        result = 31 * result + (maximum != null ? maximum.hashCode() : 0);
        result = 31 * result + (exclusiveMaximum != null ? exclusiveMaximum.hashCode() : 0);
        result = 31 * result + (minimum != null ? minimum.hashCode() : 0);
        result = 31 * result + (exclusiveMinimum != null ? exclusiveMinimum.hashCode() : 0);
        result = 31 * result + (maxLength != null ? maxLength.hashCode() : 0);
        result = 31 * result + (minLength != null ? minLength.hashCode() : 0);
        result = 31 * result + (pattern != null ? pattern.hashCode() : 0);
        result = 31 * result + (maxItems != null ? maxItems.hashCode() : 0);
        result = 31 * result + (minItems != null ? minItems.hashCode() : 0);
        result = 31 * result + (uniqueItems != null ? uniqueItems.hashCode() : 0);
        result = 31 * result + (multipleOf != null ? multipleOf.hashCode() : 0);
        return result;
    }
}

