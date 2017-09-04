package io.swagger.codegen;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

public class CodegenParameter {
    public boolean isFormParam, isQueryParam, isPathParam, isHeaderParam,
            isCookieParam, isBodyParam, hasMore, isContainer,
            secondaryParam, isCollectionFormatMulti, isPrimitiveType;
    public String baseName, paramName, dataType, datatypeWithEnum, dataFormat,
          collectionFormat, description, unescapedDescription, baseType, defaultValue, enumName;

    public String example; // example value (x-example)
    public String jsonSchema;
    public boolean isString, isNumeric, isInteger, isLong, isFloat, isDouble, isByteArray, isBinary, isBoolean, isDate, isDateTime, isUuid;
    public boolean isListContainer, isMapContainer;
    public boolean isFile, notFile;
    public boolean isEnum;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    public CodegenProperty items;
    public Map<String, Object> vendorExtensions;
    public boolean hasValidation;

    /**
     * Determines whether this parameter is mandatory. If the parameter is in "path",
     * this property is required and its value MUST be true. Otherwise, the property
     * MAY be included and its default value is false.
     */
    public boolean required;

    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor17.
     */
    public String maximum;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor17
     */
    public boolean exclusiveMaximum;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor21
     */
    public String minimum;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor21
     */
    public boolean exclusiveMinimum;
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
    public boolean uniqueItems;
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
        output.isPrimitiveType = this.isPrimitiveType;
        output.description = this.description;
        output.unescapedDescription = this.unescapedDescription;
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
        if(this.vendorExtensions != null){
            output.vendorExtensions = new HashMap<String, Object>(this.vendorExtensions);
        }
        output.hasValidation = this.hasValidation;
        output.isBinary = this.isBinary;
        output.isByteArray = this.isByteArray;
        output.isString = this.isString;
        output.isNumeric = this.isNumeric;
        output.isInteger = this.isInteger;
        output.isLong = this.isLong;
        output.isDouble = this.isDouble;
        output.isFloat = this.isFloat;
        output.isBoolean = this.isBoolean;
        output.isDate = this.isDate;
        output.isDateTime = this.isDateTime;
        output.isUuid = this.isUuid;
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
        if (isFormParam != that.isFormParam)
            return false;
        if (isQueryParam != that.isQueryParam)
            return false;
        if (isPathParam != that.isPathParam)
            return false;
        if (isHeaderParam != that.isHeaderParam)
            return false;
        if (isCookieParam != that.isCookieParam)
            return false;
        if (isBodyParam != that.isBodyParam)
            return false;
        if (hasMore != that.hasMore)
            return false;
        if (isContainer != that.isContainer)
            return false;
        if (secondaryParam != that.secondaryParam)
            return false;
        if (isCollectionFormatMulti != that.isCollectionFormatMulti)
            return false;
        if (isPrimitiveType != that.isPrimitiveType)
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
        if (isString != that.isString)
            return false;
        if (isNumeric != that.isNumeric)
            return false;
        if (isInteger != that.isInteger)
            return false;
        if (isLong != that.isLong)
            return false;
        if (isFloat != that.isFloat)
            return false;
        if (isDouble != that.isDouble)
            return false;
        if (isByteArray != that.isByteArray)
            return false;
        if (isBinary != that.isBinary)
            return false;
        if (isBoolean != that.isBoolean)
            return false;
        if (isDate != that.isDate)
            return false;
        if (isDateTime != that.isDateTime)
            return false;
        if (isListContainer != that.isListContainer)
            return false;
        if (isMapContainer != that.isMapContainer)
            return false;
        if (isFile != that.isFile)
            return false;
        if (notFile != that.notFile)
            return false;
        if (_enum != null ? !_enum.equals(that._enum) : that._enum != null)
            return false;
        if (allowableValues != null ? !allowableValues.equals(that.allowableValues) : that.allowableValues != null)
            return false;
        if (items != null ? !items.equals(that.items) : that.items != null)
            return false;
        if (vendorExtensions != null ? !vendorExtensions.equals(that.vendorExtensions) : that.vendorExtensions != null)
            return false;
        if (hasValidation != that.hasValidation)
            return false;
        if (required != that.required)
            return false;
        if (maximum != null ? !maximum.equals(that.maximum) : that.maximum != null)
            return false;
        if (exclusiveMaximum != that.exclusiveMaximum)
            return false;
        if (minimum != null ? !minimum.equals(that.minimum) : that.minimum != null)
            return false;
        if (exclusiveMinimum != that.exclusiveMinimum)
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
        if (uniqueItems != that.uniqueItems)
            return false;
        return multipleOf != null ? multipleOf.equals(that.multipleOf) : that.multipleOf == null;

    }

    @Override
    public int hashCode() {
        int result = isFormParam ? 13:31;
        result = 31 * result + (isQueryParam ? 13:31);
        result = 31 * result + (isPathParam ? 13:31);
        result = 31 * result + (isHeaderParam ? 13:31);
        result = 31 * result + (isCookieParam ? 13:31);
        result = 31 * result + (isBodyParam ? 13:31);
        result = 31 * result + (hasMore ? 13:31);
        result = 31 * result + (isContainer ? 13:31);
        result = 31 * result + (secondaryParam ? 13:31);
        result = 31 * result + (isCollectionFormatMulti ? 13:31);
        result = 31 * result + (isPrimitiveType ? 13:31);
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
        result = 31 * result + (isString ? 13:31);
        result = 31 * result + (isNumeric ? 13:31);
        result = 31 * result + (isInteger ? 13:31);
        result = 31 * result + (isLong ? 13:31);
        result = 31 * result + (isFloat ? 13:31);
        result = 31 * result + (isDouble ? 13:31);
        result = 31 * result + (isByteArray ? 13:31);
        result = 31 * result + (isBinary ? 13:31);
        result = 31 * result + (isBoolean ? 13:31);
        result = 31 * result + (isDate ? 13:31);
        result = 31 * result + (isDateTime ? 13:31);
        result = 31 * result + (isListContainer ? 13:31);
        result = 31 * result + (isMapContainer ? 13:31);
        result = 31 * result + (isFile ? 13:31);
        result = 31 * result + (notFile ? 13:31);
        result = 31 * result + (isEnum ? 1 : 0);
        result = 31 * result + (_enum != null ? _enum.hashCode() : 0);
        result = 31 * result + (allowableValues != null ? allowableValues.hashCode() : 0);
        result = 31 * result + (items != null ? items.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
        result = 31 * result + (hasValidation ? 13:31);
        result = 31 * result + (required ? 13:31);
        result = 31 * result + (maximum != null ? maximum.hashCode() : 0);
        result = 31 * result + (exclusiveMaximum ? 13:31);
        result = 31 * result + (minimum != null ? minimum.hashCode() : 0);
        result = 31 * result + (exclusiveMinimum ? 13:31);
        result = 31 * result + (maxLength != null ? maxLength.hashCode() : 0);
        result = 31 * result + (minLength != null ? minLength.hashCode() : 0);
        result = 31 * result + (pattern != null ? pattern.hashCode() : 0);
        result = 31 * result + (maxItems != null ? maxItems.hashCode() : 0);
        result = 31 * result + (minItems != null ? minItems.hashCode() : 0);
        result = 31 * result + (uniqueItems ? 13:31);
        result = 31 * result + (multipleOf != null ? multipleOf.hashCode() : 0);
        return result;
    }
}

