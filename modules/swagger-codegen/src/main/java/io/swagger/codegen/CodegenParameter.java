package io.swagger.codegen;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

public class CodegenParameter implements VendorExtendable {
    public boolean secondaryParam, notFile;
    public String baseName, paramName, dataType, datatypeWithEnum, dataFormat,
          collectionFormat, description, unescapedDescription, baseType, defaultValue, enumName;

    public String example; // example value (x-example)
    public String jsonSchema;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    public CodegenProperty items;
    public Map<String, Object> vendorExtensions = new HashMap<>();

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
        output.notFile = this.notFile;
        output.secondaryParam = this.secondaryParam;
        output.baseName = this.baseName;
        output.paramName = this.paramName;
        output.dataType = this.dataType;
        output.datatypeWithEnum = this.datatypeWithEnum;
        output.enumName = this.enumName;
        output.dataFormat = this.dataFormat;
        output.collectionFormat = this.collectionFormat;
        output.description = this.description;
        output.unescapedDescription = this.unescapedDescription;
        output.baseType = this.baseType;
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

        if (secondaryParam != that.secondaryParam)
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
        int result = secondaryParam ? 13:31;
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
        result = 31 * result + (notFile ? 13:31);
        result = 31 * result + (_enum != null ? _enum.hashCode() : 0);
        result = 31 * result + (allowableValues != null ? allowableValues.hashCode() : 0);
        result = 31 * result + (items != null ? items.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
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

    public boolean getSecondaryParam() {
        return secondaryParam;
    }

    public String getBaseName() {
        return baseName;
    }

    public String getParamName() {
        return paramName;
    }

    public String getDataType() {
        return dataType;
    }

    public String getDatatypeWithEnum() {
        return datatypeWithEnum;
    }

    public String getDataFormat() {
        return dataFormat;
    }

    public String getCollectionFormat() {
        return collectionFormat;
    }

    public String getDescription() {
        return description;
    }

    public String getUnescapedDescription() {
        return unescapedDescription;
    }

    public String getBaseType() {
        return baseType;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public String getEnumName() {
        return enumName;
    }

    public String getExample() {
        return example;
    }

    public String getJsonSchema() {
        return jsonSchema;
    }

    public boolean getIsNotFile() {
        return notFile;
    }

    public List<String> get_enum() {
        return _enum;
    }

    public Map<String, Object> getAllowableValues() {
        return allowableValues;
    }

    public CodegenProperty getItems() {
        return items;
    }

    public Map<String, Object> getVendorExtensions() {
        return vendorExtensions;
    }

    public boolean getRequired() {
        return required;
    }

    public String getMaximum() {
        return maximum;
    }

    public boolean getExclusiveMaximum() {
        return exclusiveMaximum;
    }

    public String getMinimum() {
        return minimum;
    }

    public boolean getExclusiveMinimum() {
        return exclusiveMinimum;
    }

    public Integer getMaxLength() {
        return maxLength;
    }

    public Integer getMinLength() {
        return minLength;
    }

    public String getPattern() {
        return pattern;
    }

    public Integer getMaxItems() {
        return maxItems;
    }

    public Integer getMinItems() {
        return minItems;
    }

    public boolean getUniqueItems() {
        return uniqueItems;
    }

    public Number getMultipleOf() {
        return multipleOf;
    }
}

