package io.swagger.codegen;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class CodegenResponse {
    public final List<CodegenProperty> headers = new ArrayList<CodegenProperty>();
    public String code, message;
    public boolean hasMore;
    public List<Map<String, Object>> examples;
    public String dataType, baseType, containerType;
    public boolean hasHeaders;
    public boolean isString, isNumeric, isInteger, isLong, isFloat, isDouble, isByteArray, isBoolean, isDate, isDateTime, isUuid;
    public boolean isDefault;
    public boolean simpleType;
    public boolean primitiveType;
    public boolean isMapContainer;
    public boolean isListContainer;
    public boolean isBinary = false;
    public boolean isFile = false;
    public Object schema;
    public String jsonSchema;
    public Map<String, Object> vendorExtensions;

    public boolean isWildcard() {
        return "0".equals(code) || "default".equals(code);
    }

    @Override
    public String toString() {
        return String.format("%s(%s)", code, containerType);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenResponse that = (CodegenResponse) o;

        if (!headers.equals(that.headers))
            return false;
        if (code != null ? !code.equals(that.code) : that.code != null)
            return false;
        if (message != null ? !message.equals(that.message) : that.message != null)
            return false;
        if (hasMore != that.hasMore)
            return false;
        if (examples != null ? !examples.equals(that.examples) : that.examples != null)
            return false;
        if (dataType != null ? !dataType.equals(that.dataType) : that.dataType != null)
            return false;
        if (baseType != null ? !baseType.equals(that.baseType) : that.baseType != null)
            return false;
        if (containerType != null ? !containerType.equals(that.containerType) : that.containerType != null)
            return false;
        if (isDefault != that.isDefault)
            return false;
        if (simpleType != that.simpleType)
            return false;
        if (primitiveType != that.primitiveType)
            return false;
        if (isMapContainer != that.isMapContainer)
            return false;
        if (isListContainer != that.isListContainer)
            return false;
        if (isBinary != that.isBinary)
            return false;
        if (isFile != that.isFile)
            return false;
        if (isNumeric != that.isNumeric)
            return false;
        if (schema != null ? !schema.equals(that.schema) : that.schema != null)
            return false;
        if (vendorExtensions != null ? !vendorExtensions.equals(that.vendorExtensions) : that.vendorExtensions != null)
            return false;
        return jsonSchema != null ? jsonSchema.equals(that.jsonSchema) : that.jsonSchema == null;
    }

    @Override
    public int hashCode() {
        int result = headers.hashCode();
        result = 31 * result + (code != null ? code.hashCode() : 0);
        result = 31 * result + (message != null ? message.hashCode() : 0);
        result = 31 * result + (hasMore ? 13:31);
        result = 31 * result + (examples != null ? examples.hashCode() : 0);
        result = 31 * result + (dataType != null ? dataType.hashCode() : 0);
        result = 31 * result + (baseType != null ? baseType.hashCode() : 0);
        result = 31 * result + (containerType != null ? containerType.hashCode() : 0);
        result = 31 * result + (isDefault ? 13:31);
        result = 31 * result + (isNumeric ? 13:31);
        result = 31 * result + (simpleType ? 13:31);
        result = 31 * result + (primitiveType ? 13:31);
        result = 31 * result + (isMapContainer ? 13:31);
        result = 31 * result + (isListContainer ? 13:31);
        result = 31 * result + (isBinary ? 13:31);
        result = 31 * result + (isFile ? 13:31);
        result = 31 * result + (schema != null ? schema.hashCode() : 0);
        result = 31 * result + (jsonSchema != null ? jsonSchema.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
        return result;
    }
}
