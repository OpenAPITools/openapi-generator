package io.swagger.codegen;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class CodegenResponse {
    public final List<CodegenProperty> headers = new ArrayList<CodegenProperty>();
    public String code, message;
    public Boolean hasMore;
    public List<Map<String, Object>> examples;
    public String dataType, baseType, containerType;
    public Boolean isDefault;
    public Boolean simpleType;
    public Boolean primitiveType;
    public Boolean isMapContainer;
    public Boolean isListContainer;
    public Boolean isBinary = Boolean.FALSE;
    public Object schema;
    public String jsonSchema;

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
        if (hasMore != null ? !hasMore.equals(that.hasMore) : that.hasMore != null)
            return false;
        if (examples != null ? !examples.equals(that.examples) : that.examples != null)
            return false;
        if (dataType != null ? !dataType.equals(that.dataType) : that.dataType != null)
            return false;
        if (baseType != null ? !baseType.equals(that.baseType) : that.baseType != null)
            return false;
        if (containerType != null ? !containerType.equals(that.containerType) : that.containerType != null)
            return false;
        if (isDefault != null ? !isDefault.equals(that.isDefault) : that.isDefault != null)
            return false;
        if (simpleType != null ? !simpleType.equals(that.simpleType) : that.simpleType != null)
            return false;
        if (primitiveType != null ? !primitiveType.equals(that.primitiveType) : that.primitiveType != null)
            return false;
        if (isMapContainer != null ? !isMapContainer.equals(that.isMapContainer) : that.isMapContainer != null)
            return false;
        if (isListContainer != null ? !isListContainer.equals(that.isListContainer) : that.isListContainer != null)
            return false;
        if (isBinary != null ? !isBinary.equals(that.isBinary) : that.isBinary != null)
            return false;
        if (schema != null ? !schema.equals(that.schema) : that.schema != null)
            return false;
        return jsonSchema != null ? jsonSchema.equals(that.jsonSchema) : that.jsonSchema == null;

    }

    @Override
    public int hashCode() {
        int result = headers.hashCode();
        result = 31 * result + (code != null ? code.hashCode() : 0);
        result = 31 * result + (message != null ? message.hashCode() : 0);
        result = 31 * result + (hasMore != null ? hasMore.hashCode() : 0);
        result = 31 * result + (examples != null ? examples.hashCode() : 0);
        result = 31 * result + (dataType != null ? dataType.hashCode() : 0);
        result = 31 * result + (baseType != null ? baseType.hashCode() : 0);
        result = 31 * result + (containerType != null ? containerType.hashCode() : 0);
        result = 31 * result + (isDefault != null ? isDefault.hashCode() : 0);
        result = 31 * result + (simpleType != null ? simpleType.hashCode() : 0);
        result = 31 * result + (primitiveType != null ? primitiveType.hashCode() : 0);
        result = 31 * result + (isMapContainer != null ? isMapContainer.hashCode() : 0);
        result = 31 * result + (isListContainer != null ? isListContainer.hashCode() : 0);
        result = 31 * result + (isBinary != null ? isBinary.hashCode() : 0);
        result = 31 * result + (schema != null ? schema.hashCode() : 0);
        result = 31 * result + (jsonSchema != null ? jsonSchema.hashCode() : 0);
        return result;
    }
}
