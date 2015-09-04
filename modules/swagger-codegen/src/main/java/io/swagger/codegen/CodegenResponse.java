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
}
