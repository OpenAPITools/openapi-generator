package org.openapitools.codegen.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.openapitools.codegen.CodegenOperation;

public class OperationsMap extends HashMap<String, Object> {

    public void setOperation(OperationMap objs) {
        put("operations", objs);
    }

    public OperationMap getOperations() {
        return (OperationMap) get("operations");
    }

    public void setImports(List<Map<String, String>> imports) {
        put("imports", imports);
    }

    @SuppressWarnings("unchecked")
    public List<Map<String, String>> getImports() {
        return (List<Map<String, String>>) get("imports");
    }

}
