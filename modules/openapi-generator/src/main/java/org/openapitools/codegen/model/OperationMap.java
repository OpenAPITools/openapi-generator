package org.openapitools.codegen.model;

import org.openapitools.codegen.CodegenOperation;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;

public class OperationMap extends HashMap<String, Object> {

    public void setOperation(CodegenOperation ops) {
        put("operation", Collections.singletonList(ops));
    }

    public void setOperation(List<? extends CodegenOperation> ops) {
        put("operation", ops);
    }

    @SuppressWarnings("unchecked")
    public List<CodegenOperation> getOperation() {
        return (List<CodegenOperation>) get("operation");
    }

    public void setClassname(String classname) {
        put("classname", classname);
    }

    public String getClassname() {
        return (String) get("classname");
    }

    public void setPathPrefix(String pathPrefix) {
        put("pathPrefix", pathPrefix);
    }

    public String getPathPrefix() {
        return (String) get("pathPrefix");
    }

}
