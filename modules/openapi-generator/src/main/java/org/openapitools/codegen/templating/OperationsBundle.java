package org.openapitools.codegen.templating;

import java.util.*;

import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenSecurity;

public class OperationsBundle extends BaseBundle{

    private List<CodegenOperation> operation = new ArrayList<>();
    private String classname;
    private String pathPrefix;

    // getters and setters. Each setter puts the value in the underlying Map

    public String getPathPrefix() {
        return pathPrefix;
    }

    public void setPathPrefix(String pathPrefix) {
        this.pathPrefix = pathPrefix;
        put("pathPrefix", pathPrefix);
    }

    public List<CodegenOperation> getOperation() {
        return this.operation;
    }

    public void setOperation(List<CodegenOperation> operation) {
        this.operation = operation;
        put("operation", operation);
    }


    public String getClassname() {
        return classname;
    }

    public void setClassname(String className) {
        this.classname = className;
        put("classname", className);
    }

}
