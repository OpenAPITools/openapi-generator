package org.openapitools.codegen.templating;

import org.openapitools.codegen.CodegenOperation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class CodegenOperationBundle extends BaseBundle{
    private String path;
    private Collection<CodegenOperation> operation = new ArrayList<>();
    private boolean hasMore;

    // getters and setters. Each setter puts the value in the underlying Map

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
        put("path", path);
    }


    public Collection<CodegenOperation> getOperation() {
        return operation;
    }

    public void setOperation(Collection<CodegenOperation> operation) {
        this.operation = operation;
        put("operation", operation);
    }


    public boolean getHasMore() {
        return hasMore;
    }

    public void setHasMore(boolean hasMore) {
        this.hasMore = hasMore;
        put("hasMore", hasMore);
    }
}
