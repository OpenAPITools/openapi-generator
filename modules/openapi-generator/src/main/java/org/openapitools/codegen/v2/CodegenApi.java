package org.openapitools.codegen.v2;

import java.util.Collection;

public class CodegenApi extends CodegenObject {
    public CodegenApi(String id, CodegenSdk sdk) {
        super(id, sdk);
    }

    public void addModel(CodegenModel model) {
        addChild(model);
    }

    public void addOperation(CodegenOperation operation) {
        addChild(operation);
    }

    public Collection<CodegenModel> getModels() {
        return getChildrenOfType(CodegenModel.class);
    }

    public Collection<CodegenOperation> getOperations() {
        return getChildrenOfType(CodegenOperation.class);
    }
}
