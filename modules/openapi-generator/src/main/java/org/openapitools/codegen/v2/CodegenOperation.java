package org.openapitools.codegen.v2;

public class CodegenOperation extends CodegenObject {

    public CodegenOperation(String id, CodegenApi api) {
        super(id, api);
    }

    public void addParameter(CodegenParameter parameter) {
        addChild(parameter);
    }

    public Iterable<CodegenParameter> getParameters() {
        return getChildrenOfType(CodegenParameter.class);
    }
}
