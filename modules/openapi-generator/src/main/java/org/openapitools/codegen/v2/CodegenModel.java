package org.openapitools.codegen.v2;

public class CodegenModel extends CodegenObject {
    public CodegenModel(String id, CodegenSdk sdk) {
        super(id, sdk);
    }

    public void addProperty(CodegenProperty property) {
        addChild(property);
    }

    public Iterable<CodegenProperty> getProperties() {
        return getChildrenOfType(CodegenProperty.class);
    }
}
