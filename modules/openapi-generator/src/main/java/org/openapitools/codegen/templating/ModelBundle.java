package org.openapitools.codegen.templating;

import org.openapitools.codegen.CodegenModel;

public class ModelBundle extends BaseBundle{
    CodegenModel model;
    String importPath;

    // getters and setters. Each setter puts the value in the underlying Map

    public CodegenModel getModel() {
        return model;
    }

    public void setModel(CodegenModel model) {
        this.model = model;
        put("model", model);
    }


    public String getImportPath() {
        return importPath;
    }

    public void setImportPath(String importPath) {
        this.importPath = importPath;
        put("importPath", importPath);
    }
}
