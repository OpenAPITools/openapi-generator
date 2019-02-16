package org.openapitools.codegen.templating;

import org.openapitools.codegen.CodegenModel;

import java.util.Map;

public class ModelBundle extends BaseBundle{
    CodegenModel model;
    String importPath;

    public ModelBundle(Map<String, Object> model) {
        super(model);
        if (model.containsKey("model")) {
            setModel((CodegenModel) model.get("model"));
        }
        if (model.containsKey("importPath")) {
            setImportPath((String) model.get("importPath"));
        }
    }

    public ModelBundle() {
        super();
    }

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
