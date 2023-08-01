package org.openapitools.codegen.model;

import java.util.HashMap;
import java.util.Map;

import org.openapitools.codegen.CodegenModel;

public class ModelMap extends HashMap<String, Object> {

    public ModelMap() {

    }

    public ModelMap(Map<String, Object> init) {
        putAll(init);
    }

    public void setModel(CodegenModel model) {
        put("model", model);
    }

    public CodegenModel getModel() {
        return (CodegenModel) get("model");
    }
}
