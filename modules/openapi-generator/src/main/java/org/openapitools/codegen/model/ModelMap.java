package org.openapitools.codegen.model;

import java.util.HashMap;
import java.util.List;
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

    /**
     * Convert a list of ModelMap to map of CodegenModel.
     *
     * @param  allModels list of model map
     * @return           map of Codegen Model
     */
    static public HashMap<String, CodegenModel> toCodegenModelMap(List<ModelMap> allModels) {
        HashMap<String, CodegenModel> modelMaps = new HashMap<>();

        for (ModelMap modelMap : allModels) {
            CodegenModel m = modelMap.getModel();
            modelMaps.put(m.classname, m);
        }

        return modelMaps;
    }
}
