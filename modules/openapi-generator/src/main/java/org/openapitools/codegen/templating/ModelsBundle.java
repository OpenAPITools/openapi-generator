package org.openapitools.codegen.templating;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ModelsBundle extends BaseBundle {
    private String _package;
    private List<ModelBundle> models = new ArrayList<>();
    private List<Map<String, String>> imports = new ArrayList<>();
    private String classname;
    private String modelPackage;

    public ModelsBundle (Map<String, Object> value) {
        this._package = (String) value.get("package");
        List<Map<String, Object>> models = (List<Map<String, Object>>) value.get("models");
        for (Map<String, Object> model : models) {
            this.models.add(new ModelBundle(model));
        }
        this.imports = (List<Map<String, String>>) value.get("imports");
        this.classname = (String) value.get("classname");
        this.modelPackage = (String) value.get("modelPackage");
    }

    public ModelsBundle() {
        super();
    }

    // getters and setters. Each setter puts the value in the underlying Map

    public List<ModelBundle> getModels() {
        return models;
    }

    public void setModels(List<ModelBundle> models) {
        this.models = models;
        put("models", models);
    }


    public List<Map<String, String>> getImports() {
        return imports;
    }

    public void setImports(List<Map<String, String>> imports) {
        this.imports = imports;
        put("imports", imports);
    }


    public String getPackage() {
        return _package;
    }

    public void setPackage(String _package) {
        this._package = _package;
        put("package", _package);
    }


    public String getClassname() {
        return classname;
    }

    public void setClassname(String classname) {
        this.classname = classname;
        put("classname", classname);
    }


    public String getModelPackage() {
        return modelPackage;
    }

    public void setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
        put("modelPackage", modelPackage);
    }
}
