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

    public ModelsBundle(Map<String, Object> objs) {
        super(objs);
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
