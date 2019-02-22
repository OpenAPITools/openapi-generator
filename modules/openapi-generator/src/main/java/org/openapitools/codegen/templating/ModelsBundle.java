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

    @SuppressWarnings("unchecked")
    public ModelsBundle (Map<String, Object> value) {
        super(value);
        if (value.containsKey("packages")) {
            setPackage((String) value.get("package"));
        }
        if (value.containsKey("models")) {
            List<Map<String, Object>> models = (List<Map<String, Object>>) value.get("models");
            List<ModelBundle> modelBundles = new ArrayList<>();
            for (Map<String, Object> model : models) {
                modelBundles.add(new ModelBundle(model));
            }
            setModels(modelBundles);
        }
        if (value.containsKey("imports")) {
            setImports((List<Map<String, String>>) value.get("imports"));
        }
        if (value.containsKey("classname")) {
            setClassname((String) value.get("classname"));
        }
        if (value.containsKey("modelPackage")) {
            setModelPackage((String) value.get("modelPackage"));
        }
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
