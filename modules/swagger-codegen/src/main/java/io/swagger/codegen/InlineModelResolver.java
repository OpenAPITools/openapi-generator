package io.swagger.codegen;

import com.sun.org.apache.xpath.internal.operations.Mod;
import io.swagger.models.*;
import io.swagger.models.parameters.BodyParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.parameters.RefParameter;
import io.swagger.models.properties.*;
import io.swagger.util.Json;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class InlineModelResolver {
    private Swagger swagger = null;
    private boolean skipMatches = false;

    Map<String, Model> addedModels = new HashMap<String, Model>();
    Map<String, String> generatedSignature = new HashMap<String, String>();

    public void flatten(Swagger swagger) {
        this.swagger = swagger;

        if (swagger.getDefinitions() == null) {
            swagger.setDefinitions(new HashMap<String, Model>());
        }

        // operations
        Map<String, Path> paths = swagger.getPaths();
        Map<String, Model> models = swagger.getDefinitions();

        if (paths != null) {
            for (String pathname : paths.keySet()) {
                Path path = paths.get(pathname);

                for (Operation operation : path.getOperations()) {
                    List<Parameter> parameters = operation.getParameters();

                    if (parameters != null) {
                        for (Parameter parameter : parameters) {
                            if (parameter instanceof BodyParameter) {
                                BodyParameter bp = (BodyParameter) parameter;
                                if (bp.getSchema() != null) {
                                    Model model = bp.getSchema();
                                    if(model instanceof ModelImpl) {
                                        ModelImpl obj = (ModelImpl) model;
                                        if (obj.getType() == null || "object".equals(obj.getType())) {
                                            String modelName = uniqueName(bp.getName());
                                            flattenProperties(obj.getProperties(), pathname);

                                            bp.setSchema(new RefModel(modelName));
                                            addGenerated(modelName, model);
                                            swagger.addDefinition(modelName, model);
                                        }
                                    }
                                    else if (model instanceof ArrayModel) {
                                        ArrayModel am = (ArrayModel) model;
                                        Property inner = am.getItems();

                                        if(inner instanceof ObjectProperty) {
                                            ObjectProperty op = (ObjectProperty) inner;
                                            flattenProperties(op.getProperties(), pathname);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Map<String, Response> responses = operation.getResponses();
                    if (responses != null) {
                        for (String key : responses.keySet()) {
                            Response response = responses.get(key);
                            if (response.getSchema() != null) {
                                Property property = response.getSchema();
                                if (property instanceof ObjectProperty) {
                                    String modelName = uniqueName("inline_response_" + key);
                                    ObjectProperty op = (ObjectProperty) property;
                                    Model model = modelFromProperty(op, modelName);
                                    String existing = matchGenerated(model);
                                    if (existing != null) {
                                        response.setSchema(new RefProperty(existing));
                                    } else {
                                        response.setSchema(new RefProperty(modelName));
                                        addGenerated(modelName, model);
                                        swagger.addDefinition(modelName, model);
                                    }
                                } else if (property instanceof ArrayProperty) {
                                    ArrayProperty ap = (ArrayProperty) property;
                                    if(ap.getItems() instanceof ObjectProperty) {
                                        ObjectProperty op = (ObjectProperty) ap.getItems();
                                        Map<String, Property> props = op.getProperties();
                                        flattenProperties(props, "path");
                                    }
                                } else if (property instanceof MapProperty) {
                                    MapProperty op = (MapProperty) property;

                                    Property innerProperty = op.getAdditionalProperties();
                                    if(innerProperty instanceof ObjectProperty) {
                                        ModelImpl innerModel = new ModelImpl();
                                        // TODO: model props
                                        innerModel.setTitle(property.getTitle());
                                        property.getVendorExtensions();
                                        property.getRequired();
                                        property.getReadOnly();
                                        property.getAccess();
                                        innerModel.setDescription(property.getDescription());
                                        innerModel.setExample(property.getExample());
                                        innerModel.setName(property.getName());
                                        innerModel.setXml(property.getXml());

                                        innerModel.setAdditionalProperties(innerProperty);

                                        String modelName = uniqueName("inline_response_" + key);
                                        String existing = matchGenerated(innerModel);
                                        if (existing != null) {
                                            response.setSchema(new RefProperty(existing));
                                        } else {
                                            response.setSchema(new RefProperty(modelName));
                                            addGenerated(modelName, innerModel);
                                            swagger.addDefinition(modelName, innerModel);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // definitions
        if (models != null) {
            List<String> modelNames = new ArrayList<String>(models.keySet());
            for (String modelName : modelNames) {
                Model model = models.get(modelName);
                if (model instanceof ModelImpl) {
                    ModelImpl m = (ModelImpl) model;

                    Map<String, Property> properties = m.getProperties();
                    flattenProperties(properties, modelName);

                } else if (model instanceof ArrayModel) {
                    ArrayModel m = (ArrayModel) model;
                    Property inner = m.getItems();
                    if (inner instanceof ObjectProperty) {
                        String innerModelName = uniqueName(modelName + "_inner");
                        Model innerModel = modelFromProperty((ObjectProperty) inner, modelName);

                        String existing = matchGenerated(innerModel);
                        if (existing == null) {
                            swagger.addDefinition(innerModelName, innerModel);
                            addGenerated(innerModelName, innerModel);
                            m.setItems(new RefProperty(innerModelName));
                        } else {
                            m.setItems(new RefProperty(existing));
                        }
                    }
                } else if (model instanceof ComposedModel) {
                    ComposedModel m = (ComposedModel) model;
                }
            }
        }
    }

    public String matchGenerated(Model model) {
        if (this.skipMatches) {
            return null;
        }
        String json = Json.pretty(model);
        if (generatedSignature.containsKey(json)) {
            return generatedSignature.get(json);
        }
        return null;
    }

    public void addGenerated(String name, Model model) {
        generatedSignature.put(Json.pretty(model), name);
    }

    public String uniqueName(String key) {
        int count = 0;
        boolean done = false;
        key = key.replaceAll("[^a-z_\\.A-Z0-9 ]", "");
        while (!done) {
            String name = key;
            if (count > 0) {
                name = key + "_" + count;
            }
            if (swagger.getDefinitions() == null) {
                return name;
            } else if (!swagger.getDefinitions().containsKey(name)) {
                return name;
            }
            count += 1;
        }
        return key;
    }

    public void flattenProperties(Map<String, Property> properties, String path) {
        if (properties == null) {
            return;
        }
        Map<String, Property> propsToUpdate = new HashMap<String, Property>();
        Map<String, Model> modelsToAdd = new HashMap<String, Model>();
        for (String key : properties.keySet()) {
            Property property = properties.get(key);
            if (property instanceof ObjectProperty && ((ObjectProperty)property).getProperties().size() > 0) {
                String modelName = uniqueName(path + "_" + key);

                ObjectProperty op = (ObjectProperty) property;
                Model model = modelFromProperty(op, modelName);

                String existing = matchGenerated(model);

                if (existing != null) {
                    propsToUpdate.put(key, new RefProperty(existing));
                } else {
                    propsToUpdate.put(key, new RefProperty(modelName));
                    modelsToAdd.put(modelName, model);
                    addGenerated(modelName, model);
                    swagger.addDefinition(modelName, model);
                }
            }
        }
        if (propsToUpdate.size() > 0) {
            for (String key : propsToUpdate.keySet()) {
                properties.put(key, propsToUpdate.get(key));
            }
        }
        for (String key : modelsToAdd.keySet()) {
            swagger.addDefinition(key, modelsToAdd.get(key));
            this.addedModels.put(key, modelsToAdd.get(key));
        }
    }

    public Model modelFromProperty(ArrayProperty object, String path) {
        String access = object.getAccess();
        String description = object.getDescription();
        String example = object.getExample();
        String name = object.getName();
        Integer position = object.getPosition();
        Boolean readOnly = object.getReadOnly();
        Boolean required = object.getRequired();
        String title = object.getTitle();
        Map<String, Object> extensions = object.getVendorExtensions();
        Xml xml = object.getXml();

//        object.getItems()
//        Map<String, Property> properties = object.getProperties();

        Property inner = object.getItems();
        if (inner instanceof ObjectProperty) {
            ArrayModel model = new ArrayModel();
            model.setDescription(description);
            model.setExample(example);
//          model.setName(name);
//          model.setXml(xml);

            model.setItems(object.getItems());
            return model;
        }

//        if(properties != null) {
//            flattenProperties(properties, path);
//            model.setProperties(properties);
//        }

        return null;
    }

    public Model modelFromProperty(ObjectProperty object, String path) {
        String access = object.getAccess();
        String description = object.getDescription();
        String example = object.getExample();
        String name = object.getName();
        Integer position = object.getPosition();
        Boolean readOnly = object.getReadOnly();
        Boolean required = object.getRequired();
        String title = object.getTitle();
        Map<String, Object> extensions = object.getVendorExtensions();
        Xml xml = object.getXml();

        Map<String, Property> properties = object.getProperties();

        ModelImpl model = new ModelImpl();
        model.setDescription(description);
        model.setExample(example);
        model.setName(name);
        model.setXml(xml);

        if (properties != null) {
            flattenProperties(properties, path);
            model.setProperties(properties);
        }

        return model;
    }

    public Model modelFromProperty(MapProperty object, String path) {
        String access = object.getAccess();
        String description = object.getDescription();
        String example = object.getExample();
        String name = object.getName();
        Integer position = object.getPosition();
        Boolean readOnly = object.getReadOnly();
        Boolean required = object.getRequired();
        String title = object.getTitle();
        Map<String, Object> extensions = object.getVendorExtensions();
        Xml xml = object.getXml();

        ArrayModel model = new ArrayModel();
        model.setDescription(description);
        model.setExample(example);
        model.setItems(object.getAdditionalProperties());

        return model;
    }

    public boolean isSkipMatches() {
        return skipMatches;
    }

    public void setSkipMatches(boolean skipMatches) {
        this.skipMatches = skipMatches;
    }
}
