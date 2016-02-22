package io.swagger.codegen;

import io.swagger.models.*;
import io.swagger.models.parameters.BodyParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.*;
import io.swagger.util.Json;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class InlineModelResolver {
    private Swagger swagger;
    private boolean skipMatches;
    static Logger LOGGER = LoggerFactory.getLogger(InlineModelResolver.class);

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
                                            String modelName = uniqueName(bp.getName());
                                            ObjectProperty op = (ObjectProperty) inner;
                                            flattenProperties(op.getProperties(), pathname);

                                            Model innerModel = modelFromProperty(op, modelName);
                                            String existing = matchGenerated(innerModel);
                                            if (existing != null) {
                                                am.setItems(new RefProperty(existing));
                                            } else {
                                                am.setItems(new RefProperty(modelName));
                                                addGenerated(modelName, innerModel);
                                                swagger.addDefinition(modelName, innerModel);
                                            }
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
                                    Property inner = ap.getItems();

                                    if(inner instanceof ObjectProperty) {
                                        String modelName = uniqueName("inline_response_" + key);
                                        ObjectProperty op = (ObjectProperty) inner;
                                        flattenProperties(op.getProperties(), pathname);

                                        Model innerModel = modelFromProperty(op, modelName);
                                        String existing = matchGenerated(innerModel);
                                        if (existing != null) {
                                            ap.setItems(new RefProperty(existing));
                                        } else {
                                            ap.setItems(new RefProperty(modelName));
                                            addGenerated(modelName, innerModel);
                                            swagger.addDefinition(modelName, innerModel);
                                        }
                                    }
                                } else if (property instanceof MapProperty) {
                                    MapProperty mp = (MapProperty) property;

                                    Property innerProperty = mp.getAdditionalProperties();
                                    if(innerProperty instanceof ObjectProperty) {
                                        String modelName = uniqueName("inline_response_" + key);
                                        ObjectProperty op = (ObjectProperty) innerProperty;
                                        flattenProperties(op.getProperties(), pathname);

                                        Model innerModel = modelFromProperty(op, modelName);
                                        String existing = matchGenerated(innerModel);
                                        if (existing != null) {
                                            mp.setAdditionalProperties(new RefProperty(existing));
                                        } else {
                                            mp.setAdditionalProperties(new RefProperty(modelName));
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
        key = key.replaceAll("[^a-z_\\.A-Z0-9 ]", ""); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
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
            if(property instanceof ObjectProperty && ((ObjectProperty)property).getProperties() == null) {
                MapProperty mp = new MapProperty();
                mp.setAdditionalProperties(new StringProperty());
                properties.put(key, mp);
            }
            else if (property instanceof ObjectProperty && ((ObjectProperty)property).getProperties().size() > 0) {
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
            } else if (property instanceof ArrayProperty) {
                ArrayProperty ap = (ArrayProperty) property;
                Property inner = ap.getItems();

                if (inner instanceof ObjectProperty) {
                    String modelName = uniqueName(path + "_" + key);

                    ObjectProperty op = (ObjectProperty) inner;
                    flattenProperties(op.getProperties(), path);

                    Model innerModel = modelFromProperty(op, modelName);
                    String existing = matchGenerated(innerModel);

                    if (existing != null) {
                        ap.setItems(new RefProperty(existing));
                    } else {
                        ap.setItems(new RefProperty(modelName));
                        addGenerated(modelName, innerModel);
                        swagger.addDefinition(modelName, innerModel);
                    }
                }
            } else if (property instanceof MapProperty) {
                MapProperty mp = (MapProperty) property;
                Property inner = mp.getAdditionalProperties();

                if (inner instanceof ObjectProperty) {
                    String modelName = uniqueName(path + "_" + key);

                    ObjectProperty op = (ObjectProperty) inner;
                    flattenProperties(op.getProperties(), path);

                    Model innerModel = modelFromProperty(op, modelName);
                    String existing = matchGenerated(innerModel);

                    if (existing != null) {
                        mp.setAdditionalProperties(new RefProperty(existing));
                    } else {
                        mp.setAdditionalProperties(new RefProperty(modelName));
                        addGenerated(modelName, innerModel);
                        swagger.addDefinition(modelName, innerModel);
                    }
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

    @SuppressWarnings("static-method")
    public Model modelFromProperty(ArrayProperty object, @SuppressWarnings("unused") String path) {
        String description = object.getDescription();
        String example = null;

        Object obj = object.getExample();
        if(obj != null) {
            example = obj.toString();
        }
        Property inner = object.getItems();
        if (inner instanceof ObjectProperty) {
            ArrayModel model = new ArrayModel();
            model.setDescription(description);
            model.setExample(example);
            model.setItems(object.getItems());
            return model;
        }
        return null;
    }

    public Model modelFromProperty(ObjectProperty object, String path) {
        String description = object.getDescription();
        String example = null;

        Object obj = object.getExample();
        if(obj != null) {
            example = obj.toString();
        }
        String name = object.getName();
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

    @SuppressWarnings("static-method")
    public Model modelFromProperty(MapProperty object, @SuppressWarnings("unused") String path) {
        String description = object.getDescription();
        String example = null;

        Object obj = object.getExample();
        if(obj != null) {
            example = obj.toString();
        }

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
