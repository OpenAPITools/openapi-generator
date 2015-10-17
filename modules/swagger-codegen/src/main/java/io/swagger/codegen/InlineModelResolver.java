package io.swagger.codegen;

import io.swagger.models.*;
import io.swagger.models.parameters.BodyParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ObjectProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class InlineModelResolver {
    private Swagger swagger = null;

    Map<String, Model> addedModels = new HashMap<String, Model>();

    public void flatten(Swagger swagger) {
        this.swagger = swagger;

        // operations
        Map<String, Path> paths = swagger.getPaths();
        Map<String, Model> models = swagger.getDefinitions();

        if(paths != null) {
            for(String pathname : paths.keySet()) {
                Path path = paths.get(pathname);

                for(Operation operation: path.getOperations()) {
                    List<Parameter> parameters = operation.getParameters();

                    if(parameters != null) {
                        for(Parameter parameter : parameters) {
                            if(parameter instanceof BodyParameter) {
                                BodyParameter bp = (BodyParameter) parameter;

                                if(bp.getSchema() != null) {
                                    Model model = bp.getSchema();

                                    if(model instanceof ModelImpl) {
                                        String name = bp.getName();

                                        if(models == null) {
                                            models = new HashMap<String, Model>();
                                            swagger.setDefinitions(models);
                                        }
                                        else {
                                            if (swagger.getDefinitions().containsKey(bp.getName())) {
                                                name += "_" + "inline";
                                            }
                                        }
                                        swagger.addDefinition(name, model);
                                        bp.setSchema(new RefModel(name));
                                    }
                                }
                            }
                        }
                    }
                    Map<String, Response> responses = operation.getResponses();
                    if(responses != null) {
                        for(String key : responses.keySet()) {
                            Response response = responses.get(key);
                            if(response.getSchema() != null) {
                                Property property = response.getSchema();
                                if(property instanceof ObjectProperty) {
                                    String modelName = uniqueName("inline_response_" + key);
                                    ObjectProperty op = (ObjectProperty) property;
                                    Model model = modelFromProperty(op, modelName);
                                    response.setSchema(new RefProperty(modelName));
                                    swagger.addDefinition(modelName, model);
                                }
                            }
                        }
                    }
                }
            }
        }

        // definitions
        if(models != null) {
            List<String> modelNames = new ArrayList<String>(models.keySet());
            for(String modelName : modelNames) {
                Model model = models.get(modelName);
                if(model instanceof ModelImpl) {
                    ModelImpl m = (ModelImpl) model;

                    Map<String, Property> properties = m.getProperties();
                    flattenProperties(properties, modelName);
                }
                else if (model instanceof ArrayModel) {
                    ArrayModel m = (ArrayModel) model;
                    Property inner = m.getItems();
                    if(inner instanceof ObjectProperty) {
                        String innerModelName = uniqueName(modelName + "_" + inner);
                        Model innerModel = modelFromProperty((ObjectProperty)inner, modelName);
                        swagger.addDefinition(innerModelName, innerModel);
                        m.setItems(new RefProperty(innerModelName));
                    }
                }
                else if (model instanceof ComposedModel) {
                    ComposedModel m = (ComposedModel) model;
                }
            }
        }
    }

    public String uniqueName(String key) {
        int count = 0;
        boolean done = false;
        while(!done) {
            String name = key;
            if(count > 0) {
                name = key + "_" + count;
            }
            if(swagger.getDefinitions() == null) {
                return name;
            }
            else if (!swagger.getDefinitions().containsKey(name)) {
                return name;
            }
            count += 1;
        }
        return key;
    }

    public void flattenProperties(Map<String, Property> properties, String path) {
        if(properties == null) {
            return;
        }
        Map<String, Property> propsToUpdate = new HashMap<String, Property>();
        Map<String, Model> modelsToAdd = new HashMap<String, Model>();
        for(String key : properties.keySet()) {
            Property property = properties.get(key);
            if(property instanceof ObjectProperty) {
                String modelName = uniqueName(path + "_" + key);
                ObjectProperty op = (ObjectProperty) property;
                Model model = modelFromProperty(op, modelName);

                modelsToAdd.put(modelName, model);
                propsToUpdate.put(key, new RefProperty(modelName));
            }
        }
        if(propsToUpdate.size() > 0) {
            for(String key : propsToUpdate.keySet()) {
                properties.put(key, propsToUpdate.get(key));
            }
        }
        for(String key : modelsToAdd.keySet()) {
            swagger.addDefinition(key, modelsToAdd.get(key));
            this.addedModels.put(key,  modelsToAdd.get(key));
        }
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

        if(properties != null) {
            flattenProperties(properties, path);
            model.setProperties(properties);
        }

        return model;
    }
}
