package com.wordnik.swagger.codegen.compat;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.parameters.*;
import com.wordnik.swagger.models.properties.*;

import com.wordnik.swagger.util.*;

// legacy models
import com.wordnik.swagger.parser.*;
import com.wordnik.swagger.models.apideclaration.ApiDeclaration;
import com.wordnik.swagger.models.apideclaration.Api;
import com.wordnik.swagger.models.apideclaration.ExtendedTypedObject;
import com.wordnik.swagger.models.resourcelisting.*;
import com.wordnik.swagger.models.Method;
import com.wordnik.swagger.transform.migrate.*;
import com.wordnik.swagger.report.MessageBuilder;

import com.wordnik.swagger.io.QueryParamAuthentication;
import com.wordnik.swagger.models.apideclaration.ApiDeclaration;
import com.wordnik.swagger.models.resourcelisting.ResourceListing;

import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.deser.*; 
import com.fasterxml.jackson.databind.node.*;

import java.util.*;
import java.net.URL;
import java.io.File;


public class SwaggerLegacyLoader {
  public Swagger read(String input) {
    Swagger output = null;

    MessageBuilder migrationMessages = new MessageBuilder();
    SwaggerParser swaggerParser = new SwaggerParser();
    ResourceListing resourceListing = null;

    resourceListing = readResourceListing(input, migrationMessages);

    List<ApiDeclaration> apis = new ArrayList<ApiDeclaration>();

    if(resourceListing != null) {
      List<ApiListingReference> refs = resourceListing.getApis();
      if(refs != null) {
        for(ApiListingReference ref : refs) {
          String location = null;
          if(input.startsWith("http")) {
            // look up as url
            String pathLocation = ref.getPath();
            if(pathLocation.startsWith("http")) {
              // use as absolute url
              location = pathLocation;
            }
            else {
              if(pathLocation.startsWith("/"))
                location = input + pathLocation;
              else
                location = input + "/" + pathLocation;
            }
          }
          else {
            // file system
            File fileLocation = new File(input);
            if(ref.getPath().startsWith("/"))
              location = fileLocation.getParent() + ref.getPath();
            else
              location = fileLocation.getParent() + File.separator + ref.getPath();
          }
          ApiDeclaration apiDeclaration = readDeclaration(location, migrationMessages);
          if(apiDeclaration != null) {
            apis.add(apiDeclaration);
          }
        }
      }
      output = convert(resourceListing, apis);
    }
    return output;
  }

  public ResourceListing readResourceListing(String input, MessageBuilder messages) {
    ResourceListing output = null;
    JsonNode jsonNode = null;
    try {
      if(input.startsWith("http")) {
        jsonNode = Json.mapper().readTree(new URL(input));
      }
      else {
        jsonNode = Json.mapper().readTree(new File(input));
      }
      ResourceListingMigrator migrator = new ResourceListingMigrator();
      JsonNode transformed = migrator.migrate(messages, jsonNode);
      output = Json.mapper().convertValue(transformed, ResourceListing.class);
    }
    catch (Exception e) {
      e.printStackTrace();
    }
    return output;
  }

  public Model convertModel(com.wordnik.swagger.models.apideclaration.Model model) {
    ModelImpl output = new ModelImpl();
    output.setName(model.getId());
    output.setDescription(model.getDescription());
    if(model.getRequired() != null) {
      output.setRequired(model.getRequired());
    }
    for(String key: model.getProperties().keySet()) {
      Property prop = convertProperty(model.getProperties().get(key));
      if(prop != null)
        output.addProperty(key, prop);
    }
    return output;
  }

  public Property convertProperty(com.wordnik.swagger.models.apideclaration.ModelProperty property) {
    Property output = null;
    output = propertyFromTypedObject(property);
    output.setDescription(property.getDescription());
    return output;
  }

  public Parameter convertParameter(com.wordnik.swagger.models.apideclaration.Parameter param) {
    Parameter output = null;
    if(ParamType.PATH.equals(param.getParamType())) {
      output = new PathParameter();
    }
    else if(ParamType.QUERY.equals(param.getParamType())) {
      output = new QueryParameter();
    }
    else if(ParamType.HEADER.equals(param.getParamType())) {
      output = new HeaderParameter();
    }
    else if(ParamType.BODY.equals(param.getParamType())) {
      output = new BodyParameter();
    }
    else if(ParamType.FORM.equals(param.getParamType())) {
      output = new FormParameter();
    }

    output.setName(param.getName());
    output.setDescription(param.getDescription());
    output.setRequired(param.getRequired());

    Property property = null;
    String type = param.getType() == null ? null : param.getType().toString();
    String format = param.getFormat() == null ? null : param.getFormat().toString();

    if(output instanceof BodyParameter) {
      BodyParameter bp = (BodyParameter) output;
      bp.setSchema(modelFromExtendedTypedObject(param));
    }
    else if (output instanceof SerializableParameter) {
      SerializableParameter sp = (SerializableParameter) output;
      Property p = null;
      if(param.getAllowMultiple() != null && param.getAllowMultiple() == true) {
        ArrayProperty arrayProperty = new ArrayProperty();
        Property innerType = PropertyBuilder.build(type, format, null);
        arrayProperty.setItems(innerType);
        p = arrayProperty;
      }
      else {
        p = propertyFromTypedObject(param);
      }
      if(p instanceof ArrayProperty) {
        ArrayProperty ap = (ArrayProperty) p;
        sp.setType("array");
        sp.setItems(ap.getItems());
      }
      else {
        sp.setType(p.getType());
        sp.setFormat(p.getFormat());
      }
    }
    return output;
  }

  public Model modelFromExtendedTypedObject(ExtendedTypedObject obj) {
    String type = obj.getType() == null ? null : obj.getType().toString();
    String format = obj.getFormat() == null ? null : obj.getFormat().toString();

    Model output = null;
    if(obj.getRef() != null) {
      output = new RefModel(obj.getRef());
    }
    else {
      if("array".equals(type)) {
        ArrayModel am = new ArrayModel();
        com.wordnik.swagger.models.apideclaration.Items items = obj.getItems();
        type = items.getType() == null ? null : items.getType().toString();
        format = items.getFormat() == null ? null : items.getFormat().toString();

        Property innerType = PropertyBuilder.build(type, format, null);
        if(innerType != null)
          am.setItems(innerType);
        else if(items.getRef() != null) {
          System.out.println("using ref " + items.getRef());
          am.setItems(new RefProperty(items.getRef()));
        }
        else
          am.setItems(new RefProperty(type));
        output = am;
      }
      else {
        Property input = PropertyBuilder.build(type, format, null);
        if(input == null && !"void".equals(type)) {
          //use ref model
          output = new RefModel(type);
        }
      }
    }
    return output;
  }

  public Property propertyFromTypedObject(ExtendedTypedObject obj) {
    String type = obj.getType() == null ? null : obj.getType().toString();
    String format = obj.getFormat() == null ? null : obj.getFormat().toString();

    Property output = null;
    if("array".equals(type)) {
      ArrayProperty am = new ArrayProperty();
      com.wordnik.swagger.models.apideclaration.Items items = obj.getItems();
      type = items.getType() == null ? null : items.getType().toString();
      format = items.getFormat() == null ? null : items.getFormat().toString();

      Property innerType = PropertyBuilder.build(type, format, null);
      if(innerType != null)
        am.setItems(innerType);
      else if(items.getRef() != null) {
        am.setItems(new RefProperty(items.getRef()));
      }
      else
        am.setItems(new RefProperty(type));
      output = am;
    }
    else {
      Property i = PropertyBuilder.build(type, format, null);
      if(i != null)
        output = i;
      else if(obj.getRef() != null)
        output = new RefProperty(obj.getRef());
      else
        output = new RefProperty(type);
    }

    return output;
  }

  public Operation convertOperation(String tag, com.wordnik.swagger.models.apideclaration.Operation operation) {
    Operation output = new Operation()
      .summary(operation.getSummary())
      .description(operation.getNotes())
      .operationId(operation.getNickname());

    if(tag != null)
      output.tag(tag);

    for(com.wordnik.swagger.models.apideclaration.Parameter parameter : operation.getParameters())
      output.parameter(convertParameter(parameter));

    if(operation.getConsumes() != null) {
      for(String consumes: operation.getConsumes()) {
        output.consumes(consumes);
      }
    }
    if(operation.getProduces() != null) {
      for(String produces: operation.getProduces()) {
        output.produces(produces);
      }
    }
    // default response type
    String type = operation.getType() == null ? null : operation.getType().toString();
    String format = operation.getFormat() == null ? null : operation.getFormat().toString();

    for(com.wordnik.swagger.models.apideclaration.ResponseMessage message: operation.getResponseMessages()) {
      Response response = new Response().description(message.getMessage());

      Model responseModel = null;
      if(message.getResponseModel() != null) {
        response.schema(new RefProperty(message.getResponseModel()));
      }
      output.response(message.getCode(), response);
    }

    Model responseModel = modelFromExtendedTypedObject(operation);
    if(responseModel != null) {
      Response response = new Response()
        .description("success");
      if(output.getResponses() == null)
        output.defaultResponse(response);
    }

  // protected List<Map<String, List<String>>> security;
  // protected String example;
  // protected ExternalDocs externalDocs;
    return output;
  }

  public ApiDeclaration readDeclaration(String input, MessageBuilder messages) {
    ApiDeclaration output = null;
    try {
      JsonNode jsonNode = null;
      if(input.startsWith("http")) {
        jsonNode = Json.mapper().readTree(new URL(input));
      }
      else {
        jsonNode = Json.mapper().readTree(new java.io.File(input));
      }

      ApiDeclarationMigrator migrator = new ApiDeclarationMigrator();
      JsonNode transformed = migrator.migrate(messages, jsonNode);
      output = Json.mapper().convertValue(transformed, ApiDeclaration.class);
    }
    catch (Exception e) {
      e.printStackTrace();
    }
    return output;
  }

  public Swagger convert(ResourceListing resourceListing, List<ApiDeclaration> apiDeclarations) {
    Info info = new Info();
    if(resourceListing.getInfo() != null) {
      ApiInfo apiInfo = resourceListing.getInfo();
      Contact contact = null;
      if(apiInfo.getContact() != null) {
        contact = new Contact()
          .url(apiInfo.getContact());
      }
      License license = null;
      if(apiInfo.getLicense() != null) {
        license = new License()
          .name(apiInfo.getLicense())
          .url(apiInfo.getLicenseUrl());
      }
      info = new Info()
        .description(apiInfo.getDescription())
        .version(resourceListing.getApiVersion())
        .title(apiInfo.getTitle())
        .termsOfService(apiInfo.getTermsOfServiceUrl())
        .contact(contact)
        .license(license);
    }
    Map<String, Path> paths = new HashMap<String, Path>();
    Map<String, Model> definitions = new HashMap<String, Model>();
    String basePath = null;

    for(ApiDeclaration apiDeclaration : apiDeclarations) {
      String tag = apiDeclaration.getResourcePath();
      if(tag != null) {
        tag = tag.replaceAll("/", "");
      }
      if(basePath != null) {
        if(!basePath.equals(apiDeclaration.getBasePath()) && apiDeclaration.getBasePath() != null) {
          System.out.println("warning!  multiple basePath values not supported!");
        }
      }
      else
        basePath = apiDeclaration.getBasePath();

      List<Api> apis = apiDeclaration.getApis();
      for(Api api : apis) {
        String apiPath = api.getPath();
        String description = api.getDescription();
        List<com.wordnik.swagger.models.apideclaration.Operation> ops = api.getOperations();

        Path path = paths.get(apiPath);
        if(path == null) {
          path = new Path();
          paths.put(apiPath, path);
        }
        for(com.wordnik.swagger.models.apideclaration.Operation op : ops) {
          Operation operation = convertOperation(tag, op);
          path.set(op.getMethod().toString().toLowerCase(), operation);
        }
      }

      // model definitions
      Map<String, com.wordnik.swagger.models.apideclaration.Model> apiModels = apiDeclaration.getModels();
      for(String key: apiModels.keySet()) {
        Model model = convertModel(apiModels.get(key));
          definitions.put(key, model);
      }
    }

    String host = null;
    String scheme = null;

    if(basePath != null) {
      String[] parts = basePath.split("://");
      if(parts.length == 2) {
        scheme = parts[0];
        int pos = parts[1].indexOf("/");
        if(pos != -1) {
          host = parts[1].substring(0, 1);
          basePath = parts[1].substring(1);
        }
      }
      if(!basePath.startsWith("/"))
        basePath = "/" + basePath;
    }

    Swagger swagger = new Swagger()
      .host(host)
      .scheme(Scheme.forValue(scheme))
      .basePath(basePath)
      .info(info)
      // .securityRequirement(securityRequirement)
      .paths(paths)
      // .securityDefinitions(name, securityScheme)
      .basePath(basePath);
    swagger.setDefinitions(definitions);
    // host is read from the api declarations
    return swagger;
  }
}
