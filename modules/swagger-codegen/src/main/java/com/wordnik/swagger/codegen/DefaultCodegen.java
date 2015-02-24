package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.parameters.*;
import com.wordnik.swagger.models.properties.*;
import com.wordnik.swagger.util.Json;

import org.apache.commons.lang.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.io.File;

public class DefaultCodegen {
  Logger LOGGER = LoggerFactory.getLogger(DefaultCodegen.class);

  protected String outputFolder = "";
  protected Set<String> defaultIncludes = new HashSet<String>();
  protected Map<String, String> typeMapping = new HashMap<String, String>();
  protected Map<String, String> instantiationTypes = new HashMap<String, String>();
  protected Set<String> reservedWords = new HashSet<String>();
  protected Set<String> languageSpecificPrimitives = new HashSet<String>();
  protected Map<String, String> importMapping = new HashMap<String, String>();
  protected String modelPackage = "", apiPackage = "", fileSuffix;
  protected Map<String, String> apiTemplateFiles = new HashMap<String, String>();
  protected Map<String, String> modelTemplateFiles = new HashMap<String, String>();
  protected String templateDir;
  protected Map<String, Object> additionalProperties = new HashMap<String, Object>();
  protected List<SupportingFile> supportingFiles = new ArrayList<SupportingFile>();

  public void processOpts(){
    if(additionalProperties.containsKey("templateDir")) {
      this.setTemplateDir((String)additionalProperties.get("templateDir"));
    }
  }

  // override with any special post-processing
  public Map<String, Object> postProcessModels(Map<String, Object> objs) {
    return objs;
  }

  // override with any special post-processing
  public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
    return objs;
  }

  // override with any special post-processing
  public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
    return objs;
  }

  // override with any special handling of the entire swagger spec
  public void processSwagger(Swagger swagger) {}
  
  // override with any special text escaping logic
  public String escapeText(String input) {
    if(input != null) {
      String output = input.replaceAll("\n", " ");
      output = output.replace("\"", "\\\"");
      return output;
    }
    return input;
  }

  public Set<String> defaultIncludes() {
    return defaultIncludes;
  }
  public Map<String, String> typeMapping() {
    return typeMapping;
  }
  public Map<String, String> instantiationTypes() {
    return instantiationTypes;
  }
  public Set<String> reservedWords() {
    return reservedWords;
  }
  public Set<String> languageSpecificPrimitives() {
    return languageSpecificPrimitives;
  }
  public Map<String, String> importMapping() {
    return importMapping;
  }
  public String modelPackage() {
    return modelPackage;
  }
  public String apiPackage() {
    return apiPackage;
  }
  public String fileSuffix() {
    return fileSuffix;
  }
  public String templateDir() {
    return templateDir;
  }
  public Map<String, String> apiTemplateFiles() {
    return apiTemplateFiles;
  }
  public Map<String, String> modelTemplateFiles() {
    return modelTemplateFiles;
  }

  public String apiFileFolder() {
    return outputFolder + "/" + apiPackage().replace('.', File.separatorChar);
  }

  public String modelFileFolder() {
    return outputFolder + "/" + modelPackage().replace('.', File.separatorChar);
  }

  public Map<String, Object> additionalProperties() {
    return additionalProperties;
  }
  public List<SupportingFile> supportingFiles() {
    return supportingFiles;
  }
  public String outputFolder() {
    return outputFolder;
  }

  public void setOutputDir(String dir) {
    this.outputFolder = dir;
  }
  public String getOutputDir() {
    return outputFolder();
  }

  public void setTemplateDir(String templateDir) {
    this.templateDir = templateDir;
  }

  public String toApiFilename(String name) {
    return initialCaps(name) + "Api";
  }

  public String toApiVarName(String name) {
    return snakeCase(name);
  }

  public String toModelFilename(String name) {
    return name;
  }

  public String toVarName(String name) {
    if(reservedWords.contains(name))
      return escapeReservedWord(name);
    else
      return name;
  }

  public String toParamName(String name) {
    if(reservedWords.contains(name)) {
      return escapeReservedWord(name);
    }
    return name;
  }

  public String escapeReservedWord(String name) {
    throw new RuntimeException("reserved word " + name + " not allowed");
  }

  public String toModelImport(String name) {
    if("".equals(modelPackage()))
      return name;
    else
      return modelPackage() + "." + name;
  }

  public String toApiImport(String name) {
    return apiPackage() + "." + name;
  }

  public DefaultCodegen() {
    defaultIncludes = new HashSet<String>(
      Arrays.asList("double",
        "int",
        "long",
        "short",
        "char",
        "float",
        "String",
        "boolean",
        "Boolean",
        "Double",
        "Void",
        "Integer",
        "Long",
        "Float")
      );

    typeMapping = new HashMap<String, String>();
    typeMapping.put("array", "List");
    typeMapping.put("map", "Map");
    typeMapping.put("List", "List");
    typeMapping.put("boolean", "Boolean");
    typeMapping.put("string", "String");
    typeMapping.put("int", "Integer");
    typeMapping.put("float", "Float");
    typeMapping.put("number", "BigDecimal");
    typeMapping.put("DateTime", "Date");
    typeMapping.put("long", "Long");
    typeMapping.put("short", "Short");
    typeMapping.put("char", "String");
    typeMapping.put("double", "Double");
    typeMapping.put("object", "Object");
    typeMapping.put("integer", "Integer");

    instantiationTypes = new HashMap<String, String>();

    reservedWords = new HashSet<String>();

    importMapping = new HashMap<String, String>();
    importMapping.put("BigDecimal", "java.math.BigDecimal");
    importMapping.put("UUID", "java.util.UUID");
    importMapping.put("File", "java.io.File");
    importMapping.put("Date", "java.util.Date");
    importMapping.put("Timestamp", "java.sql.Timestamp");
    importMapping.put("Map", "java.util.Map");
    importMapping.put("HashMap", "java.util.HashMap");
    importMapping.put("Array", "java.util.List");
    importMapping.put("ArrayList", "java.util.ArrayList");
    importMapping.put("List", "java.util.*");
    importMapping.put("Set", "java.util.*");
    importMapping.put("DateTime", "org.joda.time.*");
    importMapping.put("LocalDateTime", "org.joda.time.*");
    importMapping.put("LocalDate", "org.joda.time.*");
    importMapping.put("LocalTime", "org.joda.time.*");
  }

  public String toInstantiationType(Property p) {
    if (p instanceof MapProperty) {
      MapProperty ap = (MapProperty) p;
      String inner = getSwaggerType(ap.getAdditionalProperties());
      return instantiationTypes.get("map") + "<String, " + inner + ">";
    }
    else if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      String inner = getSwaggerType(ap.getItems());
      return instantiationTypes.get("array") + "<" + inner + ">";
    }
    else
      return null;
  }

  public String toDefaultValue(Property p) {
    if(p instanceof StringProperty)
      return "null";
    else if (p instanceof BooleanProperty)
      return "null";
    else if(p instanceof DateProperty)
      return "null";
    else if(p instanceof DateTimeProperty)
      return "null";
    else if (p instanceof DoubleProperty)
      return "null";
    else if (p instanceof FloatProperty)
      return "null";
    else if (p instanceof IntegerProperty)
      return "null";
    else if (p instanceof LongProperty)
      return "null";
    else if (p instanceof MapProperty) {
      MapProperty ap = (MapProperty) p;
      String inner = getSwaggerType(ap.getAdditionalProperties());
      return "new HashMap<String, " + inner + ">() ";
    }
    else if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      String inner = getSwaggerType(ap.getItems());
      return "new ArrayList<" + inner + ">() ";
    }
    else
      return "null";
  }

  /**
   * returns the swagger type for the property
   **/
  public String getSwaggerType(Property p) {
    String datatype = null;
    if(p instanceof StringProperty)
      datatype = "string";
    else if (p instanceof BooleanProperty)
      datatype = "boolean";
    else if(p instanceof DateProperty)
      datatype = "date";
    else if(p instanceof DateTimeProperty)
      datatype = "DateTime";
    else if (p instanceof DoubleProperty)
      datatype = "double";
    else if (p instanceof FloatProperty)
      datatype = "float";
    else if (p instanceof IntegerProperty)
      datatype = "integer";
    else if (p instanceof LongProperty)
      datatype = "long";
    else if (p instanceof MapProperty)
      datatype = "map";
    else if (p instanceof DecimalProperty)
      datatype = "number";
    else if (p instanceof RefProperty) {
      RefProperty r = (RefProperty)p;
      datatype = r.get$ref();
      if(datatype.indexOf("#/definitions/") == 0)
        datatype = datatype.substring("#/definitions/".length());
    }
    else {
      if(p != null) datatype = p.getType();
    }
    return datatype;
  }

  public String snakeCase(String name) {
    return Character.toLowerCase(name.charAt(0)) + name.substring(1);
  }

  public String initialCaps(String name) {
    return Character.toUpperCase(name.charAt(0)) + name.substring(1);
  }

  public String getTypeDeclaration(String name) {
    return name;
  }

  public String getTypeDeclaration(Property p) {
    String swaggerType = getSwaggerType(p);
    if(typeMapping.containsKey(swaggerType))
      return typeMapping.get(swaggerType);
    return swaggerType;
  }

  public String toApiName(String name) {
    if(name.length() == 0)
      return "DefaultApi";
    return initialCaps(name) + "Api";
  }

  public String toModelName(String name) {
    return initialCaps(name);
  }

  public CodegenModel fromModel(String name, Model model) {
    CodegenModel m = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
    if(reservedWords.contains(name))
      m.name = escapeReservedWord(name);
    else
      m.name = name;
    m.description = model.getDescription();
    m.classname = toModelName(name);
    m.classVarName = toVarName(name);
    m.modelJson = Json.pretty(model);
    m.externalDocs = model.getExternalDocs();
    int count = 0;
    if(model instanceof ArrayModel) {
      ArrayModel am = (ArrayModel) model;
      ArrayProperty arrayProperty = new ArrayProperty(am.getItems());
      CodegenProperty cp = fromProperty(name, arrayProperty);
      if(cp.complexType != null && !defaultIncludes.contains(cp.complexType))
        m.imports.add(cp.complexType);
      m.parent = toInstantiationType(arrayProperty);
      String containerType = cp.containerType;
      if(instantiationTypes.containsKey(containerType))
        m.imports.add(instantiationTypes.get(containerType));
      if(typeMapping.containsKey(containerType)) {
        containerType = typeMapping.get(containerType);
        cp.containerType = containerType;
        m.imports.add(containerType);
      }
    }
    else if (model instanceof RefModel) {
      // TODO
    }
    else {
      ModelImpl impl = (ModelImpl) model;
      if(impl.getAdditionalProperties() != null) {
        MapProperty mapProperty = new MapProperty(impl.getAdditionalProperties());
        CodegenProperty cp = fromProperty(name, mapProperty);
        if(cp.complexType != null && !defaultIncludes.contains(cp.complexType))
          m.imports.add(cp.complexType);
        m.parent = toInstantiationType(mapProperty);
        String containerType = cp.containerType;
        if(instantiationTypes.containsKey(containerType))
          m.imports.add(instantiationTypes.get(containerType));
        if(typeMapping.containsKey(containerType)) {
          containerType = typeMapping.get(containerType);
          cp.containerType = containerType;
          m.imports.add(containerType);
        }
      }
      if(impl.getProperties() != null && impl.getProperties().size() > 0) {
        m.hasVars = true;
        for(String key: impl.getProperties().keySet()) {
          Property prop = impl.getProperties().get(key);

          if(prop == null) {
            LOGGER.warn("null property for " + key);
          }
          else {
            CodegenProperty cp = fromProperty(key, prop);
            cp.required = false;
            if(impl.getRequired() != null) {
              for(String req : impl.getRequired()) {
                if(key.equals(req))
                  cp.required = true;
              }
            }
            if(cp.complexType != null && !defaultIncludes.contains(cp.complexType)) {
              m.imports.add(cp.complexType);
            }
            m.vars.add(cp);
            count += 1;
            if(count != impl.getProperties().keySet().size())
              cp.hasMore = new Boolean(true);
            if(cp.isContainer != null) {
              String arrayImport = typeMapping.get("array");
              if(arrayImport != null &&
                !languageSpecificPrimitives.contains(arrayImport) && 
                !defaultIncludes.contains(arrayImport))
                m.imports.add(arrayImport);
            }

            if(cp.complexType != null &&
              !languageSpecificPrimitives.contains(cp.complexType) && 
              !defaultIncludes.contains(cp.complexType))
              m.imports.add(cp.complexType);

            if(cp.baseType != null &&
              !languageSpecificPrimitives.contains(cp.baseType) && 
              !defaultIncludes.contains(cp.baseType))
              m.imports.add(cp.baseType);
          }
        }
      }
      else {
        m.emptyVars = true;
      }
    }
    return m;
  }

  public CodegenProperty fromProperty(String name, Property p) {
    if(p == null) {
      LOGGER.error("unexpected missing property for name " + null);
      return null;
    }
    CodegenProperty property = CodegenModelFactory.newInstance(CodegenModelType.PROPERTY);

    property.name = toVarName(name);
    property.baseName = name;
    property.description = escapeText(p.getDescription());
    property.getter = "get" + initialCaps(name);
    property.setter = "set" + initialCaps(name);
    property.example = p.getExample();
    property.defaultValue = toDefaultValue(p);

    String type = getSwaggerType(p);

    if(p instanceof AbstractNumericProperty) {
      AbstractNumericProperty np = (AbstractNumericProperty) p;
      property.minimum = np.getMinimum();
      property.maximum = np.getMaximum();
      property.exclusiveMinimum = np.getExclusiveMinimum();
      property.exclusiveMaximum = np.getExclusiveMaximum();

      // legacy support
      Map<String, Object> allowableValues = new HashMap<String, Object>();
      if(np.getMinimum() != null)
        allowableValues.put("min", np.getMinimum());
      if(np.getMaximum() != null)
        allowableValues.put("max", np.getMaximum());
      property.allowableValues = allowableValues;
    }

    if(p instanceof StringProperty) {
      StringProperty sp = (StringProperty) p;
      property.maxLength = sp.getMaxLength();
      property.minLength = sp.getMinLength();
      property.pattern = sp.getPattern();
      if(sp.getEnum() != null) {
        List<String> _enum = sp.getEnum();
        property._enum = _enum;
        property.isEnum = true;

        // legacy support
        Map<String, Object> allowableValues = new HashMap<String, Object>();
        allowableValues.put("values", _enum);
        property.allowableValues = allowableValues;
      }
    }

    property.datatype = getTypeDeclaration(p);

    // this can cause issues for clients which don't support enums
    if(property.isEnum)
      property.datatypeWithEnum = StringUtils.capitalize(property.name) + "Enum";
    else
      property.datatypeWithEnum = property.datatype;

    property.baseType = getSwaggerType(p);

    if(p instanceof ArrayProperty) {
      property.isContainer = true;
      property.containerType = "array";
      ArrayProperty ap = (ArrayProperty) p;
      CodegenProperty cp = fromProperty("inner", ap.getItems());
      if(cp == null) {
        LOGGER.warn("skipping invalid property " + Json.pretty(p));
      }
      else {
        property.baseType = getSwaggerType(p);
        if(!languageSpecificPrimitives.contains(cp.baseType))
          property.complexType = cp.baseType;
        else
          property.isPrimitiveType = true;
      }
    }
    else if(p instanceof MapProperty) {
      property.isContainer = true;
      property.containerType = "map";
      MapProperty ap = (MapProperty) p;
      CodegenProperty cp = fromProperty("inner", ap.getAdditionalProperties());

      property.baseType = getSwaggerType(p);
      if(!languageSpecificPrimitives.contains(cp.baseType))
        property.complexType = cp.baseType;
      else
        property.isPrimitiveType = true;
    }
    else {
      property.isNotContainer = true;
      if(languageSpecificPrimitives().contains(type))
        property.isPrimitiveType = true;
      else
        property.complexType = property.baseType;
    }
    return property;
  }

  public CodegenOperation fromOperation(String path, String httpMethod, Operation operation){
    CodegenOperation op = CodegenModelFactory.newInstance(CodegenModelType.OPERATION);
    Set<String> imports = new HashSet<String>();

    String operationId = operation.getOperationId();
    if(operationId == null) {
      String tmpPath = path;
      tmpPath = tmpPath.replaceAll("\\{", "");
      tmpPath = tmpPath.replaceAll("\\}", "");
      String[] parts = (tmpPath + "/" + httpMethod).split("/");
      StringBuilder builder = new StringBuilder();
      if("/".equals(tmpPath)) {
        // must be root tmpPath
        builder.append("root");
      }
      for(int i = 0; i < parts.length; i++) {
        String part = parts[i];
        if(part.length() > 0) {
          if(builder.toString().length() == 0)
            part = Character.toLowerCase(part.charAt(0)) + part.substring(1);
          else
            part = initialCaps(part);
          builder.append(part);
        }
      }
      operationId = builder.toString();
      LOGGER.warn("generated operationId " + operationId);
    }
    op.path = path;
    op.operationId = operationId;
    op.summary = escapeText(operation.getSummary());
    op.notes = escapeText(operation.getDescription());
    op.tags = operation.getTags();

    Response methodResponse = null;

    if(operation.getConsumes() != null && operation.getConsumes().size() > 0) {
      List<Map<String, String>> c = new ArrayList<Map<String, String>>();
      int count = 0;
      for(String key: operation.getConsumes()) {
        Map<String, String> mediaType = new HashMap<String, String>();
        mediaType.put("mediaType", key);
        count += 1;
        if (count < operation.getConsumes().size())
          mediaType.put("hasMore", "true");
        c.add(mediaType);
      }
      op.consumes = c;
      op.hasConsumes = true;
    }

    if(operation.getProduces() != null && operation.getProduces().size() > 0) {
      List<Map<String, String>> c = new ArrayList<Map<String, String>>();
      int count = 0;
      for(String key: operation.getProduces()) {
        Map<String, String> mediaType = new HashMap<String, String>();
        mediaType.put("mediaType", key);
        count += 1;
        if (count < operation.getProduces().size())
          mediaType.put("hasMore", "true");
        c.add(mediaType);
      }
      op.produces = c;
      op.hasProduces = true;
    }

    if(operation.getResponses() != null) {
      for(String responseCode: new TreeSet<String>(operation.getResponses().keySet())) {
        Response response = operation.getResponses().get(responseCode);
        if (responseCode.startsWith("2")) {
          // use the first, i.e. the smallest 2xx response status as methodResponse
          methodResponse = response;
          break;
        }
      }
      if(methodResponse == null && operation.getResponses().keySet().contains("default")) {
        methodResponse = operation.getResponses().get("default");
      }
      for(String responseCode: operation.getResponses().keySet()) {
        Response response = operation.getResponses().get(responseCode);
        if(response != methodResponse) {
          CodegenResponse r = fromResponse(responseCode, response);
          op.responses.add(r);
        }
        for(int i = 0; i < op.responses.size() - 1; i++) {
          CodegenResponse r = op.responses.get(i);
          r.hasMore = new Boolean(true);
        }
      }
    }

    if(methodResponse != null) {
     if (methodResponse.getSchema() != null) {
      CodegenProperty cm = fromProperty("response", methodResponse.getSchema());

      Property responseProperty = methodResponse.getSchema();

      if(responseProperty instanceof ArrayProperty) {
        ArrayProperty ap = (ArrayProperty) responseProperty;
        CodegenProperty innerProperty = fromProperty("response", ap.getItems());
        op.returnBaseType = innerProperty.baseType;
      }
      else {
        if(cm.complexType != null)
          op.returnBaseType = cm.complexType;
        else
          op.returnBaseType = cm.baseType;
      }
      op.examples = toExamples(methodResponse.getExamples());
      op.defaultResponse = toDefaultValue(responseProperty);
      op.returnType = cm.datatype;
      if(cm.isContainer != null) {
        op.returnContainer = cm.containerType;
        if("map".equals(cm.containerType))
          op.isMapContainer = Boolean.TRUE;
        else if ("list".equalsIgnoreCase(cm.containerType))
          op.isListContainer = Boolean.TRUE;
      }
      else
        op.returnSimpleType = true;
      if (languageSpecificPrimitives().contains(op.returnBaseType) || op.returnBaseType == null)
        op.returnTypeIsPrimitive = true;
     }
     addHeaders(methodResponse, op.responseHeaders);
    }

    if(op.returnBaseType == null) {
      op.returnTypeIsPrimitive = true;
      op.returnSimpleType = true;
    }

    if(op.returnBaseType != null &&
      !defaultIncludes.contains(op.returnBaseType) &&
      !languageSpecificPrimitives.contains(op.returnBaseType))
      imports.add(op.returnBaseType);

    List<Parameter> parameters = operation.getParameters();
    CodegenParameter bodyParam = null;
    List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> cookieParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();

    if(parameters != null) {
      for(Parameter param : parameters) {
        CodegenParameter p = fromParameter(param, imports);
        allParams.add(p);
        if(param instanceof QueryParameter) {
          p.isQueryParam = new Boolean(true);
          queryParams.add(p.copy());
        }
        else if(param instanceof PathParameter) {
          p.required = true;
          p.isPathParam = new Boolean(true);
          pathParams.add(p.copy());
        }
        else if(param instanceof HeaderParameter) {
          p.isHeaderParam = new Boolean(true);
          headerParams.add(p.copy());
        }
        else if(param instanceof CookieParameter) {
          p.isCookieParam = new Boolean(true);
          cookieParams.add(p.copy());
        }
        else if(param instanceof BodyParameter) {
          p.isBodyParam = new Boolean(true);
          bodyParam = p;
          bodyParams.add(p.copy());
        }
        else if(param instanceof FormParameter) {
          if("file".equalsIgnoreCase(((FormParameter)param).getType()))
            p.isFile = true;
          else
            p.notFile = true;
          p.isFormParam = new Boolean(true);
          formParams.add(p.copy());
        }
      }
    }
    for(String i: imports) {
      if(!defaultIncludes.contains(i) && !languageSpecificPrimitives.contains(i)){
        op.imports.add(i);
      }
    }
    op.bodyParam = bodyParam;
    op.httpMethod = httpMethod.toUpperCase();
    op.allParams = addHasMore(allParams);
    op.bodyParams = addHasMore(bodyParams);
    op.pathParams = addHasMore(pathParams);
    op.queryParams = addHasMore(queryParams);
    op.headerParams = addHasMore(headerParams);
    // op.cookieParams = cookieParams;
    op.formParams = addHasMore(formParams);
    // legacy support
    op.nickname = operationId;

    if(op.allParams.size() > 0) 
      op.hasParams = true;
    op.externalDocs = operation.getExternalDocs();

    return op;
  }

  public CodegenResponse fromResponse(String responseCode, Response response) {
    CodegenResponse r = CodegenModelFactory.newInstance(CodegenModelType.RESPONSE);
    if("default".equals(responseCode))
      r.code = "0";
    else
      r.code = responseCode;
    r.message = response.getDescription();
    r.schema = response.getSchema();
    r.examples = toExamples(response.getExamples());
    return r;
  }

  public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
    CodegenParameter p = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
    p.baseName = param.getName();
    p.description = param.getDescription();
    p.required = param.getRequired();

    if(param instanceof SerializableParameter) {
      SerializableParameter qp = (SerializableParameter) param;
      Property property = null;
      String collectionFormat = null;
      if("array".equals(qp.getType())) {
        Property inner = qp.getItems();
        if(inner == null) {
          LOGGER.warn("warning!  No inner type supplied for array parameter \"" + qp.getName() + "\", using String");
          inner = new StringProperty().description("//TODO automatically added by swagger-codegen");
        }
        property = new ArrayProperty(inner);
        collectionFormat = qp.getCollectionFormat();
        CodegenProperty pr = fromProperty("inner", inner);
        p.baseType = pr.datatype;
        imports.add(pr.baseType);
      }
      else
        property = PropertyBuilder.build(qp.getType(), qp.getFormat(), null);
      if(property == null) {
        LOGGER.warn("warning!  Property type \"" + qp.getType() + "\" not found for parameter \"" + param.getName() + "\", using String");
        property = new StringProperty().description("//TODO automatically added by swagger-codegen.  Type was " + qp.getType() + " but not supported");
      }
      CodegenProperty model = fromProperty(qp.getName(), property);
      p.collectionFormat = collectionFormat;
      p.dataType = model.datatype;
      p.paramName = toParamName(qp.getName());

      if(model.complexType != null) {
        imports.add(model.complexType);
      }
    }
    else {
      BodyParameter bp = (BodyParameter) param;
      Model model = bp.getSchema();

      if(model instanceof ModelImpl) {
        ModelImpl impl = (ModelImpl) model;
        CodegenModel cm = fromModel(bp.getName(), impl);
        if(cm.emptyVars != null && cm.emptyVars == false) {
          p.dataType = getTypeDeclaration(cm.classname);
          imports.add(p.dataType);
        }
        else {
          // TODO: missing format, so this will not always work
          Property prop = PropertyBuilder.build(impl.getType(), null, null);
          CodegenProperty cp = fromProperty("property", prop);
          if(cp != null) {
            p.dataType = cp.datatype;
          }
        }
      }
      else if(model instanceof ArrayModel) {
        // to use the built-in model parsing, we unwrap the ArrayModel
        // and get a single property from it
        ArrayModel impl = (ArrayModel) model;
        CodegenModel cm = fromModel(bp.getName(), impl);
        // get the single property
        ArrayProperty ap = new ArrayProperty().items(impl.getItems());
        CodegenProperty cp = fromProperty("inner", ap);
        if(cp.complexType != null) {
          imports.add(cp.complexType);
        }
        imports.add(cp.baseType);
        p.dataType = cp.datatype;
        p.isContainer = true;
      }
      else{
        Model sub = bp.getSchema();
        if(sub instanceof RefModel) {
          String name = ((RefModel)sub).getSimpleRef();
          if(typeMapping.containsKey(name))
            name = typeMapping.get(name);
          else {
            name = toModelName(name);
            if(defaultIncludes.contains(name)) {
              imports.add(name);
            }
            imports.add(name);
            name = getTypeDeclaration(name);
          }
          p.dataType = name;
        }
      }
      p.paramName = toParamName(bp.getName());
    }
    return p;
  }

  protected List<Map<String, String>> toExamples(Map<String, String> examples) {
    if(examples == null)
      return null;

    List<Map<String, String>> output = new ArrayList<Map<String, String>>();
    for(String key: examples.keySet()) {
      String value = examples.get(key);

      Map<String, String> kv = new HashMap<String, String>();
      kv.put("contentType", key);
      kv.put("example", value);
      output.add(kv);
    }
    return output;
  }

  private void addHeaders(Response response, List<CodegenProperty> target) {
    if (response.getHeaders() != null) {
      for (Map.Entry<String, Property> headers : response.getHeaders().entrySet()) {
        target.add(fromProperty(headers.getKey(), headers.getValue()));
      }
    }
  }

  private List<CodegenParameter> addHasMore(List<CodegenParameter> objs) {
    if(objs != null) {
      for(int i = 0; i < objs.size(); i++) {
        if(i > 0)
          objs.get(i).secondaryParam = new Boolean(true);
        if(i < objs.size() - 1)
          objs.get(i).hasMore = new Boolean(true);
      }
    }
    return objs;
  }

  private Map<String, Object> addHasMore(Map<String, Object> objs) {
    if(objs != null) {
      for(int i = 0; i < objs.size() - 1; i++) {
        if(i > 0)
          objs.put("secondaryParam", new Boolean(true));
        if(i < objs.size() - 1)
          objs.put("hasMore", true);
      }
    }
    return objs;
  }


  public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
    List<CodegenOperation> opList = operations.get(tag);
    if(opList == null) {
      opList = new ArrayList<CodegenOperation>();
      operations.put(tag, opList);
    }
    opList.add(co);
    co.baseName = tag;    
  }
}