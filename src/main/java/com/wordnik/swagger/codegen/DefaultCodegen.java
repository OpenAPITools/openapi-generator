package com.wordnik.swagger.codegen;

import com.wordnik.swagger.util.Json;
import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.parameters.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class DefaultCodegen {
  protected String outputFolder = "";
  protected Set<String> defaultIncludes = new HashSet<String>();
  protected Map<String, String> typeMapping = new HashMap<String, String>();
  protected Set<String> reservedWords = new HashSet<String>();
  protected Set<String> languageSpecificPrimitives = new HashSet<String>();
  protected Map<String, String> importMapping = new HashMap<String, String>();
  protected String modelPackage = "", apiPackage = "", fileSuffix;
  protected Map<String, String> apiTemplateFiles = new HashMap<String, String>();
  protected Map<String, String> modelTemplateFiles = new HashMap<String, String>();
  protected String templateDir;
  protected Map<String, Object> additionalProperties = new HashMap<String, Object>();

  public Set<String> defaultIncludes() {
    return defaultIncludes;
  }
  public Map<String, String> typeMapping() {
    return typeMapping;
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
    return outputFolder + File.separator + apiPackage().replaceAll("\\.", File.separator);
  }
  public String modelFileFolder() {
    return outputFolder + File.separator + modelPackage().replaceAll("\\.", File.separator);
  }
  public Map<String, Object> additionalProperties() {
    return additionalProperties;
  }

  public void setTemplateDir(String templateDir) {
    this.templateDir = templateDir;
  }

  public String toApiFilename(String name) {
    return initialCaps(name);
  }

  public String toModelFilename(String name) {
    return name;
  }

  public String toVarName(String name) {
    return name;
  }

  public String toModelImport(String name) {
    return modelPackage() + "." + name;
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
        "Integer",
        "Long",
        "Float")
      );

    typeMapping = new HashMap<String, String>();
    typeMapping.put("Array", "List");
    typeMapping.put("array", "List");
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

    reservedWords = new HashSet<String> (
      Arrays.asList(
        "abstract", "continue", "for", "new", "switch", "assert", 
        "default", "if", "package", "synchronized", "boolean", "do", "goto", "private", 
        "this", "break", "double", "implements", "protected", "throw", "byte", "else", 
        "import", "public", "throws", "case", "enum", "instanceof", "return", "transient", 
        "catch", "extends", "int", "short", "try", "char", "final", "interface", "static", 
        "void", "class", "finally", "long", "strictfp", "volatile", "const", "float", 
        "native", "super", "while")
    );

    importMapping = new HashMap<String, String> ();
    importMapping.put("BigDecimal", "java.math.BigDecimal");
    importMapping.put("UUID", "java.util.UUID");
    importMapping.put("File", "java.io.File");
    importMapping.put("Date", "java.util.Date");
    importMapping.put("Timestamp", "java.sql.Timestamp");
    importMapping.put("Array", "java.util.*");
    importMapping.put("ArrayList", "java.util.*");
    importMapping.put("List", "java.util.*");
    importMapping.put("Set", "java.util.*");
    importMapping.put("DateTime", "org.joda.time.*");
    importMapping.put("LocalDateTime", "org.joda.time.*");
    importMapping.put("LocalDate", "org.joda.time.*");
    importMapping.put("LocalTime", "org.joda.time.*");
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
    else if (p instanceof MapProperty)
      return "null";
    else if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      String inner = getSwaggerType(ap.getItems());
      return "new ArrayList<" + inner + ">() ";
    }
    else {
      System.out.println("unhandled property default value");
      // Json.prettyPrint(p);
      return "null";
    }
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
    else if (p instanceof RefProperty) {
      RefProperty r = (RefProperty)p;
      datatype = r.get$ref();
      if(datatype.indexOf("#/definitions/") == 0)
        datatype = datatype.substring("#/definitions/".length());
    }
    else {
      if(p != null)
        datatype = p.getType();
    }
    return datatype;
  }

  public String initialCaps(String name) {
    return Character.toUpperCase(name.charAt(0)) + name.substring(1);
  }

  public String getTypeDeclaration(Property p) {
    String swaggerType = getSwaggerType(p);
    if(typeMapping.containsKey(swaggerType))
      return typeMapping.get(swaggerType);
    return swaggerType;
  }

  public String toApiName(String name) {
    return initialCaps(name);
  }

  public String toModelName(String name) {
    return initialCaps(name);
  }

  public CodegenModel fromModel(String name, Model model) {
    CodegenModel m = new CodegenModel();
    m.name = name;
    m.description = model.getDescription();
    m.classname = toModelName(name);

    int count = 0;
    if(model instanceof ArrayModel) {
      ArrayModel am = (ArrayModel) model;
      ArrayProperty arrayProperty = new ArrayProperty(am.getItems());
      CodegenProperty cp = fromProperty(name, arrayProperty);
      m.vars.add(cp);
    }
    else if (model instanceof RefModel) {

    }
    else {
      ModelImpl impl = (ModelImpl) model;
      for(String key: impl.getProperties().keySet()) {
        Property prop = impl.getProperties().get(key);
        if(prop == null) {
          System.out.println("null property for " + key);
          // Json.prettyPrint(impl.getProperties());
        }
        else {
          CodegenProperty cp = fromProperty(key, prop);
          if(cp.complexType != null && !defaultIncludes.contains(cp.complexType)) {
            m.imports.add(cp.complexType);
          }
          m.vars.add(cp);
          count += 1;
          if(count != impl.getProperties().keySet().size())
            cp.hasMore = new Boolean(true);
        }
      }
    }
    return m;
  }

  public CodegenProperty fromProperty(String name, Property p) {
    CodegenProperty property = new CodegenProperty();

    property.name = toVarName(name);
    property.baseName = name;
    property.description = p.getDescription();
    property.getter = "get" + name.substring(0, 1).toUpperCase() + name.substring(1);
    property.setter = "set" + name.substring(0, 1).toUpperCase() + name.substring(1);

    property.defaultValue = toDefaultValue(p);
    property.required = p.getRequired();

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
      if(sp.getEnum() != null) {
        List<String> _enum = sp.getEnum();
        property._enum = _enum;

        // legacy support
        Map<String, Object> allowableValues = new HashMap<String, Object>();
        allowableValues.put("values", _enum);
        property.allowableValues = allowableValues;
      }
    }

    property.datatype = getTypeDeclaration(p);

    if(languageSpecificPrimitives().contains(type)) {
      property.isPrimitiveType = true;
    }
    else
      property.complexType = type;
    if(p instanceof ArrayProperty)
      property.isContainer = true;
    else
      property.isNotContainer = true;
    return property;
  }

  public CodegenOperation fromOperation(String path, String httpMethod, Operation operation){
    CodegenOperation op = new CodegenOperation();

    String operationId = operation.getOperationId();
    if(operationId == null)
      operationId = "fixme";
    op.path = path;
    op.operationId = operationId;

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
    }

    if(operation.getResponses() != null) {
      for(String responseCode: operation.getResponses().keySet()) {
        Response response = operation.getResponses().get(responseCode);
        if("200".equals(responseCode)) {
          methodResponse = response;
        }
      }
      if(methodResponse == null && operation.getResponses().keySet().contains("default"))
        methodResponse = operation.getResponses().get("default");
    }

    if(methodResponse != null && methodResponse.getSchema() != null) {
      CodegenProperty responseModel = fromProperty("response", methodResponse.getSchema());

      Property responseProperty = methodResponse.getSchema();
      if(responseProperty instanceof ArrayProperty) {
        ArrayProperty ap = (ArrayProperty) responseProperty;
        CodegenProperty innerProperty = fromProperty("response", ap.getItems());

        op.returnBaseType = innerProperty.datatype;
      }
      else
        op.returnBaseType = responseModel.datatype;

      op.returnType = responseModel.datatype;
      if(responseModel.isContainer)
        op.returnContainer = responseModel.complexType;

    }

    List<Parameter> parameters = operation.getParameters();
    List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> cookieParams = new ArrayList<CodegenParameter>();
    List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();

    if(parameters != null) {
      for(Parameter param : parameters) {
        CodegenParameter p = new CodegenParameter();
        p.baseName = param.getName();

        if(param instanceof SerializableParameter) {
          SerializableParameter qp = (SerializableParameter) param;
          Property property = null;
          String collectionFormat = null;
          if("array".equals(qp.getType())) {
            Property inner = qp.getItems();
            property = new ArrayProperty(inner);
            collectionFormat = qp.getCollectionFormat();
          }
          else
            property = PropertyBuilder.build(qp.getType(), qp.getFormat(), null);
          CodegenProperty model = fromProperty(qp.getName(), property);
          p.collectionFormat = collectionFormat;
          p.dataType = model.datatype;
          p.paramName = qp.getName();
        }
        else {
          BodyParameter bp = (BodyParameter) param;
          Model model = bp.getSchema();

          if(model instanceof ModelImpl) {
            ModelImpl impl = (ModelImpl) model;
            CodegenModel cm = fromModel(bp.getName(), impl);
            p.dataType = cm.classname;
          }
          else if(model instanceof ArrayModel) {
            // to use the built-in model parsing, we unwrap the ArrayModel
            // and get a single property from it
            ArrayModel impl = (ArrayModel) model;
            CodegenModel cm = fromModel(bp.getName(), impl);
            // get the single property
            CodegenProperty cp = cm.vars.get(0);
            p.dataType = cp.datatype;
          }
          else{
            Model sub = bp.getSchema();
            if(sub instanceof RefModel)
              p.dataType = ((RefModel)sub).getSimpleRef();
          }
          p.paramName = bp.getName();
        }
        allParams.add(p);
        if(param instanceof QueryParameter)
          queryParams.add(p);
        else if(param instanceof PathParameter)
          pathParams.add(p);
        else if(param instanceof HeaderParameter)
          headerParams.add(p);
        else if(param instanceof CookieParameter)
          cookieParams.add(p);
        else if(param instanceof BodyParameter)
          bodyParams.add(p);
        // else if(param instanceof FormParameter)
        //   formParams.add(p);
      }
    }
    op.httpMethod = httpMethod.toUpperCase();
    op.allParams = allParams;
    op.bodyParams = bodyParams;
    op.pathParams = pathParams;
    op.queryParams = queryParams;
    op.headerParams = headerParams;
    // op.cookieParams = cookieParams;
    op.formParams = formParams;
    // legacy support
    op.nickname = operationId;

    return op;
  }
}