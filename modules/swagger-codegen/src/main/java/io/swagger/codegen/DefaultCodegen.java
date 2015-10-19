package io.swagger.codegen;

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import io.swagger.codegen.examples.ExampleGenerator;
import io.swagger.models.ArrayModel;
import io.swagger.models.ComposedModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.Operation;
import io.swagger.models.RefModel;
import io.swagger.models.Response;
import io.swagger.models.Swagger;
import io.swagger.models.auth.ApiKeyAuthDefinition;
import io.swagger.models.auth.BasicAuthDefinition;
import io.swagger.models.auth.In;
import io.swagger.models.auth.OAuth2Definition;
import io.swagger.models.auth.SecuritySchemeDefinition;
import io.swagger.models.parameters.BodyParameter;
import io.swagger.models.parameters.CookieParameter;
import io.swagger.models.parameters.FormParameter;
import io.swagger.models.parameters.HeaderParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.parameters.PathParameter;
import io.swagger.models.parameters.QueryParameter;
import io.swagger.models.parameters.SerializableParameter;
import io.swagger.models.properties.AbstractNumericProperty;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.ByteArrayProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DecimalProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.PropertyBuilder;
import io.swagger.models.properties.PropertyBuilder.PropertyId;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;
import io.swagger.util.Json;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nullable;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class DefaultCodegen {
    protected static final Logger LOGGER = LoggerFactory.getLogger(DefaultCodegen.class);

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
    protected List<CliOption> cliOptions = new ArrayList<CliOption>();
    protected boolean skipOverwrite;
    protected boolean supportsInheritance = false;
    protected Map<String, String> supportedLibraries = new LinkedHashMap<String, String>();
    protected String library = null;
    protected Boolean sortParamsByRequiredFlag = true;

    public List<CliOption> cliOptions() {
        return cliOptions;
    }

    public void processOpts() {
        if (additionalProperties.containsKey(CodegenConstants.TEMPLATE_DIR)) {
            this.setTemplateDir((String) additionalProperties.get(CodegenConstants.TEMPLATE_DIR));
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            this.setModelPackage((String) additionalProperties.get(CodegenConstants.MODEL_PACKAGE));
        }

        if (additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            this.setApiPackage((String) additionalProperties.get(CodegenConstants.API_PACKAGE));
        }

        if (additionalProperties.containsKey(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG)) {
            this.setSortParamsByRequiredFlag(Boolean.valueOf((String)additionalProperties.get(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG).toString()));
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

    //override with any special handling of the entire swagger spec
    public void preprocessSwagger(Swagger swagger) {
    }

    // override with any special handling of the entire swagger spec
    public void processSwagger(Swagger swagger) {
    }

    // override with any special text escaping logic
    public String escapeText(String input) {
        if (input != null) {
            input = input.trim();
            String output = input.replaceAll("\n", "\\\\n");
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

    public void setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
    }

    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
    }

    public void setSortParamsByRequiredFlag(Boolean sortParamsByRequiredFlag) {
        this.sortParamsByRequiredFlag = sortParamsByRequiredFlag;
    }

    public String toApiFilename(String name) {
        return toApiName(name);
    }

    public String toApiVarName(String name) {
        return snakeCase(name);
    }

    public String toModelFilename(String name) {
        return initialCaps(name);
    }

    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        return operationId;
    }

    public String toVarName(String name) {
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        } else {
            return name;
        }
    }

    public String toParamName(String name) {
        name = removeNonNameElementToCamelCase(name);
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        }
        return name;
    }

    public String toEnumName(CodegenProperty property) {
        return StringUtils.capitalize(property.name) + "Enum";
    }

    public String escapeReservedWord(String name) {
        throw new RuntimeException("reserved word " + name + " not allowed");
    }

    public String toModelImport(String name) {
        if ("".equals(modelPackage())) {
            return name;
        } else {
            return modelPackage() + "." + name;
        }
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
        typeMapping.put("ByteArray", "byte[]");


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

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC));
    }


    public String generateExamplePath(String path, Operation operation) {
        StringBuilder sb = new StringBuilder();
        sb.append(path);

        if (operation.getParameters() != null) {
            int count = 0;

            for (Parameter param : operation.getParameters()) {
                if (param instanceof QueryParameter) {
                    StringBuilder paramPart = new StringBuilder();
                    QueryParameter qp = (QueryParameter) param;

                    if (count == 0) {
                        paramPart.append("?");
                    } else {
                        paramPart.append(",");
                    }
                    count += 1;
                    if (!param.getRequired()) {
                        paramPart.append("[");
                    }
                    paramPart.append(param.getName()).append("=");
                    paramPart.append("{");
                    if (qp.getCollectionFormat() != null) {
                        paramPart.append(param.getName() + "1");
                        if ("csv".equals(qp.getCollectionFormat())) {
                            paramPart.append(",");
                        } else if ("pipes".equals(qp.getCollectionFormat())) {
                            paramPart.append("|");
                        } else if ("tsv".equals(qp.getCollectionFormat())) {
                            paramPart.append("\t");
                        } else if ("multi".equals(qp.getCollectionFormat())) {
                            paramPart.append("&").append(param.getName()).append("=");
                            paramPart.append(param.getName() + "2");
                        }
                    } else {
                        paramPart.append(param.getName());
                    }
                    paramPart.append("}");
                    if (!param.getRequired()) {
                        paramPart.append("]");
                    }
                    sb.append(paramPart.toString());
                }
            }
        }

        return sb.toString();
    }

    public String toInstantiationType(Property p) {
        if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            Property additionalProperties2 = ap.getAdditionalProperties();
            String type = additionalProperties2.getType();
            if (null == type) {
                LOGGER.error("No Type defined for Additional Property " + additionalProperties2 + "\n" //
                      + "\tIn Property: " + p);
            }
            String inner = getSwaggerType(additionalProperties2);
            return instantiationTypes.get("map") + "<String, " + inner + ">";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            return instantiationTypes.get("array") + "<" + inner + ">";
        } else {
            return null;
        }
    }

    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            return "null";
        } else if (p instanceof BooleanProperty) {
            return "null";
        } else if (p instanceof DateProperty) {
            return "null";
        } else if (p instanceof DateTimeProperty) {
            return "null";
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "null";
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "null";
        } else if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "null";
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "null";
        } else {
            return "null";
        }
    }

    /**
     * returns the swagger type for the property
     **/
    public String getSwaggerType(Property p) {
        String datatype = null;
        if (p instanceof StringProperty) {
            datatype = "string";
        } else if (p instanceof ByteArrayProperty) {
            datatype = "ByteArray";
        } else if (p instanceof BooleanProperty) {
            datatype = "boolean";
        } else if (p instanceof DateProperty) {
            datatype = "date";
        } else if (p instanceof DateTimeProperty) {
            datatype = "DateTime";
        } else if (p instanceof DoubleProperty) {
            datatype = "double";
        } else if (p instanceof FloatProperty) {
            datatype = "float";
        } else if (p instanceof IntegerProperty) {
            datatype = "integer";
        } else if (p instanceof LongProperty) {
            datatype = "long";
        } else if (p instanceof MapProperty) {
            datatype = "map";
        } else if (p instanceof DecimalProperty) {
            datatype = "number";
        } else if (p instanceof RefProperty) {
            try {
                RefProperty r = (RefProperty) p;
                datatype = r.get$ref();
                if (datatype.indexOf("#/definitions/") == 0) {
                    datatype = datatype.substring("#/definitions/".length());
                }
            } catch (Exception e) {
                LOGGER.warn("Error obtaining the datatype from RefProperty:" + p + ". Datatype default to Object");
                datatype = "Object";
                e.printStackTrace();
            }
        } else {
            if (p != null) {
                datatype = p.getType();
            }
        }
        return datatype;
    }

    public String snakeCase(String name) {
        return (name.length() > 0) ? (Character.toLowerCase(name.charAt(0)) + name.substring(1)) : "";
    }

    public String initialCaps(String name) {
        return StringUtils.capitalize(name);
    }

    public String getTypeDeclaration(String name) {
        return name;
    }

    public String getTypeDeclaration(Property p) {
        String swaggerType = getSwaggerType(p);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }
        return swaggerType;
    }

    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        return initialCaps(name) + "Api";
    }

    public String toModelName(String name) {
        return initialCaps(name);
    }

    public CodegenModel fromModel(String name, Model model) {
        return fromModel(name, model, null);
    }

    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel m = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
        if (reservedWords.contains(name)) {
            m.name = escapeReservedWord(name);
        } else {
            m.name = name;
        }
        m.description = escapeText(model.getDescription());
        m.unescapedDescription = model.getDescription();
        m.classname = toModelName(name);
        m.classVarName = toVarName(name);
        m.modelJson = Json.pretty(model);
        m.externalDocs = model.getExternalDocs();
        if (model instanceof ArrayModel) {
            ArrayModel am = (ArrayModel) model;
            ArrayProperty arrayProperty = new ArrayProperty(am.getItems());
            m.hasEnums = false; // Otherwise there will be a NullPointerException in JavaClientCodegen.fromModel
            addParentContainer(m, name, arrayProperty);
        } else if (model instanceof RefModel) {
            // TODO
        } else if (model instanceof ComposedModel) {
            final ComposedModel composed = (ComposedModel) model;
            Map<String, Property> properties = new HashMap<String, Property>();
            List<String> required = new ArrayList<String>();
            // parent model
            final RefModel parent = (RefModel) composed.getParent();
            if (parent != null) {
                final String parentRef = toModelName(parent.getSimpleRef());
                m.parent = parentRef;
                addImport(m, parentRef);
                if (!supportsInheritance && allDefinitions != null) {
                    final Model parentModel = allDefinitions.get(parentRef);
                    if (parentModel instanceof ModelImpl) {
                        final ModelImpl _parent = (ModelImpl) parentModel;
                        if (_parent.getProperties() != null) {
                            properties.putAll(_parent.getProperties());
                        }
                        if (_parent.getRequired() != null) {
                            required.addAll(_parent.getRequired());
                        }
                    }
                }
            }
            // interfaces (intermediate models)
            if (allDefinitions != null && composed.getInterfaces() != null) {
                for (RefModel _interface : composed.getInterfaces()) {
                    final String interfaceRef = toModelName(_interface.getSimpleRef());
                    final Model interfaceModel = allDefinitions.get(interfaceRef);
                    if (interfaceModel instanceof ModelImpl) {
                        final ModelImpl _interfaceModel = (ModelImpl) interfaceModel;
                        if (_interfaceModel.getProperties() != null) {
                            properties.putAll(_interfaceModel.getProperties());
                        }
                        if (_interfaceModel.getRequired() != null) {
                            required.addAll(_interfaceModel.getRequired());
                        }
                    }
                }
            }
            // child model (properties owned by the model itself)
            Model child = composed.getChild();
            if (child != null && child instanceof RefModel && allDefinitions != null) {
                final String childRef = ((RefModel) child).getSimpleRef();
                child = allDefinitions.get(childRef);
            }
            if (child != null && child instanceof ModelImpl) {
                final ModelImpl _child = (ModelImpl) child;
                if (_child.getProperties() != null) {
                    properties.putAll(_child.getProperties());
                }
                if (_child.getRequired() != null) {
                    required.addAll(_child.getRequired());
                }
            }
            addVars(m, properties, required);
        } else {
            ModelImpl impl = (ModelImpl) model;
            if (impl.getAdditionalProperties() != null) {
                MapProperty mapProperty = new MapProperty(impl.getAdditionalProperties());
                addParentContainer(m, name, mapProperty);
            }
            addVars(m, impl.getProperties(), impl.getRequired());
        }
        return m;
    }

    public String getterAndSetterCapitalize(String name) {
        if (name == null || name.length() == 0) {
            return name;
        }

        return camelize(toVarName(name));

    }

    public CodegenProperty fromProperty(String name, Property p) {
        if (p == null) {
            LOGGER.error("unexpected missing property for name " + name);
            return null;
        }
        CodegenProperty property = CodegenModelFactory.newInstance(CodegenModelType.PROPERTY);

        property.name = toVarName(name);
        property.baseName = name;
        property.description = escapeText(p.getDescription());
        property.unescapedDescription = p.getDescription();
        property.getter = "get" + getterAndSetterCapitalize(name);
        property.setter = "set" + getterAndSetterCapitalize(name);
        property.example = p.getExample();
        property.defaultValue = toDefaultValue(p);
        property.jsonSchema = Json.pretty(p);
        property.isReadOnly = p.getReadOnly();

        String type = getSwaggerType(p);
        if (p instanceof AbstractNumericProperty) {
            AbstractNumericProperty np = (AbstractNumericProperty) p;
            property.minimum = np.getMinimum();
            property.maximum = np.getMaximum();
            property.exclusiveMinimum = np.getExclusiveMinimum();
            property.exclusiveMaximum = np.getExclusiveMaximum();

            // legacy support
            Map<String, Object> allowableValues = new HashMap<String, Object>();
            if (np.getMinimum() != null) {
                allowableValues.put("min", np.getMinimum());
            }
            if (np.getMaximum() != null) {
                allowableValues.put("max", np.getMaximum());
            }
            if(allowableValues.size() > 0) {
              property.allowableValues = allowableValues;
            }
        }

        if (p instanceof StringProperty) {
            StringProperty sp = (StringProperty) p;
            property.maxLength = sp.getMaxLength();
            property.minLength = sp.getMinLength();
            property.pattern = sp.getPattern();
            if (sp.getEnum() != null) {
                List<String> _enum = sp.getEnum();
                property._enum = _enum;
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if(p instanceof IntegerProperty) {
            IntegerProperty sp = (IntegerProperty) p;
            if(sp.getEnum() != null) {
                List<Integer> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Integer i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if(p instanceof LongProperty) {
            LongProperty sp = (LongProperty) p;
            if(sp.getEnum() != null) {
                List<Long> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Long i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if(p instanceof DoubleProperty) {
            DoubleProperty sp = (DoubleProperty) p;
            if(sp.getEnum() != null) {
                List<Double> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Double i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if(p instanceof FloatProperty) {
            FloatProperty sp = (FloatProperty) p;
            if(sp.getEnum() != null) {
                List<Float> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Float i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if(p instanceof DateProperty) {
            DateProperty sp = (DateProperty) p;
            if(sp.getEnum() != null) {
                List<String> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(String i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if(p instanceof DateTimeProperty) {
            DateTimeProperty sp = (DateTimeProperty) p;
            if(sp.getEnum() != null) {
                List<String> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(String i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        property.datatype = getTypeDeclaration(p);

        // this can cause issues for clients which don't support enums
        if (property.isEnum) {
            property.datatypeWithEnum = toEnumName(property);
        } else {
            property.datatypeWithEnum = property.datatype;
        }

        property.baseType = getSwaggerType(p);

      	if (p instanceof ArrayProperty) {
        		property.isContainer = true;
        		property.containerType = "array";
        		ArrayProperty ap = (ArrayProperty) p;
        		CodegenProperty cp = fromProperty(property.name, ap.getItems());
        		if (cp == null) {
        			 LOGGER.warn("skipping invalid property " + Json.pretty(p));
        		} else {
          			property.baseType = getSwaggerType(p);
          			if (!languageSpecificPrimitives.contains(cp.baseType)) {
          				  property.complexType = cp.baseType;
          			} else {
          				  property.isPrimitiveType = true;
          			}
          			property.items = cp;
          			if (property.items.isEnum) {
          				  property.datatypeWithEnum = property.datatypeWithEnum.replace(property.items.baseType,
          						property.items.datatypeWithEnum);
                            if(property.defaultValue != null)
          				        property.defaultValue = property.defaultValue.replace(property.items.baseType, property.items.datatypeWithEnum);
          			}
        		}
      	} else if (p instanceof MapProperty) {
            property.isContainer = true;
            property.containerType = "map";
            MapProperty ap = (MapProperty) p;
            CodegenProperty cp = fromProperty("inner", ap.getAdditionalProperties());

            property.baseType = getSwaggerType(p);
            if (!languageSpecificPrimitives.contains(cp.baseType)) {
                property.complexType = cp.baseType;
            } else {
                property.isPrimitiveType = true;
            }
        } else {
            setNonArrayMapProperty(property, type);
        }
        return property;
    }

    protected void setNonArrayMapProperty(CodegenProperty property, String type) {
        property.isNotContainer = true;
        if (languageSpecificPrimitives().contains(type)) {
            property.isPrimitiveType = true;
        } else {
            property.complexType = property.baseType;
        }
    }

    private Response findMethodResponse(Map<String, Response> responses) {

        String code = null;
        for (String responseCode : responses.keySet()) {
            if (responseCode.startsWith("2") || responseCode.equals("default")) {
                if (code == null || code.compareTo(responseCode) > 0) {
                    code = responseCode;
                }
            }
        }
        if (code == null) {
            return null;
        }
        return responses.get(code);
    }
    
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Model> definitions) {
    	return fromOperation(path, httpMethod, operation, definitions, null);
    }
    
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger) {
        CodegenOperation op = CodegenModelFactory.newInstance(CodegenModelType.OPERATION);
        Set<String> imports = new HashSet<String>();
        op.vendorExtensions = operation.getVendorExtensions();

        String operationId = operation.getOperationId();
        if (operationId == null) {
            String tmpPath = path;
            tmpPath = tmpPath.replaceAll("\\{", "");
            tmpPath = tmpPath.replaceAll("\\}", "");
            String[] parts = (tmpPath + "/" + httpMethod).split("/");
            StringBuilder builder = new StringBuilder();
            if ("/".equals(tmpPath)) {
                // must be root tmpPath
                builder.append("root");
            }
            for (int i = 0; i < parts.length; i++) {
                String part = parts[i];
                if (part.length() > 0) {
                    if (builder.toString().length() == 0) {
                        part = Character.toLowerCase(part.charAt(0)) + part.substring(1);
                    } else {
                        part = initialCaps(part);
                    }
                    builder.append(part);
                }
            }
            operationId = builder.toString();
            LOGGER.info("generated operationId " + operationId + "\tfor Path: " + httpMethod + " " + path);
        }
        operationId = removeNonNameElementToCamelCase(operationId);
        op.path = path;
        op.operationId = toOperationId(operationId);
        op.summary = escapeText(operation.getSummary());
        op.notes = escapeText(operation.getDescription());
        op.tags = operation.getTags();
        op.hasConsumes = false;
        op.hasProduces = false;

        List<String> consumes = new ArrayList<String>();
        if (operation.getConsumes() != null) {
            if (operation.getConsumes().size() > 0) {
                // use consumes defined in the operation
                consumes = operation.getConsumes();
            } else {
                // empty list, do nothing to override global setting
            }
        } else if (swagger != null && swagger.getConsumes() != null && swagger.getConsumes().size() > 0) {
            // use consumes defined globally 
            consumes = swagger.getConsumes();
            LOGGER.debug("No consumes defined in operation. Using global consumes (" + swagger.getConsumes() + ") for " + op.operationId);
        }
        
        // if "consumes" is defined (per operation or using global definition)
        if (consumes != null && consumes.size() > 0) {
            List<Map<String, String>> c = new ArrayList<Map<String, String>>();
            int count = 0;
            for (String key : consumes) {
                Map<String, String> mediaType = new HashMap<String, String>();
                mediaType.put("mediaType", key);
                count += 1;
                if (count < consumes.size()) {
                    mediaType.put("hasMore", "true");
                } else {
                    mediaType.put("hasMore", null);
                }
                c.add(mediaType);
            }
            op.consumes = c;
            op.hasConsumes = true;
        }

        List<String> produces = new ArrayList<String>();
        if (operation.getProduces() != null) {
            if (operation.getProduces().size() > 0) {
                // use produces defined in the operation
                produces = operation.getProduces();
            } else {
                // empty list, do nothing to override global setting
            }
        } else if (swagger != null && swagger.getProduces() != null && swagger.getProduces().size() > 0) {
            // use produces defined globally 
            produces = swagger.getProduces();
            LOGGER.debug("No produces defined in operation. Using global produces (" + swagger.getProduces() + ") for " + op.operationId);
        }
        
        // if "produces" is defined (per operation or using global definition)
        if (produces != null && produces.size() > 0) {
            List<Map<String, String>> c = new ArrayList<Map<String, String>>();
            int count = 0;
            for (String key : produces) {
                Map<String, String> mediaType = new HashMap<String, String>();
                mediaType.put("mediaType", key);
                count += 1;
                if (count < produces.size()) {
                    mediaType.put("hasMore", "true");
                } else {
                    mediaType.put("hasMore", null);
                }
                c.add(mediaType);
            }
            op.produces = c;
            op.hasProduces = true;
        }

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            Response methodResponse = findMethodResponse(operation.getResponses());

            for (Map.Entry<String, Response> entry : operation.getResponses().entrySet()) {
                Response response = entry.getValue();
                CodegenResponse r = fromResponse(entry.getKey(), response);
                r.hasMore = true;
                if (r.baseType != null &&
                        !defaultIncludes.contains(r.baseType) &&
                        !languageSpecificPrimitives.contains(r.baseType)) {
                    imports.add(r.baseType);
                }
                r.isDefault = response == methodResponse;
                op.responses.add(r);
                if (r.isBinary && r.isDefault){
                    op.isResponseBinary = Boolean.TRUE;
                }
            }
            op.responses.get(op.responses.size() - 1).hasMore = false;

            if (methodResponse != null) {
                if (methodResponse.getSchema() != null) {
                    CodegenProperty cm = fromProperty("response", methodResponse.getSchema());

                    Property responseProperty = methodResponse.getSchema();

                    if (responseProperty instanceof ArrayProperty) {
                        ArrayProperty ap = (ArrayProperty) responseProperty;
                        CodegenProperty innerProperty = fromProperty("response", ap.getItems());
                        op.returnBaseType = innerProperty.baseType;
                    } else {
                        if (cm.complexType != null) {
                            op.returnBaseType = cm.complexType;
                        } else {
                            op.returnBaseType = cm.baseType;
                        }
                    }
                    op.examples = new ExampleGenerator(definitions).generate(methodResponse.getExamples(), operation.getProduces(), responseProperty);
                    op.defaultResponse = toDefaultValue(responseProperty);
                    op.returnType = cm.datatype;
                    if (cm.isContainer != null) {
                        op.returnContainer = cm.containerType;
                        if ("map".equals(cm.containerType)) {
                            op.isMapContainer = Boolean.TRUE;
                        } else if ("list".equalsIgnoreCase(cm.containerType)) {
                            op.isListContainer = Boolean.TRUE;
                        } else if ("array".equalsIgnoreCase(cm.containerType)) {
                            op.isListContainer = Boolean.TRUE;
                        }
                    } else {
                        op.returnSimpleType = true;
                    }
                    if (languageSpecificPrimitives().contains(op.returnBaseType) || op.returnBaseType == null) {
                        op.returnTypeIsPrimitive = true;
                    }
                }
                addHeaders(methodResponse, op.responseHeaders);
            }
        }

        List<Parameter> parameters = operation.getParameters();
        CodegenParameter bodyParam = null;
        List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> cookieParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();

        if (parameters != null) {
            for (Parameter param : parameters) {
                CodegenParameter p = fromParameter(param, imports);
                allParams.add(p);
                if (param instanceof QueryParameter) {
                    p.isQueryParam = new Boolean(true);
                    queryParams.add(p.copy());
                } else if (param instanceof PathParameter) {
                    p.required = true;
                    p.isPathParam = new Boolean(true);
                    pathParams.add(p.copy());
                } else if (param instanceof HeaderParameter) {
                    p.isHeaderParam = new Boolean(true);
                    headerParams.add(p.copy());
                } else if (param instanceof CookieParameter) {
                    p.isCookieParam = new Boolean(true);
                    cookieParams.add(p.copy());
                } else if (param instanceof BodyParameter) {
                    p.isBodyParam = new Boolean(true);
                    bodyParam = p;
                    bodyParams.add(p.copy());
                } else if (param instanceof FormParameter) {
                    if ("file".equalsIgnoreCase(((FormParameter) param).getType())) {
                        p.isFile = true;
                    } else {
                        p.notFile = true;
                    }
                    p.isFormParam = new Boolean(true);
                    formParams.add(p.copy());
                }
            }
        }
        for (String i : imports) {
            if (needToImport(i)) {
                op.imports.add(i);
            }
        }
        op.bodyParam = bodyParam;
        op.httpMethod = httpMethod.toUpperCase();

        // move "required" parameters in front of "optional" parameters
        if(sortParamsByRequiredFlag) {
          Collections.sort(allParams, new Comparator<CodegenParameter>() {
              @Override
              public int compare(CodegenParameter one, CodegenParameter another) {
                  boolean oneRequired = one.required == null ? false : one.required;
                  boolean anotherRequired = another.required == null ? false : another.required;
                  if (oneRequired == anotherRequired) return 0;
                  else if (oneRequired) return -1;
                  else return 1;
              }
          });
        }
        op.allParams = addHasMore(allParams);
        op.bodyParams = addHasMore(bodyParams);
        op.pathParams = addHasMore(pathParams);
        op.queryParams = addHasMore(queryParams);
        op.headerParams = addHasMore(headerParams);
        // op.cookieParams = cookieParams;
        op.formParams = addHasMore(formParams);
        // legacy support
        op.nickname = op.operationId;


        if (op.allParams.size() > 0) {
            op.hasParams = true;
        }
        op.externalDocs = operation.getExternalDocs();

        return op;
    }

    public CodegenResponse fromResponse(String responseCode, Response response) {
        CodegenResponse r = CodegenModelFactory.newInstance(CodegenModelType.RESPONSE);
        if ("default".equals(responseCode)) {
            r.code = "0";
        } else {
            r.code = responseCode;
        }
        r.message = escapeText(response.getDescription());
        r.schema = response.getSchema();
        r.examples = toExamples(response.getExamples());
        r.jsonSchema = Json.pretty(response);
        addHeaders(response, r.headers);

        if (r.schema != null) {
            Property responseProperty = response.getSchema();
            responseProperty.setRequired(true);
            CodegenProperty cm = fromProperty("response", responseProperty);

            if (responseProperty instanceof ArrayProperty) {
                ArrayProperty ap = (ArrayProperty) responseProperty;
                CodegenProperty innerProperty = fromProperty("response", ap.getItems());
                r.baseType = innerProperty.baseType;
            } else {
                if (cm.complexType != null) {
                    r.baseType = cm.complexType;
                } else {
                    r.baseType = cm.baseType;
                }
            }
            r.dataType = cm.datatype;
            r.isBinary = cm.datatype.equals("byte[]");
            if (cm.isContainer != null) {
                r.simpleType = false;
                r.containerType = cm.containerType;
                r.isMapContainer = "map".equals(cm.containerType);
                r.isListContainer = "list".equals(cm.containerType);
            } else {
                r.simpleType = true;
            }
            r.primitiveType = (r.baseType == null || languageSpecificPrimitives().contains(r.baseType));
        }
        if (r.baseType == null) {
            r.isMapContainer = false;
            r.isListContainer = false;
            r.primitiveType = true;
            r.simpleType = true;
        }
        return r;
    }

    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
        CodegenParameter p = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        p.baseName = param.getName();
        p.description = escapeText(param.getDescription());
        if (param.getRequired()) {
            p.required = param.getRequired();
        }
        p.jsonSchema = Json.pretty(param);

        if (System.getProperty("debugParser") != null) {
            LOGGER.info("working on Parameter " + param);
        }

        // move the defaultValue for headers, forms and params
        if (param instanceof QueryParameter) {
            p.defaultValue = ((QueryParameter) param).getDefaultValue();
        } else if (param instanceof HeaderParameter) {
            p.defaultValue = ((HeaderParameter) param).getDefaultValue();
        } else if (param instanceof FormParameter) {
            p.defaultValue = ((FormParameter) param).getDefaultValue();
        }

        p.vendorExtensions = param.getVendorExtensions();

        if (param instanceof SerializableParameter) {
            SerializableParameter qp = (SerializableParameter) param;
            Property property = null;
            String collectionFormat = null;
            String type = qp.getType();
            if (null == type) {
                LOGGER.warn("Type is NULL for Serializable Parameter: " + param);
            }
            if ("array".equals(type)) {
                Property inner = qp.getItems();
                if (inner == null) {
                    LOGGER.warn("warning!  No inner type supplied for array parameter \"" + qp.getName() + "\", using String");
                    inner = new StringProperty().description("//TODO automatically added by swagger-codegen");
                }
                property = new ArrayProperty(inner);
                collectionFormat = qp.getCollectionFormat();
                CodegenProperty pr = fromProperty("inner", inner);
                p.baseType = pr.datatype;
                p.isContainer = true;
                imports.add(pr.baseType);
            } else if ("object".equals(type)) {
                Property inner = qp.getItems();
                if (inner == null) {
                    LOGGER.warn("warning!  No inner type supplied for map parameter \"" + qp.getName() + "\", using String");
                    inner = new StringProperty().description("//TODO automatically added by swagger-codegen");
                }
                property = new MapProperty(inner);
                collectionFormat = qp.getCollectionFormat();
                CodegenProperty pr = fromProperty("inner", inner);
                p.baseType = pr.datatype;
                imports.add(pr.baseType);
            } else {
                Map<PropertyId, Object> args = new HashMap<PropertyId, Object>();
                String format = qp.getFormat();
                args.put(PropertyId.ENUM, qp.getEnum());
                property = PropertyBuilder.build(type, format, args);
            }
            if (property == null) {
                LOGGER.warn("warning!  Property type \"" + type + "\" not found for parameter \"" + param.getName() + "\", using String");
                property = new StringProperty().description("//TODO automatically added by swagger-codegen.  Type was " + type + " but not supported");
            }
            property.setRequired(param.getRequired());
            CodegenProperty model = fromProperty(qp.getName(), property);
            p.dataType = model.datatype;
            p.isEnum = model.isEnum;
            p._enum = model._enum;
            p.allowableValues = model.allowableValues;
            p.collectionFormat = collectionFormat;
            if(collectionFormat != null && collectionFormat.equals("multi")) {
                p.isCollectionFormatMulti = true;
            }
            p.paramName = toParamName(qp.getName());

            if (model.complexType != null) {
                imports.add(model.complexType);
            }
        } else {
            if (!(param instanceof BodyParameter)) {
                LOGGER.error("Cannot use Parameter " + param + " as Body Parameter");
            }

            BodyParameter bp = (BodyParameter) param;
            Model model = bp.getSchema();

            if (model instanceof ModelImpl) {
                ModelImpl impl = (ModelImpl) model;
                CodegenModel cm = fromModel(bp.getName(), impl);
                if (cm.emptyVars != null && cm.emptyVars == false) {
                    p.dataType = getTypeDeclaration(cm.classname);
                    imports.add(p.dataType);
                } else {
                    Property prop = PropertyBuilder.build(impl.getType(), impl.getFormat(), null);
                    prop.setRequired(bp.getRequired());
                    CodegenProperty cp = fromProperty("property", prop);
                    if (cp != null) {
                        p.dataType = cp.datatype;
                        if (p.dataType.equals("byte[]")) {
                            p.isBinary = true;
                        }
                        else {
                            p.isBinary = false;
                        }
                    }
                }
            } else if (model instanceof ArrayModel) {
                // to use the built-in model parsing, we unwrap the ArrayModel
                // and get a single property from it
                ArrayModel impl = (ArrayModel) model;
                CodegenModel cm = fromModel(bp.getName(), impl);
                // get the single property
                ArrayProperty ap = new ArrayProperty().items(impl.getItems());
                ap.setRequired(param.getRequired());
                CodegenProperty cp = fromProperty("inner", ap);
                if (cp.complexType != null) {
                    imports.add(cp.complexType);
                }
                imports.add(cp.baseType);
                p.dataType = cp.datatype;
                p.isContainer = true;
            } else {
                Model sub = bp.getSchema();
                if (sub instanceof RefModel) {
                    String name = ((RefModel) sub).getSimpleRef();
                    if (typeMapping.containsKey(name)) {
                        name = typeMapping.get(name);
                    } else {
                        name = toModelName(name);
                        if (defaultIncludes.contains(name)) {
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

    public List<CodegenSecurity> fromSecurity(Map<String, SecuritySchemeDefinition> schemes) {
        if (schemes == null) {
            return null;
        }

        List<CodegenSecurity> secs = new ArrayList<CodegenSecurity>(schemes.size());
        for (Iterator<Map.Entry<String, SecuritySchemeDefinition>> it = schemes.entrySet().iterator(); it.hasNext(); ) {
            final Map.Entry<String, SecuritySchemeDefinition> entry = it.next();
            final SecuritySchemeDefinition schemeDefinition = entry.getValue();

            CodegenSecurity sec = CodegenModelFactory.newInstance(CodegenModelType.SECURITY);
            sec.name = entry.getKey();
            sec.type = schemeDefinition.getType();

            if (schemeDefinition instanceof ApiKeyAuthDefinition) {
                final ApiKeyAuthDefinition apiKeyDefinition = (ApiKeyAuthDefinition) schemeDefinition;
                sec.isBasic = sec.isOAuth = false;
                sec.isApiKey = true;
                sec.keyParamName = apiKeyDefinition.getName();
                sec.isKeyInHeader = apiKeyDefinition.getIn() == In.HEADER;
                sec.isKeyInQuery = !sec.isKeyInHeader;
            } else if(schemeDefinition instanceof BasicAuthDefinition) {
                sec.isKeyInHeader = sec.isKeyInQuery = sec.isApiKey = sec.isOAuth = false;
                sec.isBasic = true;
            } else {
            	final OAuth2Definition oauth2Definition = (OAuth2Definition) schemeDefinition;
            	sec.isKeyInHeader = sec.isKeyInQuery = sec.isApiKey = sec.isBasic = false;
                sec.isOAuth = true;
                sec.flow = oauth2Definition.getFlow();
                sec.authorizationUrl = oauth2Definition.getAuthorizationUrl();
                sec.tokenUrl = oauth2Definition.getTokenUrl();
                if (oauth2Definition.getScopes() != null) {
                    List<Map<String, Object>> scopes = new ArrayList<Map<String, Object>>();
                    int count = 0, numScopes = oauth2Definition.getScopes().size();
                    for(Map.Entry<String, String> scopeEntry : oauth2Definition.getScopes().entrySet()) {
                        Map<String, Object> scope = new HashMap<String, Object>();
                        scope.put("scope", scopeEntry.getKey());
                        scope.put("description", scopeEntry.getValue());
                        
                        count += 1;
                        if (count < numScopes) {
                            scope.put("hasMore", "true");
                        } else {
                            scope.put("hasMore", null);
                        }
                        
                        scopes.add(scope);
                    }
                    sec.scopes = scopes;
                }
            }

            sec.hasMore = it.hasNext();
            secs.add(sec);
        }
        return secs;
    }

    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type)
            && !languageSpecificPrimitives.contains(type)
            && type.indexOf(".") < 0;
    }

    protected List<Map<String, Object>> toExamples(Map<String, Object> examples) {
        if (examples == null) {
            return null;
        }

        final List<Map<String, Object>> output = new ArrayList<Map<String, Object>>(examples.size());
        for (Map.Entry<String, Object> entry : examples.entrySet()) {
            final Map<String, Object> kv = new HashMap<String, Object>();
            kv.put("contentType", entry.getKey());
            kv.put("example", entry.getValue());
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
        if (objs != null) {
            for (int i = 0; i < objs.size(); i++) {
                if (i > 0) {
                    objs.get(i).secondaryParam = new Boolean(true);
                }
                if (i < objs.size() - 1) {
                    objs.get(i).hasMore = new Boolean(true);
                }
            }
        }
        return objs;
    }

    private Map<String, Object> addHasMore(Map<String, Object> objs) {
        if (objs != null) {
            for (int i = 0; i < objs.size() - 1; i++) {
                if (i > 0) {
                    objs.put("secondaryParam", new Boolean(true));
                }
                if (i < objs.size() - 1) {
                    objs.put("hasMore", true);
                }
            }
        }
        return objs;
    }

    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(tag);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(tag, opList);
        }
        opList.add(co);
        co.baseName = tag;
    }

  /* underscore and camelize are copied from Twitter elephant bird
   * https://github.com/twitter/elephant-bird/blob/master/core/src/main/java/com/twitter/elephantbird/util/Strings.java
   */

    private void addParentContainer(CodegenModel m, String name, Property property) {
        final CodegenProperty tmp = fromProperty(name, property);
        addImport(m, tmp.complexType);
        m.parent = toInstantiationType(property);
        final String containerType = tmp.containerType;
        final String instantiationType = instantiationTypes.get(containerType);
        if (instantiationType != null) {
            addImport(m, instantiationType);
        }
        final String mappedType = typeMapping.get(containerType);
        if (mappedType != null) {
            addImport(m, mappedType);
        }
    }

    /**
     * Underscore the given word.
     *
     * @param word The word
     * @return The underscored version of the word
     */
    public static String underscore(String word) {
        String firstPattern = "([A-Z]+)([A-Z][a-z])";
        String secondPattern = "([a-z\\d])([A-Z])";
        String replacementPattern = "$1_$2";
        // Replace package separator with slash.
        word = word.replaceAll("\\.", "/");
        // Replace $ with two underscores for inner classes.
        word = word.replaceAll("\\$", "__");
        // Replace capital letter with _ plus lowercase letter.
        word = word.replaceAll(firstPattern, replacementPattern);
        word = word.replaceAll(secondPattern, replacementPattern);
        word = word.replace('-', '_');
        word = word.toLowerCase();
        return word;
    }

    private void addImport(CodegenModel m, String type) {
        if (type != null && needToImport(type)) {
            m.imports.add(type);
        }
    }

    private void addVars(CodegenModel m, Map<String, Property> properties, Collection<String> required) {
        if (properties != null && properties.size() > 0) {
            m.hasVars = true;
            m.hasEnums = false;
            final int totalCount = properties.size();
            final Set<String> mandatory = required == null ? Collections.<String>emptySet() : new HashSet<String>(required);
            int count = 0;
            for (Map.Entry<String, Property> entry : properties.entrySet()) {
                final String key = entry.getKey();
                final Property prop = entry.getValue();

                if (prop == null) {
                    LOGGER.warn("null property for " + key);
                } else {
                    final CodegenProperty cp = fromProperty(key, prop);
                    cp.required = mandatory.contains(key) ? true : null;
                    if (cp.isEnum) {
                        m.hasEnums = true;
                    }
                    count += 1;
                    if (count != totalCount) {
                        cp.hasMore = true;
                    }
                    if (cp.isContainer != null) {
                        addImport(m, typeMapping.get("array"));
                    }
                    addImport(m, cp.baseType);
                    addImport(m, cp.complexType);
                    m.vars.add(cp);
                }
            }
        } else {
            m.emptyVars = true;
            m.hasVars = false;
            m.hasEnums = false;
        }
    }


    /**
     * Remove characters not suitable for variable or method name from the input and camelize it
     *
     * @param name
     * @return
     */
    public String removeNonNameElementToCamelCase(String name) {
        String nonNameElementPattern = "[-_:;#]";
        name = StringUtils.join(Lists.transform(Lists.newArrayList(name.split(nonNameElementPattern)), new Function<String, String>() {
            @Nullable
            @Override
            public String apply(String input) {
                return StringUtils.capitalize(input);
            }
        }), "");
        if (name.length() > 0) {
            name = name.substring(0, 1).toLowerCase() + name.substring(1);
        }
        return name;
    }

    public static String camelize(String word) {
        return camelize(word, false);
    }

    public static String camelize(String word, boolean lowercaseFirstLetter) {
        // Replace all slashes with dots (package separator)
        Pattern p = Pattern.compile("\\/(.?)");
        Matcher m = p.matcher(word);
        while (m.find()) {
            word = m.replaceFirst("." + m.group(1)/*.toUpperCase()*/);
            m = p.matcher(word);
        }

        // case out dots
        String[] parts = word.split("\\.");
        StringBuilder f = new StringBuilder();
        for (String z : parts) {
            if (z.length() > 0) {
                f.append(Character.toUpperCase(z.charAt(0))).append(z.substring(1));
            }
        }
        word = f.toString();

        m = p.matcher(word);
        while (m.find()) {
            word = m.replaceFirst("" + Character.toUpperCase(m.group(1).charAt(0)) + m.group(1).substring(1)/*.toUpperCase()*/);
            m = p.matcher(word);
        }

        // Uppercase the class name.
        p = Pattern.compile("(\\.?)(\\w)([^\\.]*)$");
        m = p.matcher(word);
        if (m.find()) {
            String rep = m.group(1) + m.group(2).toUpperCase() + m.group(3);
            rep = rep.replaceAll("\\$", "\\\\\\$");
            word = m.replaceAll(rep);
        }

        // Replace two underscores with $ to support inner classes.
        p = Pattern.compile("(__)(.)");
        m = p.matcher(word);
        while (m.find()) {
            word = m.replaceFirst("\\$" + m.group(2).toUpperCase());
            m = p.matcher(word);
        }

        // Remove all underscores
        p = Pattern.compile("(_)(.)");
        m = p.matcher(word);
        while (m.find()) {
            word = m.replaceFirst(m.group(2).toUpperCase());
            m = p.matcher(word);
        }

        if (lowercaseFirstLetter) {
            word = word.substring(0, 1).toLowerCase() + word.substring(1);
        }

        return word;
    }

    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        return apiFileFolder() + File.separator + toApiFilename(tag) + suffix;
    }

    public boolean shouldOverwrite(String filename) {
        return !(skipOverwrite && new File(filename).exists());
    }

    public boolean isSkipOverwrite() {
        return skipOverwrite;
    }

    public void setSkipOverwrite(boolean skipOverwrite) {
        this.skipOverwrite = skipOverwrite;
    }

    /**
     * All library templates supported.
     * (key: library name, value: library description)
     */
    public Map<String, String> supportedLibraries() {
        return supportedLibraries;
    }

    public void setLibrary(String library) {
        if (library != null && !supportedLibraries.containsKey(library))
            throw new RuntimeException("unknown library: " + library);
        this.library = library;
    }

    /**
     * Library template (sub-template).
     */
    public String getLibrary() {
        return library;
    }

    protected CliOption buildLibraryCliOption(Map<String, String> supportedLibraries) {
        StringBuilder sb = new StringBuilder("library template (sub-template) to use:");
        for (String lib : supportedLibraries.keySet()) {
            sb.append("\n").append(lib).append(" - ").append(supportedLibraries.get(lib));
        }
        return new CliOption("library", sb.toString());
    }

    /**
     * sanitize name (parameter, property, method, etc)
     *
     * @param name string to be sanitize
     * @return sanitized string
     */
    public String sanitizeName(String name) {
        // NOTE: performance wise, we should have written with 2 replaceAll to replace desired
        // character with _ or empty character. Below aims to spell out different cases we've
        // encountered so far and hopefully make it easier for others to add more special
        // cases in the future.

        // input[] => input
        name = name.replaceAll("\\[\\]", "");

        // input[a][b] => input_a_b
        name = name.replaceAll("\\[", "_");
        name = name.replaceAll("\\]", "");

        // input(a)(b) => input_a_b
        name = name.replaceAll("\\(", "_");
        name = name.replaceAll("\\)", "");

        // input.name => input_name
        name = name.replaceAll("\\.", "_");

        // input-name => input_name
        name = name.replaceAll("-", "_");

        // input name and age => input_name_and_age
        name = name.replaceAll(" ", "_");

        // remove everything else other than word, number and _
        // $php_variable => php_variable
        return name.replaceAll("[^a-zA-Z0-9_]", "");

    }
}
