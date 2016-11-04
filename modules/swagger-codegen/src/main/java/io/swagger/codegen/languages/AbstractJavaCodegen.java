package io.swagger.codegen.languages;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;

import com.google.common.base.Strings;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;
import io.swagger.models.parameters.FormParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;


public abstract class AbstractJavaCodegen extends DefaultCodegen implements CodegenConfig {

    public static final String FULL_JAVA_UTIL = "fullJavaUtil";
    public static final String DEFAULT_LIBRARY = "<default>";
    public static final String DATE_LIBRARY = "dateLibrary";
    public static final String SUPPORT_JAVA6 = "supportJava6";

    protected String dateLibrary = "joda";
    protected String invokerPackage = "io.swagger";
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-java";
    protected String artifactVersion = "1.0.0";
    protected String projectFolder = "src" + File.separator + "main";
    protected String projectTestFolder = "src" + File.separator + "test";
    protected String sourceFolder = projectFolder + File.separator + "java";
    protected String testFolder = projectTestFolder + File.separator + "java";
    protected String localVariablePrefix = "";
    protected boolean fullJavaUtil;
    protected String javaUtilPrefix = "";
    protected Boolean serializableModel = false;
    protected boolean serializeBigDecimalAsString = false;
    protected boolean hideGenerationTimestamp = false;
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected boolean supportJava6= false;

    public AbstractJavaCodegen() {
        super();
        supportsInheritance = true;
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        apiTestTemplateFiles.put("api_test.mustache", ".java");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        setReservedWordsLowerCase(
            Arrays.asList(
                // used as internal variables, can collide with parameter names
                "localVarPath", "localVarQueryParams", "localVarHeaderParams", "localVarFormParams",
                "localVarPostBody", "localVarAccepts", "localVarAccept", "localVarContentTypes",
                "localVarContentType", "localVarAuthNames", "localReturnType",
                "ApiClient", "ApiException", "ApiResponse", "Configuration", "StringUtil",

                // language reserved words
                "abstract", "continue", "for", "new", "switch", "assert",
                "default", "if", "package", "synchronized", "boolean", "do", "goto", "private",
                "this", "break", "double", "implements", "protected", "throw", "byte", "else",
                "import", "public", "throws", "case", "enum", "instanceof", "return", "transient",
                "catch", "extends", "int", "short", "try", "char", "final", "interface", "static",
                "void", "class", "finally", "long", "strictfp", "volatile", "const", "float",
                "native", "super", "while")
        );

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float",
                        "Object",
                        "byte[]")
        );
        instantiationTypes.put("array", "ArrayList");
        instantiationTypes.put("map", "HashMap");
        typeMapping.put("date", "Date");
        typeMapping.put("file", "File");
        typeMapping.put("UUID", "String");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.GROUP_ID, CodegenConstants.GROUP_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_ID, CodegenConstants.ARTIFACT_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, CodegenConstants.ARTIFACT_VERSION_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC));
        cliOptions.add(new CliOption(CodegenConstants.LOCAL_VARIABLE_PREFIX, CodegenConstants.LOCAL_VARIABLE_PREFIX_DESC));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.SERIALIZABLE_MODEL, CodegenConstants.SERIALIZABLE_MODEL_DESC));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING, CodegenConstants
                .SERIALIZE_BIG_DECIMAL_AS_STRING_DESC));
        cliOptions.add(CliOption.newBoolean(FULL_JAVA_UTIL, "whether to use fully qualified name for classes under java.util. This option only works for Java API client"));
        cliOptions.add(new CliOption("hideGenerationTimestamp", "hides the timestamp when files were generated"));

        CliOption dateLibrary = new CliOption(DATE_LIBRARY, "Option. Date library to use");
        Map<String, String> dateOptions = new HashMap<String, String>();
        dateOptions.put("java8", "Java 8 native");
        dateOptions.put("java8-localdatetime", "Java 8 using LocalDateTime (for legacy app only)");
        dateOptions.put("joda", "Joda");
        dateOptions.put("legacy", "Legacy java.util.Date");
        dateLibrary.setEnum(dateOptions);

        cliOptions.add(dateLibrary);

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(SUPPORT_JAVA6)) {
            this.setSupportJava6(Boolean.valueOf(additionalProperties.get(SUPPORT_JAVA6).toString()));
        }
        additionalProperties.put(SUPPORT_JAVA6, supportJava6);


        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
        } else if (additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            // guess from api package
            String derviedInvokerPackage = deriveInvokerPackageName((String)additionalProperties.get(CodegenConstants.API_PACKAGE));
            this.additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, derviedInvokerPackage);
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
            LOGGER.info("Invoker Package Name, originally not set, is now dervied from api package name: " + derviedInvokerPackage);
        } else if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            // guess from model package
            String derviedInvokerPackage = deriveInvokerPackageName((String)additionalProperties.get(CodegenConstants.MODEL_PACKAGE));
            this.additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, derviedInvokerPackage);
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
            LOGGER.info("Invoker Package Name, originally not set, is now dervied from model package name: " + derviedInvokerPackage);
        } else {
            //not set, use default to be passed to template
            additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }

        if (additionalProperties.containsKey(CodegenConstants.GROUP_ID)) {
            this.setGroupId((String) additionalProperties.get(CodegenConstants.GROUP_ID));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_ID)) {
            this.setArtifactId((String) additionalProperties.get(CodegenConstants.ARTIFACT_ID));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_VERSION)) {
            this.setArtifactVersion((String) additionalProperties.get(CodegenConstants.ARTIFACT_VERSION));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }

        if (additionalProperties.containsKey(CodegenConstants.LOCAL_VARIABLE_PREFIX)) {
            this.setLocalVariablePrefix((String) additionalProperties.get(CodegenConstants.LOCAL_VARIABLE_PREFIX));
        }

        if (additionalProperties.containsKey(CodegenConstants.SERIALIZABLE_MODEL)) {
            this.setSerializableModel(Boolean.valueOf(additionalProperties.get(CodegenConstants.SERIALIZABLE_MODEL).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            this.setLibrary((String) additionalProperties.get(CodegenConstants.LIBRARY));
        }

        if(additionalProperties.containsKey(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING)) {
            this.setSerializeBigDecimalAsString(Boolean.valueOf(additionalProperties.get(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING).toString()));
        }

        // need to put back serializableModel (boolean) into additionalProperties as value in additionalProperties is string
        additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, serializableModel);

        if (additionalProperties.containsKey(FULL_JAVA_UTIL)) {
            this.setFullJavaUtil(Boolean.valueOf(additionalProperties.get(FULL_JAVA_UTIL).toString()));
        }

        if (fullJavaUtil) {
            javaUtilPrefix = "java.util.";
        }
        additionalProperties.put(FULL_JAVA_UTIL, fullJavaUtil);
        additionalProperties.put("javaUtilPrefix", javaUtilPrefix);

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        importMapping.put("List", "java.util.List");

        if (fullJavaUtil) {
            typeMapping.put("array", "java.util.List");
            typeMapping.put("map", "java.util.Map");
            typeMapping.put("DateTime", "java.util.Date");
            typeMapping.remove("List");
            importMapping.remove("Date");
            importMapping.remove("Map");
            importMapping.remove("HashMap");
            importMapping.remove("Array");
            importMapping.remove("ArrayList");
            importMapping.remove("List");
            importMapping.remove("Set");
            importMapping.remove("DateTime");
            instantiationTypes.put("array", "java.util.ArrayList");
            instantiationTypes.put("map", "java.util.HashMap");
        }

        this.sanitizeConfig();

        // optional jackson mappings for BigDecimal support
        importMapping.put("ToStringSerializer", "com.fasterxml.jackson.databind.ser.std.ToStringSerializer");
        importMapping.put("JsonSerialize", "com.fasterxml.jackson.databind.annotation.JsonSerialize");

        // imports for pojos
        importMapping.put("ApiModelProperty", "io.swagger.annotations.ApiModelProperty");
        importMapping.put("ApiModel", "io.swagger.annotations.ApiModel");
        importMapping.put("JsonProperty", "com.fasterxml.jackson.annotation.JsonProperty");
        importMapping.put("JsonCreator", "com.fasterxml.jackson.annotation.JsonCreator");
        importMapping.put("JsonValue", "com.fasterxml.jackson.annotation.JsonValue");
        importMapping.put("SerializedName", "com.google.gson.annotations.SerializedName");
        importMapping.put("Objects", "java.util.Objects");
        importMapping.put("StringUtil", invokerPackage + ".StringUtil");
        // import JsonCreator if JsonProperty is imported
        // used later in recursive import in postProcessingModels
        importMapping.put("com.fasterxml.jackson.annotation.JsonProperty", "com.fasterxml.jackson.annotation.JsonCreator");

        if(additionalProperties.containsKey(DATE_LIBRARY)) {
            setDateLibrary(additionalProperties.get("dateLibrary").toString());
            additionalProperties.put(dateLibrary, "true");
        }

        if("joda".equals(dateLibrary)) {
            typeMapping.put("date", "LocalDate");
            typeMapping.put("DateTime", "DateTime");

            importMapping.put("LocalDate", "org.joda.time.LocalDate");
            importMapping.put("DateTime", "org.joda.time.DateTime");
        } else if (dateLibrary.startsWith("java8")) {
            additionalProperties.put("java8", "true");
            typeMapping.put("date", "LocalDate");
            importMapping.put("LocalDate", "java.time.LocalDate");
            if ("java8-localdatetime".equals(dateLibrary)) {
                typeMapping.put("DateTime", "LocalDateTime");
                importMapping.put("LocalDateTime", "java.time.LocalDateTime");
            } else {
                typeMapping.put("DateTime", "OffsetDateTime");
                importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
            }
        } else if (dateLibrary.equals("legacy")) {
            additionalProperties.put("legacyDates", "true");
        }
    }

    private void sanitizeConfig() {
        // Sanitize any config options here. We also have to update the additionalProperties because
        // the whole additionalProperties object is injected into the main object passed to the mustache layer

        this.setApiPackage(sanitizePackageName(apiPackage));
        if (additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            this.additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        }

        this.setModelPackage(sanitizePackageName(modelPackage));
        if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            this.additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        }

        this.setInvokerPackage(sanitizePackageName(invokerPackage));
        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', '/');
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + "/" + testFolder + "/" + apiPackage().replace('.', '/');
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', '/');
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiName(name) + "Test";
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        if ("class".equals(name.toLowerCase())) {
            return "propertyClass";
        }

        if("_".equals(name)) {
          name = "_u";
        }

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        if(startsWithTwoUppercaseLetters(name)){
            name = name.substring(0, 2).toLowerCase() + name.substring(2);
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    private boolean startsWithTwoUppercaseLetters(String name) {
        boolean startsWithTwoUppercaseLetters = false;
        if(name.length() > 1) {
            startsWithTwoUppercaseLetters = name.substring(0, 2).equals(name.substring(0, 2).toUpperCase());
        }
        return startsWithTwoUppercaseLetters;
    }

    @Override
    public String toParamName(String name) {
        // to avoid conflicts with 'callback' parameter for async call
        if ("callback".equals(name)) {
            return "paramCallback";
        }

        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(final String name) {
        final String sanitizedName = sanitizeName(modelNamePrefix + name + modelNameSuffix);

        // camelize the model name
        // phone_number => PhoneNumber
        final String camelizedName = camelize(sanitizedName);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            final String modelName = "Model" + camelizedName;
            LOGGER.warn(camelizedName + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            final String modelName = "Model" + camelizedName; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        return camelizedName;
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            return getSwaggerType(p) + "<String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof ArrayProperty) {
            final ArrayProperty ap = (ArrayProperty) p;
            final String pattern;
            if (fullJavaUtil) {
                pattern = "new java.util.ArrayList<%s>()";
            } else {
                pattern = "new ArrayList<%s>()";
            }
            return String.format(pattern, getTypeDeclaration(ap.getItems()));
        } else if (p instanceof MapProperty) {
            final MapProperty ap = (MapProperty) p;
            final String pattern;
            if (fullJavaUtil) {
                pattern = "new java.util.HashMap<String, %s>()";
            } else {
                pattern = "new HashMap<String, %s>()";
            }
            return String.format(pattern, getTypeDeclaration(ap.getAdditionalProperties()));
        } else if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "null";
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString()+"l";
            }
           return "null";
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString() + "d";
            }
            return "null";
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString() + "f";
            }
            return "null";
        } else if (p instanceof BooleanProperty) {
            BooleanProperty bp = (BooleanProperty) p;
            if (bp.getDefault() != null) {
                return bp.getDefault().toString();
            }
            return "null";
        } else if (p instanceof StringProperty) {
            StringProperty sp = (StringProperty) p;
            if (sp.getDefault() != null) {
                String _default = sp.getDefault();
                if (sp.getEnum() == null) {
                    return "\"" + escapeText(_default) + "\"";
                } else {
                    // convert to enum var name later in postProcessModels
                    return _default;
                }
            }
            return "null";
        }
        return super.toDefaultValue(p);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            example = p.defaultValue;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("String".equals(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if ("Integer".equals(type) || "Short".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if ("Long".equals(type)) {
            if (example == null) {
                example = "56";
            }
            example = example + "L";
        } else if ("Float".equals(type)) {
            if (example == null) {
                example = "3.4";
            }
            example = example + "F";
        } else if ("Double".equals(type)) {
            example = "3.4";
            example = example + "D";
        } else if ("Boolean".equals(type)) {
            if (example == null) {
                example = "true";
            }
        } else if ("File".equals(type)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "new File(\"" + escapeText(example) + "\")";
        } else if ("Date".equals(type)) {
            example = "new Date()";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = "new " + type + "()";
        }

        if (example == null) {
            example = "null";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            example = "Arrays.asList(" + example + ")";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            example = "new HashMap()";
        }

        p.example = example;
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type) || type.indexOf(".") >= 0 ||
                type.equals("Map") || type.equals("List") ||
                type.equals("File") || type.equals("Date")) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        if (null == type) {
            LOGGER.error("No Type defined for Property " + p);
        }
        return toModelName(type);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");
        }

        operationId = camelize(sanitizeName(operationId), true);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize("call_" + operationId, true);
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        return operationId;
    }

    @Override
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);
        if(codegenModel.description != null) {
            codegenModel.imports.add("ApiModel");
        }
        if (allDefinitions != null && codegenModel.parentSchema != null && codegenModel.hasEnums) {
            final Model parentModel = allDefinitions.get(codegenModel.parentSchema);
            final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent, parentModel);
            codegenModel = AbstractJavaCodegen.reconcileInlineEnums(codegenModel, parentCodegenModel);
        }

        return codegenModel;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if(serializeBigDecimalAsString) {
            if (property.baseType.equals("BigDecimal")) {
                // we serialize BigDecimal as `string` to avoid precision loss
                property.vendorExtensions.put("extraAnnotation", "@JsonSerialize(using = ToStringSerializer.class)");

                // this requires some more imports to be added for this model...
                model.imports.add("ToStringSerializer");
                model.imports.add("JsonSerialize");
            }
        }

        if ("array".equals(property.containerType)) {
          model.imports.add("ArrayList");
        } else if ("map".equals(property.containerType)) {
          model.imports.add("HashMap");
        }

        if(!BooleanUtils.toBoolean(model.isEnum)) {
            // needed by all pojos, but not enums
            model.imports.add("ApiModelProperty");
            model.imports.add("ApiModel");
        }
        return;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) { }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // recursively add import for mapping one type to multiple imports
        List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
        if (recursiveImports == null)
            return objs;

        ListIterator<Map<String, String>> listIterator = recursiveImports.listIterator();
        while (listIterator.hasNext()) {
            String _import = listIterator.next().get("import");
            // if the import package happens to be found in the importMapping (key)
            // add the corresponding import package to the list
            if (importMapping.containsKey(_import)) {
                Map<String, String> newImportMap= new HashMap<String, String>();
                newImportMap.put("import", importMapping.get(_import));
                listIterator.add(newImportMap);
            }
        }

        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        // Remove imports of List, ArrayList, Map and HashMap as they are
        // imported in the template already.
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        Pattern pattern = Pattern.compile("java\\.util\\.(List|ArrayList|Map|HashMap)");
        for (Iterator<Map<String, String>> itr = imports.iterator(); itr.hasNext();) {
            String _import = itr.next().get("import");
            if (pattern.matcher(_import).matches()) {
              itr.remove();
            }
        }
        return objs;
    }

    @Override
    public void preprocessSwagger(Swagger swagger) {
        if (swagger != null && swagger.getPaths() != null) {
            for (String pathname : swagger.getPaths().keySet()) {
                Path path = swagger.getPath(pathname);
                if (path.getOperations() != null) {
                    for (Operation operation : path.getOperations()) {
                        boolean hasFormParameters = false;
                        for (Parameter parameter : operation.getParameters()) {
                            if (parameter instanceof FormParameter) {
                                hasFormParameters = true;
                            }
                        }

                        String defaultContentType = hasFormParameters ? "application/x-www-form-urlencoded" : "application/json";
                        String contentType = operation.getConsumes() == null || operation.getConsumes().isEmpty()
                                ? defaultContentType : operation.getConsumes().get(0);
                        String accepts = getAccept(operation);
                        operation.setVendorExtension("x-contentType", contentType);
                        operation.setVendorExtension("x-accepts", accepts);
                    }
                }
            }
        }
    }

    private static String getAccept(Operation operation) {
        String accepts = null;
        String defaultContentType = "application/json";
        if (operation.getProduces() != null && !operation.getProduces().isEmpty()) {
            StringBuilder sb = new StringBuilder();
            for (String produces : operation.getProduces()) {
                if (defaultContentType.equalsIgnoreCase(produces)) {
                    accepts = defaultContentType;
                    break;
                } else {
                    if (sb.length() > 0) {
                        sb.append(",");
                    }
                    sb.append(produces);
                }
            }
            if (accepts == null) {
                accepts = sb.toString();
            }
        } else {
            accepts = defaultContentType;
        }

        return accepts;
    }

    @Override
    protected boolean needToImport(String type) {
        return super.needToImport(type) && type.indexOf(".") < 0;
    }
/*
    @Override
    public String findCommonPrefixOfVars(List<String> vars) {
        String prefix = StringUtils.getCommonPrefix(vars.toArray(new String[vars.size()]));
        // exclude trailing characters that should be part of a valid variable
        // e.g. ["status-on", "status-off"] => "status-" (not "status-o")
        return prefix.replaceAll("[a-zA-Z0-9]+\\z", "");
    }
*/

    @Override
    public String toEnumName(CodegenProperty property) {
        return sanitizeName(camelize(property.name)) + "Enum";
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        // for symbol, e.g. $, #
        if (getSymbolName(value) != null) {
            return getSymbolName(value).toUpperCase();
        }

        // number
        if ("Integer".equals(datatype) || "Long".equals(datatype) ||
            "Float".equals(datatype) || "Double".equals(datatype)) {
            String varName = "NUMBER_" + value;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String var = value.replaceAll("\\W+", "_").toUpperCase();
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("Integer".equals(datatype) || "Long".equals(datatype) ||
            "Float".equals(datatype) || "Double".equals(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, definitions, swagger);

        op.path = sanitizePath(op.path);
        
        return op;
    }
    
    private static CodegenModel reconcileInlineEnums(CodegenModel codegenModel, CodegenModel parentCodegenModel) {
        // This generator uses inline classes to define enums, which breaks when
        // dealing with models that have subTypes. To clean this up, we will analyze
        // the parent and child models, look for enums that match, and remove
        // them from the child models and leave them in the parent.
        // Because the child models extend the parents, the enums will be available via the parent.

        // Only bother with reconciliation if the parent model has enums.
        if (parentCodegenModel.hasEnums) {

            // Get the properties for the parent and child models
            final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
            List<CodegenProperty> codegenProperties = codegenModel.vars;

            // Iterate over all of the parent model properties
            boolean removedChildEnum = false;
            for (CodegenProperty parentModelCodegenPropery : parentModelCodegenProperties) {
                // Look for enums
                if (parentModelCodegenPropery.isEnum) {
                    // Now that we have found an enum in the parent class,
                    // and search the child class for the same enum.
                    Iterator<CodegenProperty> iterator = codegenProperties.iterator();
                    while (iterator.hasNext()) {
                        CodegenProperty codegenProperty = iterator.next();
                        if (codegenProperty.isEnum && codegenProperty.equals(parentModelCodegenPropery)) {
                            // We found an enum in the child class that is
                            // a duplicate of the one in the parent, so remove it.
                            iterator.remove();
                            removedChildEnum = true;
                        }
                    }
                }
            }

            if(removedChildEnum) {
                // If we removed an entry from this model's vars, we need to ensure hasMore is updated
                int count = 0, numVars = codegenProperties.size();
                for(CodegenProperty codegenProperty : codegenProperties) {
                    count += 1;
                    codegenProperty.hasMore = (count < numVars) ? true : null;
                }
                codegenModel.vars = codegenProperties;
            }
        }

        return codegenModel;
    }

    private static String sanitizePackageName(String packageName) {
        packageName = packageName.trim(); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        packageName = packageName.replaceAll("[^a-zA-Z0-9_\\.]", "_");
        if(Strings.isNullOrEmpty(packageName)) {
            return "invalidPackageName";
        }
        return packageName;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setTestFolder(String testFolder) {
        this.testFolder = testFolder;
    }

    public void setLocalVariablePrefix(String localVariablePrefix) {
        this.localVariablePrefix = localVariablePrefix;
    }

    public void setSerializeBigDecimalAsString(boolean s) {
        this.serializeBigDecimalAsString = s;
    }

    public void setSerializableModel(Boolean serializableModel) {
        this.serializableModel = serializableModel;
    }

    private String sanitizePath(String p) {
        //prefer replace a ", instead of a fuLL URL encode for readability
        return p.replaceAll("\"", "%22");
    }

    public void setFullJavaUtil(boolean fullJavaUtil) {
        this.fullJavaUtil = fullJavaUtil;
    }

    public void setDateLibrary(String library) {
        this.dateLibrary = library;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    /*
     * Derive invoker package name based on the input
     * e.g. foo.bar.model => foo.bar
     *
     * @param input API package/model name
     * @return Derived invoker package name based on API package/model name
     */
    private String deriveInvokerPackageName(String input) {
        String[] parts = input.split(Pattern.quote(".")); // Split on period.

        StringBuilder sb = new StringBuilder();
        String delim = "";
        for (String p : Arrays.copyOf(parts, parts.length-1)) {
            sb.append(delim).append(p);
            delim = ".";
        }
        return sb.toString();
    }
    
    public void setSupportJava6(boolean value) {
        this.supportJava6 = value;
    }

    public String toRegularExpression(String pattern) {
        return escapeText(pattern);
    }

}
