package io.swagger.codegen.languages;

import com.google.common.base.Strings;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
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

public class JavaClientCodegen extends DefaultCodegen implements CodegenConfig {
    @SuppressWarnings("hiding")
    private static final Logger LOGGER = LoggerFactory.getLogger(JavaClientCodegen.class);
    public static final String FULL_JAVA_UTIL = "fullJavaUtil";
    public static final String DEFAULT_LIBRARY = "<default>";
    public static final String DATE_LIBRARY = "dateLibrary";
    public static final String USE_RX_JAVA = "useRxJava";

    public static final String RETROFIT_1 = "retrofit";
    public static final String RETROFIT_2 = "retrofit2";

    protected String dateLibrary = "default";
    protected String invokerPackage = "io.swagger.client";
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-java-client";
    protected String artifactVersion = "1.0.0";
    protected String projectFolder = "src" + File.separator + "main";
    protected String sourceFolder = projectFolder + File.separator + "java";
    protected String localVariablePrefix = "";
    protected boolean fullJavaUtil;
    protected String javaUtilPrefix = "";
    protected Boolean serializableModel = false;
    protected boolean serializeBigDecimalAsString = false;
    protected boolean useRxJava = false;

    public JavaClientCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "java";
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        embeddedTemplateDir = templateDir = "Java";
        apiPackage = "io.swagger.client.api";
        modelPackage = "io.swagger.client.model";

        reservedWords = new HashSet<String>(
                Arrays.asList(
                        // used as internal variables, can collide with parameter names
                        "path", "queryParams", "headerParams", "formParams", "postBody", "accepts", "accept", "contentTypes",
                        "contentType", "authNames",

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
        cliOptions.add(CliOption.newBoolean(FULL_JAVA_UTIL, "whether to use fully qualified name for classes under java.util"));
        cliOptions.add(CliOption.newBoolean(USE_RX_JAVA, "Whether to use the RxJava adapter with the retrofit2 library."));

        supportedLibraries.put(DEFAULT_LIBRARY, "HTTP client: Jersey client 1.18. JSON processing: Jackson 2.4.2");
        supportedLibraries.put("feign", "HTTP client: Netflix Feign 8.1.1");
        supportedLibraries.put("jersey2", "HTTP client: Jersey client 2.6");
        supportedLibraries.put("okhttp-gson", "HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1");
        supportedLibraries.put(RETROFIT_1, "HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1 (Retrofit 1.9.0)");
        supportedLibraries.put(RETROFIT_2, "HTTP client: OkHttp 2.5.0. JSON processing: Gson 2.4 (Retrofit 2.0.0-beta2). Enable the RxJava adapter using '-DuseRxJava=true'.");

        CliOption library = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        library.setDefault(DEFAULT_LIBRARY);
        library.setEnum(supportedLibraries);
        library.setDefault(DEFAULT_LIBRARY);
        cliOptions.add(library);

        CliOption dateLibrary = new CliOption(DATE_LIBRARY, "Option. Date library to use");
        Map<String, String> dateOptions = new HashMap<String, String>();
        dateOptions.put("java8", "Java 8 native");
        dateOptions.put("joda", "Joda");
        dateLibrary.setEnum(dateOptions);

        cliOptions.add(dateLibrary);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "java";
    }

    @Override
    public String getHelp() {
        return "Generates a Java client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
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

        if (additionalProperties.containsKey(USE_RX_JAVA)) {
            this.setUseRxJava(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA).toString()));
        }
        if (fullJavaUtil) {
            javaUtilPrefix = "java.util.";
        }
        additionalProperties.put(FULL_JAVA_UTIL, fullJavaUtil);
        additionalProperties.put("javaUtilPrefix", javaUtilPrefix);

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
        importMapping.put("JsonValue", "com.fasterxml.jackson.annotation.JsonValue");
        importMapping.put("Objects", "java.util.Objects");
        importMapping.put("StringUtil", invokerPackage + ".StringUtil");

        final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("gradle.properties.mustache", "", "gradle.properties"));
        supportingFiles.add(new SupportingFile("manifest.mustache", projectFolder, "AndroidManifest.xml"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache", invokerFolder, "ApiClient.java"));
        supportingFiles.add(new SupportingFile("StringUtil.mustache", invokerFolder, "StringUtil.java"));

        final String authFolder = (sourceFolder + '/' + invokerPackage + ".auth").replace(".", "/");
        if ("feign".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("FormAwareEncoder.mustache", invokerFolder, "FormAwareEncoder.java"));
        }
        supportingFiles.add(new SupportingFile("auth/HttpBasicAuth.mustache", authFolder, "HttpBasicAuth.java"));
        supportingFiles.add(new SupportingFile("auth/ApiKeyAuth.mustache", authFolder, "ApiKeyAuth.java"));
        supportingFiles.add(new SupportingFile("auth/OAuth.mustache", authFolder, "OAuth.java"));
        supportingFiles.add(new SupportingFile("auth/OAuthFlow.mustache", authFolder, "OAuthFlow.java"));

        if (!("feign".equals(getLibrary()) || usesAnyRetrofitLibrary())) {
            supportingFiles.add(new SupportingFile("apiException.mustache", invokerFolder, "ApiException.java"));
            supportingFiles.add(new SupportingFile("Configuration.mustache", invokerFolder, "Configuration.java"));
            supportingFiles.add(new SupportingFile("Pair.mustache", invokerFolder, "Pair.java"));
            supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));
        }

        // library-specific files
        if ("okhttp-gson".equals(getLibrary())) {
            // the "okhttp-gson" library template requires "ApiCallback.mustache" for async call
            supportingFiles.add(new SupportingFile("ApiCallback.mustache", invokerFolder, "ApiCallback.java"));
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("ProgressRequestBody.mustache", invokerFolder, "ProgressRequestBody.java"));
            supportingFiles.add(new SupportingFile("ProgressResponseBody.mustache", invokerFolder, "ProgressResponseBody.java"));
            // "build.sbt" is for development with SBT
            supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        } else if (usesAnyRetrofitLibrary()) {
            supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
            supportingFiles.add(new SupportingFile("CollectionFormats.mustache", invokerFolder, "CollectionFormats.java"));
        } else if("jersey2".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
        }

        if(additionalProperties.containsKey(DATE_LIBRARY)) {
            this.dateLibrary = additionalProperties.get(DATE_LIBRARY).toString();
        }

        if("joda".equals(dateLibrary)) {
            typeMapping.put("date", "LocalDate");
            typeMapping.put("DateTime", "DateTime");

            importMapping.put("LocalDate", "org.joda.time.LocalDate");
            importMapping.put("DateTime", "org.joda.time.DateTime");
        }
        else if ("java8".equals(dateLibrary)) {
            additionalProperties.put("java8", "true");
            additionalProperties.put("javaVersion", "1.8");
            typeMapping.put("date", "LocalDate");
            typeMapping.put("DateTime", "LocalDateTime");
            importMapping.put("LocalDate", "java.time.LocalDate");
            importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        }
    }

    private boolean usesAnyRetrofitLibrary() {
        return getLibrary() != null && getLibrary().contains(RETROFIT_1);
    }

    private boolean usesRetrofit2Library() {
        return getLibrary() != null && getLibrary().contains(RETROFIT_2);
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
    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', '/');
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        if("_".equals(name)) {
          name = "_u";
        }

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (reservedWords.contains(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
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
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type) || type.indexOf(".") >= 0) {
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

        // method name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return camelize(sanitizeName(operationId), true);
    }

    @Override
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);

        if (allDefinitions != null && codegenModel != null && codegenModel.parentSchema != null && codegenModel.hasEnums) {
            final Model parentModel = allDefinitions.get(codegenModel.parentSchema);
            final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent, parentModel);
            codegenModel = JavaClientCodegen.reconcileInlineEnums(codegenModel, parentCodegenModel);
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

        if(model.isEnum == null || model.isEnum) {
            // needed by all pojos, but not enums
            model.imports.add("ApiModelProperty");
            model.imports.add("ApiModel");
            // comment out below as it's in the model template 
            //model.imports.add("Objects");

            final String lib = getLibrary();
            if(StringUtils.isEmpty(lib) || "feign".equals(lib) || "jersey2".equals(lib)) {
                model.imports.add("JsonProperty");

                if(model.hasEnums != null || model.hasEnums == true) {
                  model.imports.add("JsonValue");
                }
            }
        }
        return;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        return;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            for (CodegenProperty var : cm.vars) {
                Map<String, Object> allowableValues = var.allowableValues;

                // handle ArrayProperty
                if (var.items != null) {
                    allowableValues = var.items.allowableValues;
                }

                if (allowableValues == null) {
                    continue;
                }
                List<String> values = (List<String>) allowableValues.get("values");
                if (values == null) {
                    continue;
                }

                // put "enumVars" map into `allowableValues", including `name` and `value`
                List<Map<String, String>> enumVars = new ArrayList<Map<String, String>>();
                String commonPrefix = findCommonPrefixOfVars(values);
                int truncateIdx = commonPrefix.length();
                for (String value : values) {
                    Map<String, String> enumVar = new HashMap<String, String>();
                    String enumName;
                    if (truncateIdx == 0) {
                        enumName = value;
                    } else {
                        enumName = value.substring(truncateIdx);
                        if ("".equals(enumName)) {
                            enumName = value;
                        }
                    }
                    enumVar.put("name", toEnumVarName(enumName));
                    enumVar.put("value", value);
                    enumVars.add(enumVar);
                }
                allowableValues.put("enumVars", enumVars);
                // handle default value for enum, e.g. available => StatusEnum.AVAILABLE
                if (var.defaultValue != null) {
                    String enumName = null;
                    for (Map<String, String> enumVar : enumVars) {
                        if (var.defaultValue.equals(enumVar.get("value"))) {
                            enumName = enumVar.get("name");
                            break;
                        }
                    }
                    if (enumName != null) {
                        var.defaultValue = var.datatypeWithEnum + "." + enumName;
                    }
                }
            }
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        if(usesAnyRetrofitLibrary()) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    if (operation.hasConsumes == Boolean.TRUE) {
                        Map<String, String> firstType = operation.consumes.get(0);
                        if (firstType != null) {
                            if ("multipart/form-data".equals(firstType.get("mediaType"))) {
                                operation.isMultipart = Boolean.TRUE;
                            }
                        }
                    }
                    if (operation.returnType == null) {
                        operation.returnType = "Void";
                    }
                    if (usesRetrofit2Library() && StringUtils.isNotEmpty(operation.path) && operation.path.startsWith("/"))
                    	operation.path = operation.path.substring(1);
                }
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

    private static String findCommonPrefixOfVars(List<String> vars) {
        String prefix = StringUtils.getCommonPrefix(vars.toArray(new String[vars.size()]));
        // exclude trailing characters that should be part of a valid variable
        // e.g. ["status-on", "status-off"] => "status-" (not "status-o")
        return prefix.replaceAll("[a-zA-Z0-9]+\\z", "");
    }

    private static String toEnumVarName(String value) {
        String var = value.replaceAll("\\W+", "_").toUpperCase();
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
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

    public void setLocalVariablePrefix(String localVariablePrefix) {
        this.localVariablePrefix = localVariablePrefix;
    }

    public void setSerializeBigDecimalAsString(boolean s) {
        this.serializeBigDecimalAsString = s;
    }

    public Boolean getSerializableModel() {
        return serializableModel;
    }

    public void setSerializableModel(Boolean serializableModel) {
        this.serializableModel = serializableModel;
    }

    private static String sanitizePackageName(String packageName) {
        packageName = packageName.trim(); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        packageName = packageName.replaceAll("[^a-zA-Z0-9_\\.]", "_");
        if(Strings.isNullOrEmpty(packageName)) {
            return "invalidPackageName";
        }
        return packageName;
    }

    public void setFullJavaUtil(boolean fullJavaUtil) {
        this.fullJavaUtil = fullJavaUtil;
    }

    public void setUseRxJava(boolean useRxJava) {
        this.useRxJava = useRxJava;
    }

    public void setDateLibrary(String library) {
        this.dateLibrary = library;
    }
}
