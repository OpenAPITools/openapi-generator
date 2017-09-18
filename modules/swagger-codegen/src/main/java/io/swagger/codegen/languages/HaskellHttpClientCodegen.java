package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.util.*;
import java.util.regex.Pattern;

import org.apache.commons.io.FileUtils;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.SupportingFile;
import io.swagger.util.Json;

import java.io.IOException;
import java.io.File;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class HaskellHttpClientCodegen extends DefaultCodegen implements CodegenConfig {

    // source folder where to write the files
    protected String sourceFolder = "src";
    protected String apiVersion = "0.0.1";

    protected String artifactId = "swagger-haskell-http-client";
    protected String artifactVersion = "1.0.0";

    protected String defaultDateFormat = "%Y-%m-%d";

    protected Boolean useMonadLogger = false;

    // CLI
    public static final String ALLOW_FROMJSON_NULLS = "allowFromJsonNulls";
    public static final String ALLOW_TOJSON_NULLS = "allowToJsonNulls";
    public static final String DATETIME_FORMAT = "dateTimeFormat";
    public static final String DATE_FORMAT = "dateFormat";
    public static final String GENERATE_FORM_URLENCODED_INSTANCES = "generateFormUrlEncodedInstances";
    public static final String GENERATE_LENSES = "generateLenses";
    public static final String GENERATE_MODEL_CONSTRUCTORS = "generateModelConstructors";
    public static final String MODEL_DERIVING = "modelDeriving";
    public static final String STRICT_FIELDS = "strictFields";
    public static final String USE_MONAD_LOGGER = "useMonadLogger";

    // protected String MODEL_IMPORTS = "modelImports";
    // protected String MODEL_EXTENSIONS = "modelExtensions";

    private static final Pattern LEADING_UNDERSCORE = Pattern.compile("^_+");

    static final String MEDIA_TYPE = "mediaType";
    static final String MEDIA_DATA_TYPE = "x-mediaDataType";


    protected Map<String, CodegenParameter> uniqueOptionalParamsByName = new HashMap<String, CodegenParameter>();
    protected Map<String, CodegenModel> modelNames = new HashMap<String, CodegenModel>();
    protected Map<String, Map<String,String>> allMimeTypes = new HashMap<String, Map<String,String>>();
    protected Map<String, String> knownMimeDataTypes = new HashMap<String, String>();
    protected Map<String, Set<String>> modelMimeTypes = new HashMap<String, Set<String>>();
    protected String lastTag = "";
    protected ArrayList<Map<String,String>> unknownMimeTypes = new ArrayList<Map<String,String>>();

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }
    public String getName() {
        return "haskell-http-client";
    }
    public String getHelp() {
        return "Generates a Haskell http-client library.";
    }


    public HaskellHttpClientCodegen() {
        super();

        // override the mapping to keep the original mapping in Haskell
        specialCharReplacements.put("-", "Dash");
        specialCharReplacements.put(">", "GreaterThan");
        specialCharReplacements.put("<", "LessThan");

        // backslash and double quote need double the escapement for both Java and Haskell
        specialCharReplacements.remove("\\");
        specialCharReplacements.remove("\"");
        specialCharReplacements.put("\\\\", "Back_Slash");
        specialCharReplacements.put("\\\"", "Double_Quote");

        // set the output folder here
        outputFolder = "generated-code/haskell-http-client";

        embeddedTemplateDir = templateDir = "haskell-http-client";
        apiPackage = "API";
        modelPackage = "Model";

        // Haskell keywords and reserved function names, taken mostly from https://wiki.haskell.org/Keywords
        setReservedWordsLowerCase(
                Arrays.asList(
                        // Keywords
                        "as", "case", "of",
                        "class", "data", "family",
                        "default", "deriving",
                        "do", "forall", "foreign", "hiding",
                        "if", "then", "else",
                        "import", "infix", "infixl", "infixr",
                        "instance", "let", "in",
                        "mdo", "module", "newtype",
                        "proc", "qualified", "rec",
                        "type", "where"
                )
        );

        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties.put("artifactId", artifactId);
        additionalProperties.put("artifactVersion", artifactVersion);

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("stack.mustache", "", "stack.yaml"));
        supportingFiles.add(new SupportingFile("Setup.mustache", "", "Setup.hs"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));

        supportingFiles.add(new SupportingFile("tests/ApproxEq.mustache", "tests", "ApproxEq.hs"));
        supportingFiles.add(new SupportingFile("tests/Instances.mustache", "tests", "Instances.hs"));
        supportingFiles.add(new SupportingFile("tests/PropMime.mustache", "tests", "PropMime.hs"));
        supportingFiles.add(new SupportingFile("tests/Test.mustache", "tests", "Test.hs"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "Bool",
                        "String",
                        "Int",
                        "Integer",
                        "Float",
                        "Char",
                        "Double",
                        "List",
                        "FilePath"
                )
        );

        typeMapping.clear();
//        typeMapping.put("array", "List");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "Text");
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Integer");
        typeMapping.put("short", "Int");
        typeMapping.put("char", "Char");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Double");
        typeMapping.put("Date", "Day");
        typeMapping.put("DateTime", "UTCTime");
        typeMapping.put("file", "FilePath");
        typeMapping.put("number", "Double");
        typeMapping.put("integer", "Int");
        typeMapping.put("any", "Value");
        typeMapping.put("UUID", "Text");
        typeMapping.put("binary", "ByteString");
        typeMapping.put("ByteArray", "ByteString");

        knownMimeDataTypes.put("application/json", "MimeJSON");
        knownMimeDataTypes.put("application/xml", "MimeXML");
        knownMimeDataTypes.put("application/x-www-form-urlencoded", "MimeFormUrlEncoded");
        knownMimeDataTypes.put("application/octet-stream", "MimeOctetStream");
        knownMimeDataTypes.put("multipart/form-data", "MimeMultipartFormData");
        knownMimeDataTypes.put("text/plain", "MimePlainText");

        importMapping.clear();
        importMapping.put("Map", "qualified Data.Map as Map");

        cliOptions.add(CliOption.newString(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(CliOption.newString(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));

        cliOptions.add(CliOption.newBoolean(ALLOW_FROMJSON_NULLS, "allow JSON Null during model decoding from JSON").defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(ALLOW_TOJSON_NULLS, "allow emitting JSON Null during model encoding to JSON").defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(CliOption.newBoolean(GENERATE_LENSES, "Generate Lens optics for Models").defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(GENERATE_MODEL_CONSTRUCTORS, "Generate smart constructors (only supply required fields) for models").defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(GENERATE_FORM_URLENCODED_INSTANCES, "Generate FromForm/ToForm instances for models that are used by operations that produce or consume application/x-www-form-urlencoded").defaultValue(Boolean.TRUE.toString()));

        cliOptions.add(CliOption.newString(MODEL_DERIVING, "Additional classes to include in the deriving() clause of Models"));
        cliOptions.add(CliOption.newBoolean(STRICT_FIELDS, "Add strictness annotations to all model fields").defaultValue((Boolean.TRUE.toString())));
        cliOptions.add(CliOption.newBoolean(USE_MONAD_LOGGER, "Use the monad-logger package to provide logging (if false, use the katip logging package)").defaultValue((Boolean.FALSE.toString())));

        cliOptions.add(CliOption.newString(DATETIME_FORMAT, "format string used to parse/render a datetime"));
        cliOptions.add(CliOption.newString(DATE_FORMAT, "format string used to parse/render a date").defaultValue(defaultDateFormat));

        cliOptions.add(CliOption.newBoolean(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "hides the timestamp when files were generated").defaultValue(Boolean.TRUE.toString()));

        // cliOptions.add(new CliOption(MODEL_IMPORTS, "Additional imports in the Models file"));
        // cliOptions.add(new CliOption(MODEL_EXTENSIONS, "Additional extensions in the Models file"));
    }

    public void setAllowFromJsonNulls(Boolean value) {
        additionalProperties.put(ALLOW_FROMJSON_NULLS, value);
    }

    public void setAllowToJsonNulls(Boolean value) {
        additionalProperties.put(ALLOW_TOJSON_NULLS, value);
    }

    public void setGenerateModelConstructors(Boolean value) {
        additionalProperties.put(GENERATE_MODEL_CONSTRUCTORS, value);
    }

    public void setGenerateFormUrlEncodedInstances(Boolean value) {
        additionalProperties.put(GENERATE_FORM_URLENCODED_INSTANCES, value);
    }

    public void setGenerateLenses(Boolean value) {
        additionalProperties.put(GENERATE_LENSES, value);
    }

    public void setModelDeriving(String value) {
        if (StringUtils.isBlank(value)) {
            additionalProperties.remove(MODEL_DERIVING);
        } else {
            additionalProperties.put(MODEL_DERIVING, StringUtils.join(value.split(" "), ","));
        }
    }

    public void setDateTimeFormat(String value) {
        if (StringUtils.isBlank(value)) {
            additionalProperties.remove(DATETIME_FORMAT);
        } else {
            additionalProperties.put(DATETIME_FORMAT, value);
        }

    }

    public void setDateFormat(String value) {
        if (StringUtils.isBlank(value)) {
            additionalProperties.remove(DATE_FORMAT);
        } else {
            additionalProperties.put(DATE_FORMAT, value);
        }
    }

    public void setStrictFields(Boolean value) {
        additionalProperties.put("x-strictFields", value);
    }

    public void setUseMonadLogger(Boolean value) {
        additionalProperties.put("x-useMonadLogger", value);
        this.useMonadLogger = value;
    }

    @Override
    public void processOpts() {
        super.processOpts();
        // default HIDE_GENERATION_TIMESTAMP to true
        if (additionalProperties.containsKey(CodegenConstants.HIDE_GENERATION_TIMESTAMP)) {
            convertPropertyToBooleanAndWriteBack(CodegenConstants.HIDE_GENERATION_TIMESTAMP);
        } else {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, true);
        }

        if (additionalProperties.containsKey(ALLOW_FROMJSON_NULLS)) {
            setAllowFromJsonNulls(convertPropertyToBoolean(ALLOW_FROMJSON_NULLS));
        } else {
            setAllowFromJsonNulls(true);
        }

        if (additionalProperties.containsKey(ALLOW_TOJSON_NULLS)) {
            setAllowToJsonNulls(convertPropertyToBoolean(ALLOW_TOJSON_NULLS));
        } else {
            setAllowToJsonNulls(false);
        }

        if (additionalProperties.containsKey(GENERATE_MODEL_CONSTRUCTORS)) {
            setGenerateModelConstructors(convertPropertyToBoolean(GENERATE_MODEL_CONSTRUCTORS));
        } else {
            setGenerateModelConstructors(true);
        }

        if (additionalProperties.containsKey(GENERATE_FORM_URLENCODED_INSTANCES)) {
            setGenerateFormUrlEncodedInstances(convertPropertyToBoolean(GENERATE_FORM_URLENCODED_INSTANCES));
        } else {
            setGenerateFormUrlEncodedInstances(true);
        }

        if (additionalProperties.containsKey(GENERATE_LENSES)) {
            setGenerateLenses(convertPropertyToBoolean(GENERATE_LENSES));
        } else {
            setGenerateLenses(true);
        }

        if (additionalProperties.containsKey(MODEL_DERIVING)) {
            setModelDeriving(additionalProperties.get(MODEL_DERIVING).toString());
        } else {
            setModelDeriving("");
        }

        if (additionalProperties.containsKey(DATETIME_FORMAT)) {
            setDateTimeFormat(additionalProperties.get(DATETIME_FORMAT).toString());
        } else {
            setDateTimeFormat(null); // default should be null
        }

        if (additionalProperties.containsKey(DATE_FORMAT)) {
            setDateFormat(additionalProperties.get(DATE_FORMAT).toString());
        } else {
            setDateFormat(defaultDateFormat);
        }

        if (additionalProperties.containsKey(STRICT_FIELDS)) {
            setStrictFields(convertPropertyToBoolean(STRICT_FIELDS));
        } else {
            setStrictFields(true);
        }
        if (additionalProperties.containsKey(USE_MONAD_LOGGER)) {
            setUseMonadLogger(convertPropertyToBoolean(USE_MONAD_LOGGER));
        } else {
            setUseMonadLogger(false);
        }

    }

//    @Override
//    public String apiFileFolder() {
//        String apiName = (String)additionalProperties.get("title");
//        return outputFolder + File.separator + "lib/" + apiName;
//    }
//    @Override
//    public String modelFileFolder() {
//        String apiName = (String)additionalProperties.get("title");
//        return outputFolder + File.separator + "lib/" + apiName;
//    }

    @Override
    public void preprocessSwagger(Swagger swagger) {
        // From the title, compute a reasonable name for the package and the API
        String title = swagger.getInfo().getTitle();

        // Drop any API suffix
        if (title == null) {
            title = "Swagger";
        } else {
            title = title.trim();
            if (title.toUpperCase().endsWith("API")) {
                title = title.substring(0, title.length() - 3);
            }
        }

        String[] words = title.split(" ");

        // The package name is made by appending the lowercased words of the title interspersed with dashes
        List<String> wordsLower = new ArrayList<String>();
        for (String word : words) {
            wordsLower.add(word.toLowerCase());
        }
        String cabalName = StringUtils.join(wordsLower, "-");
        String pathsName = StringUtils.join(wordsLower, "_");

        // The API name is made by appending the capitalized words of the title
        List<String> wordsCaps = new ArrayList<String>();
        for (String word : words) {
            wordsCaps.add(firstLetterToUpper(word));
        }
        String apiName = StringUtils.join(wordsCaps, "");

        // Set the filenames to write for the API

        // root
        supportingFiles.add(new SupportingFile("haskell-http-client.cabal.mustache", "", cabalName + ".cabal"));

        // lib
        supportingFiles.add(new SupportingFile("TopLevel.mustache", "lib/", apiName + ".hs"));
        supportingFiles.add(new SupportingFile("Client.mustache", "lib/" + apiName, "Client.hs"));

        supportingFiles.add(new SupportingFile("API.mustache", "lib/" + apiName, "API.hs"));
        supportingFiles.add(new SupportingFile("Model.mustache", "lib/" + apiName, "Model.hs"));
        supportingFiles.add(new SupportingFile("MimeTypes.mustache", "lib/" + apiName, "MimeTypes.hs"));

        // logger
        supportingFiles.add(new SupportingFile(useMonadLogger ? "LoggingMonadLogger.mustache" : "LoggingKatip.mustache", "lib/" + apiName, "Logging.hs"));

        // modelTemplateFiles.put("API.mustache", ".hs");
        // apiTemplateFiles.put("Model.mustache", ".hs");

        // lens
        if ((boolean)additionalProperties.get(GENERATE_LENSES)) {
            supportingFiles.add(new SupportingFile("Lens.mustache", "lib/" + apiName, "Lens.hs"));
        }

        additionalProperties.put("title", apiName);
        additionalProperties.put("titleLower", firstLetterToLower(apiName));
        additionalProperties.put("package", cabalName);
        additionalProperties.put("pathsName", pathsName);
        additionalProperties.put("requestType", apiName + "Request");
        additionalProperties.put("configType", apiName + "Config");
        additionalProperties.put("swaggerVersion", swagger.getSwagger());

        // prepend '
//        List<Map<String, Object>> replacements = new ArrayList<>();
//        Object[] replacementChars = specialCharReplacements.keySet().toArray();
//        for (int i = 0; i < replacementChars.length; i++) {
//            String c = (String) replacementChars[i];
//            Map<String, Object> o = new HashMap<>();
//            o.put("char", c);
//            o.put("replacement", "'" + specialCharReplacements.get(c));
//            o.put("hasMore", i != replacementChars.length - 1);
//            replacements.add(o);
//        }
//        additionalProperties.put("specialCharReplacements", replacements);

        //copy input swagger to output folder
        try {
            String swaggerJson = Json.pretty(swagger);
            FileUtils.writeStringToFile(new File(outputFolder + File.separator + "swagger.json"), swaggerJson);
        } catch (IOException e) {
            throw new RuntimeException(e.getMessage(), e.getCause());
        }

        super.preprocessSwagger(swagger);
    }


    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "[" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return "Map.Map String " + getTypeDeclaration(inner);
        }
        return fixModelChars(super.getTypeDeclaration(p));
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type))
                return toModelName(type);
        } else if (swaggerType == "object") {
            type = "Value";
        } else if (typeMapping.containsValue(swaggerType)) {
            type = swaggerType + "_";
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
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
            return "(Map.Map Text " + inner + ")";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            // Return only the inner type; the wrapping with QueryList is done
            // somewhere else, where we have access to the collection format.
            return inner;
        } else {
            return null;
        }
    }

    @Override
    public CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger) {
        CodegenOperation op = super.fromOperation(resourcePath, httpMethod, operation, definitions, swagger);

        op.vendorExtensions.put("x-baseOperationId", op.operationId);
        op.vendorExtensions.put("x-haddockPath", String.format("%s %s", op.httpMethod, op.path.replace("/", "\\/")));
        op.operationId = toHsVarName(op.operationId);
        op.vendorExtensions.put("x-operationType", toHsTypeName(op.operationId));
        op.vendorExtensions.put("x-hasBodyOrFormParam", op.getHasBodyParam() || op.getHasFormParams());

        for (CodegenParameter param : op.allParams) {
            param.vendorExtensions.put("x-operationType", capitalize(op.operationId));
            param.vendorExtensions.put("x-isBodyOrFormParam", param.isBodyParam || param.isFormParam);
            if (!StringUtils.isBlank(param.collectionFormat)) {
                param.vendorExtensions.put("x-collectionFormat", mapCollectionFormat(param.collectionFormat));
            }
            if (!param.required) {
                op.vendorExtensions.put("x-hasOptionalParams", true);

                String paramNameType = capitalize(param.paramName);

                if (uniqueOptionalParamsByName.containsKey(paramNameType)) {
                    CodegenParameter lastParam = this.uniqueOptionalParamsByName.get(paramNameType);
                    if (lastParam.dataType != null && lastParam.dataType.equals(param.dataType)) {
                        param.vendorExtensions.put("x-duplicate", true);
                    } else {
                        paramNameType = paramNameType + param.dataType;
                        while (modelNames.containsKey(paramNameType)) {
                            paramNameType = generateNextName(paramNameType);
                        }
                    }
                } else {
                    while (modelNames.containsKey(paramNameType)) {
                        paramNameType = generateNextName(paramNameType);
                    }
                    uniqueOptionalParamsByName.put(paramNameType, param);
                }

                param.vendorExtensions.put("x-paramNameType", paramNameType);
                op.vendorExtensions.put("x-hasBodyOrFormParam", op.getHasBodyParam() || op.getHasFormParams());
            }
        }
        if (op.getHasPathParams()) {
            String remainingPath = op.path;
            for (CodegenParameter param : op.pathParams) {
                param.paramName = toHsVarName(param.paramName);
                String[] pieces = remainingPath.split("\\{" + param.baseName + "\\}");
                if (pieces.length == 0)
                    throw new RuntimeException("paramName {" + param.baseName + "} not in path " + op.path);
                if (pieces.length > 2)
                    throw new RuntimeException("paramName {" + param.baseName + "} found multiple times in path " + op.path);
                if (pieces.length == 2) {
                    param.vendorExtensions.put("x-pathPrefix", pieces[0]);
                    remainingPath = pieces[1];
                } else {
                    if (remainingPath.startsWith("{" + param.baseName + "}")) {
                        remainingPath = pieces[0];
                    } else {
                        param.vendorExtensions.put("x-pathPrefix", pieces[0]);
                        remainingPath = "";
                    }
                }
            }
            op.vendorExtensions.put("x-hasPathParams", true);
            if (remainingPath.length() > 0) {
                op.vendorExtensions.put("x-pathSuffix", remainingPath);
            }
        } else {
            op.vendorExtensions.put("x-hasPathParams", false);
            op.vendorExtensions.put("x-pathSuffix", op.path);
        }
        for (CodegenParameter param : op.queryParams) {
        }
        for (CodegenParameter param : op.headerParams) {
        }
        for (CodegenParameter param : op.bodyParams) {
        }
        for (CodegenParameter param : op.formParams) {
        }

        if (op.hasConsumes) {
            for (Map<String, String> m : op.consumes) {
                processMediaType(op,m);
            }
            if (isMultipart(op.consumes)) {
                op.isMultipart = Boolean.TRUE;
            }
        }
        if (op.hasProduces) {
            for (Map<String, String> m : op.produces) {
                processMediaType(op,m);
            }
        }

        String returnType = op.returnType;
        if (returnType == null || returnType.equals("null")) {
            if(op.hasProduces) {
                returnType = "res";
                op.vendorExtensions.put("x-hasUnknownReturn", true);
            } else {
                returnType = "NoContent";
            }
        }
        if (returnType.indexOf(" ") >= 0) {
            returnType = "(" + returnType + ")";
        }
        op.vendorExtensions.put("x-returnType", returnType);


        return op;
    }


    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> ret = super.postProcessOperations(objs);

        HashMap<String, Object> pathOps = (HashMap<String, Object>)ret.get("operations");
        ArrayList<CodegenOperation> ops = (ArrayList<CodegenOperation>)pathOps.get("operation");
        if(ops.size() > 0) {
            ops.get(0).vendorExtensions.put("x-hasNewTag", true);
        }

        additionalProperties.put("x-hasUnknownMimeTypes", !unknownMimeTypes.isEmpty());
        additionalProperties.put("x-unknownMimeTypes", unknownMimeTypes);

        return ret;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        for (Object o : allModels) {
            HashMap<String, Object> h = (HashMap<String, Object>) o;
            CodegenModel m = (CodegenModel) h.get("model");
            if (modelMimeTypes.containsKey(m.classname)) {
                Set<String> mimeTypes = modelMimeTypes.get(m.classname);
                m.vendorExtensions.put("x-mimeTypes", mimeTypes);
                if ((boolean)additionalProperties.get(GENERATE_FORM_URLENCODED_INSTANCES) && mimeTypes.contains("MimeFormUrlEncoded")) {
                    Boolean hasMimeFormUrlEncoded = true;
                    for (CodegenProperty v : m.vars) {
                        if (!(v.isPrimitiveType || v.isString || v.isDate || v.isDateTime)) {
                            hasMimeFormUrlEncoded = false;
                        }
                    }
                    if (hasMimeFormUrlEncoded) {
                        m.vendorExtensions.put("x-hasMimeFormUrlEncoded", true);
                    }
                }
            }

        }
        return objs;
    }

    @Override
    public CodegenModel fromModel(String name, Model mod, Map<String, Model> allDefinitions) {
        CodegenModel model = super.fromModel(name, mod, allDefinitions);

        // Clean up the class name to remove invalid characters
        model.classname = fixModelChars(model.classname);
        if (typeMapping.containsValue(model.classname)) {
            model.classname += "_";
        }
        while (uniqueOptionalParamsByName.containsKey(model.classname)) {
            model.classname = generateNextName(model.classname);
        }

        // From the model name, compute the prefix for the fields.
        String prefix = camelize(model.classname, true);
        for (CodegenProperty prop : model.vars) {
            prop.name = toVarName(prefix + camelize(fixOperatorChars(prop.name)));
        }

        //String dataOrNewtype = "data";
        // check if it's a ModelImpl before casting
        if (!(mod instanceof ModelImpl)) {
            return model;
        }

        // Create newtypes for things with non-object types
//        String modelType = ((ModelImpl)  mod).getType();
//        if(modelType != "object" && typeMapping.containsKey(modelType)) {
//            String newtype = typeMapping.get(modelType);
//            model.vendorExtensions.put("x-customNewtype", newtype);
//        }

        modelNames.put(model.classname, model);
        return model;
    }

    @Override
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
        CodegenParameter p = super.fromParameter(param, imports);
        p.paramName = toHsVarName(p.baseName);
        p.dataType = fixModelChars(p.dataType);
        return p;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }


    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("{-", "{_-").replace("-}", "-_}");
    }

    @Override
    public boolean isDataTypeFile(String dataType) {
        return dataType != null && dataType.equals("FilePath");
    }

    @Override
    public boolean isDataTypeBinary(final String dataType) {
        return dataType != null && dataType.equals("B.ByteString");
    }

    private void processMediaType(CodegenOperation op, Map<String, String> m) {
        String mediaType = m.get(MEDIA_TYPE);

        if(StringUtils.isBlank(mediaType)) return;

        String[] mediaTypeParts = mediaType.split("/",2);
        if(mediaTypeParts.length > 1) {
            m.put("x-mediaMainType", mediaTypeParts[0]);
            m.put("x-mediaSubType", mediaTypeParts[1]);
        } else {
            m.put("x-mediaMainType", mediaTypeParts[0]);
            m.put("x-mediaSubType", "");
        }

        String mimeType = getMimeDataType(mediaType);
        m.put(MEDIA_DATA_TYPE, mimeType);

        allMimeTypes.put(mediaType, m);
        if(!knownMimeDataTypes.containsKey(mediaType) && !unknownMimeTypes.contains(m)) {
            unknownMimeTypes.add(m);
        }
        for (CodegenParameter param : op.allParams) {
            if (param.isBodyParam || param.isFormParam && (!param.isPrimitiveType && !param.isListContainer && !param.isMapContainer)) {
                Set<String> mimeTypes = modelMimeTypes.containsKey(param.dataType) ? modelMimeTypes.get(param.dataType) : new HashSet();
                mimeTypes.add(mimeType);
                modelMimeTypes.put(param.dataType, mimeTypes);
            }
        }
    }

    public String firstLetterToUpper(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toUpperCase();
        } else {
            return word.substring(0, 1).toUpperCase() + word.substring(1);
        }
    }

    public String firstLetterToLower(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toLowerCase();
        } else {
            return word.substring(0, 1).toLowerCase() + word.substring(1);
        }
    }

    private String mapCollectionFormat(String collectionFormat) {
        switch (collectionFormat) {
            case "csv":
                return "CommaSeparated";
            case "tsv":
                return "TabSeparated";
            case "ssv":
                return "SpaceSeparated";
            case "pipes":
                return "PipeSeparated";
            case "multi":
                return "MultiParamArray";
            default:
                throw new UnsupportedOperationException();
        }
    }

    private String getMimeDataType(String mimeType) {
        if (StringUtils.isBlank(mimeType)) {
            return "MimeNoContent";
        }
        if (knownMimeDataTypes.containsKey(mimeType)) {
            return knownMimeDataTypes.get(mimeType);
        }
        String shortenedName = mimeType.replaceFirst("application/","");
        return "Mime" + toHsTypeName(shortenedName);
    }

    private String toHsVarName(String paramName) {
        return toVarName(camelize(fixOperatorChars(fixModelChars(paramName)), true));
    }

    private String toHsTypeName(String paramName) {
        return toHsTypeName(paramName, "");
    }

    private String toHsTypeName(String paramName, String modelCharReplacement) {
        return camelize(fixOperatorChars(fixModelChars(paramName, modelCharReplacement)), false);
    }

    private String fixOperatorChars(String string) {
        if(string == null) return null;
        StringBuilder sb = new StringBuilder();
        String name = string;
        //Check if it is a reserved word, in which case the underscore is added when property name is generated.
        if (string.startsWith("_")) {
            if (reservedWords.contains(string.substring(1, string.length()))) {
                name = string.substring(1, string.length());
            } else if (reservedWordsMappings.containsValue(string)) {
                name = LEADING_UNDERSCORE.matcher(string).replaceFirst("");
            }
        }
        // prepend '
        for (char c : name.toCharArray()) {
            String cString = String.valueOf(c);
            if (specialCharReplacements.containsKey(cString)) {
//                sb.append("'");
                sb.append(specialCharReplacements.get(cString));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    // Remove characters from a string that do not belong in a model classname
    private String fixModelChars(String string, String replacement) {
        if(string == null) return null;
        return string.replace(".", replacement).replace("-", replacement);
    }

    private String fixModelChars(String string) {
        return fixModelChars(string, "");
    }

    private String capitalize(String word) {
        if(word == null) return null;
        if (word.length() > 0) {
            word = word.substring(0, 1).toUpperCase() + word.substring(1);
        }

        return word;
    }
    private static String generateNextName(String name) {
        Pattern pattern = Pattern.compile("\\d+\\z");
        Matcher matcher = pattern.matcher(name);
        if (matcher.find()) {
            String numStr = matcher.group();
            int num = Integer.parseInt(numStr) + 1;
            return name.substring(0, name.length() - numStr.length()) + num;
        } else {
            return name + "2";
        }
    }
    private static boolean isMultipart(List<Map<String, String>> consumes) {
        for(Map<String, String> consume : consumes) {
            if (consume != null) {
                if ("multipart/form-data".equals(consume.get(MEDIA_TYPE))) {
                    return true;
                }
            }
        }
        return false;
    }

//    private boolean isModelledType(CodegenParameter param) {
//        return isModelledType(param.baseType == null ? param.dataType : param.baseType);
//    }
//
//    private boolean isModelledType(String typeName) {
//        return !languageSpecificPrimitives.contains(typeName) && !typeMapping.values().contains(typeName);
//    }
}
