/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class HaskellHttpClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(HaskellHttpClientCodegen.class);

    // source folder where to write the files
    protected String sourceFolder = "lib";

    protected String defaultDateFormat = "%Y-%m-%d";
    protected String defaultCabalVersion = "0.1.0.0";
    protected String modulePath = null;

    protected Boolean useKatip = true;
    protected Boolean allowNonUniqueOperationIds = false;
    protected Boolean genEnums = true;

    // CLI PROPS
    public static final String PROP_ALLOW_FROMJSON_NULLS = "allowFromJsonNulls";
    public static final String PROP_ALLOW_NONUNIQUE_OPERATION_IDS = "allowNonUniqueOperationIds";
    public static final String PROP_ALLOW_TOJSON_NULLS = "allowToJsonNulls";
    public static final String PROP_BASE_MODULE = "baseModule";
    public static final String PROP_CABAL_PACKAGE = "cabalPackage";
    public static final String PROP_CABAL_VERSION = "cabalVersion";
    public static final String PROP_CONFIG_TYPE = "configType";
    public static final String PROP_DATETIME_FORMAT = "dateTimeFormat";
    public static final String PROP_DATETIME_PARSE_FORMAT = "dateTimeParseFormat";
    public static final String PROP_CUSTOM_TEST_INSTANCE_MODULE = "customTestInstanceModule";
    public static final String PROP_DATE_FORMAT = "dateFormat";
    public static final String PROP_GENERATE_ENUMS = "generateEnums";
    public static final String PROP_GENERATE_FORM_URLENCODED_INSTANCES = "generateFormUrlEncodedInstances";
    public static final String PROP_GENERATE_LENSES = "generateLenses";
    public static final String PROP_GENERATE_MODEL_CONSTRUCTORS = "generateModelConstructors";
    public static final String PROP_INLINE_MIME_TYPES = "inlineMimeTypes";
    public static final String PROP_MODEL_DERIVING = "modelDeriving";
    public static final String PROP_REQUEST_TYPE = "requestType";
    public static final String PROP_STRICT_FIELDS = "strictFields";
    public static final String PROP_USE_KATIP = "useKatip";

    // protected String MODEL_IMPORTS = "modelImports";
    // protected String MODEL_EXTENSIONS = "modelExtensions";

    private static final Pattern LEADING_UNDERSCORE = Pattern.compile("^_+");

    static final String MEDIA_TYPE = "mediaType";
    static final String MIME_NO_CONTENT = "MimeNoContent";
    static final String MIME_ANY = "MimeAny";

    // vendor extensions. These must follow our convention of x- prefixed and lower-kebab cased.
    static final String VENDOR_EXTENSION_X_UNKNOWN_MIME_TYPES = "x-unknown-mime-types";
    static final String VENDOR_EXTENSION_X_COLLECTION_FORMAT = "x-collection-format";
    static final String VENDOR_EXTENSION_X_HADDOCK_PATH = "x-haddock-path";
    static final String VENDOR_EXTENSION_X_HAS_BODY_OR_FORM_PARAM = "x-has-body-or-form-param";
    static final String VENDOR_EXTENSION_X_HAS_MIME_FORM_URL_ENCODED = "x-has-mime-form-url-encoded";
    static final String VENDOR_EXTENSION_X_HAS_NEW_TAG = "x-has-new-tag";
    static final String VENDOR_EXTENSION_X_HAS_OPTIONAL_PARAMS = "x-has-optional-params";
    static final String VENDOR_EXTENSION_X_HAS_UNKNOWN_RETURN = "x-has-unknown-return";
    static final String VENDOR_EXTENSION_X_INLINE_CONTENT_TYPE = "x-inline-content-type";
    static final String VENDOR_EXTENSION_X_INLINE_ACCEPT = "x-inline-accept";
    static final String VENDOR_EXTENSION_X_IS_BODY_OR_FORM_PARAM = "x-is-body-or-form-param";
    static final String VENDOR_EXTENSION_X_IS_MAYBE_VALUE = "x-is-maybe-value";
    static final String VENDOR_EXTENSION_X_DATA_TYPE = "x-data-type";
    static final String VENDOR_EXTENSION_X_MIME_TYPES = "x-mime-types";
    static final String VENDOR_EXTENSION_X_OPERATION_TYPE = "x-operation-type";
    static final String VENDOR_EXTENSION_X_PARAM_NAME_TYPE = "x-param-name-type";
    static final String VENDOR_EXTENSION_X_RETURN_TYPE = "x-return-type";

    // note; newtype is a single lowercase word in Haskell (not separated by hyphen)
    static final String X_NEWTYPE = "x-newtype";
    static final String VENDOR_EXTENSION_X_ENUM = "x-enum";
    static final String VENDOR_EXTENSION_X_PATH = "x-path";


    // additional properties
    // FIXME: These appear to be used as vendor extensions in the additional properties collection.
    static final String X_ALL_UNIQUE_PARAMS = "x-allUniqueParams";
    static final String X_ALL_IMPORT_MAPPINGS = "x-allImportMappings";
    static final String X_HAS_ENUM_SECTION = "x-hasEnumSection";
    static final String X_HAS_IMPORT_MAPPINGS = "x-hasImportMappings";
    static final String X_HAS_UNKNOWN_MIME_TYPES = "x-hasUnknownMimeTypes";
    static final String X_IS_BODY_PARAM = "x-isBodyParam";
    static final String X_MEDIA_DATA_TYPE = "x-mediaDataType";
    static final String X_ENUM_VALUES = "x-enumValues";
    static final String X_MEDIA_IS_JSON = "x-mediaIsJson";
    static final String X_MEDIA_IS_WILDCARD = "x-mediaIsWildcard";
    static final String X_STRICT_FIELDS = "x-strictFields";
    static final String X_ALL_UNIQUE_IMPORT_PATHS = "x-allUniqueImportPaths";
    static final String X_USE_KATIP = "x-useKatip";
    static final String X_ALLOW_NONUNIQUE_OPERATION_IDS = "x-allowNonUniqueOperationIds";

    protected ArrayList<Map<String, String>> unknownMimeTypes = new ArrayList<>();
    protected Map<String, Map<String, Object>> uniqueParamNameTypes = new HashMap<>();
    protected Map<String, Set<String>> modelMimeTypes = new HashMap<>();
    protected Map<String, String> knownMimeDataTypes = new HashMap<>();
    protected Set<String> typeNames = new HashSet<String>();
    protected Set<String> modelTypeNames = new HashSet<String>();

    final private static Pattern JSON_MIME_PATTERN = Pattern.compile("(?i)application/.*json(;.*)?");

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

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        this.prependFormOrBodyParameters = true;

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
        //modelPackage = "Model";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

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
                        "type", "where", "pure", "return",
                        "Accept", "ContentType"
                )
        );

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
                        "FilePath",
                        "Text"
                )
        );

        typeMapping.clear();
        // prim
        typeMapping.put("boolean", "Bool");
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Integer");
        typeMapping.put("short", "Int");
        typeMapping.put("char", "Char");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Double");
        typeMapping.put("number", "Double");
        typeMapping.put("BigDecimal", "Double");
        typeMapping.put("integer", "Int");
        typeMapping.put("file", "FilePath");
        // lib
        typeMapping.put("string", "Text");
        typeMapping.put("UUID", "Text");
        typeMapping.put("URI", "Text");
        typeMapping.put("any", "A.Value");
        typeMapping.put("set", "Set.Set");
        // newtype
        typeMapping.put("binary", "Binary");
        typeMapping.put("ByteArray", "ByteArray");
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "DateTime");

        knownMimeDataTypes.put("application/json", "MimeJSON");
        knownMimeDataTypes.put("application/xml", "MimeXML");
        knownMimeDataTypes.put("application/x-www-form-urlencoded", "MimeFormUrlEncoded");
        knownMimeDataTypes.put("application/octet-stream", "MimeOctetStream");
        knownMimeDataTypes.put("multipart/form-data", "MimeMultipartFormData");
        knownMimeDataTypes.put("text/plain", "MimePlainText");
        knownMimeDataTypes.put("*/*", MIME_ANY);

        importMapping.clear();

        //cliOptions.add(CliOption.newString(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        //cliOptions.add(CliOption.newString(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));

        cliOptions.add(CliOption.newString(PROP_CABAL_PACKAGE, "Set the cabal package name, which consists of one or more alphanumeric words separated by hyphens"));
        cliOptions.add(CliOption.newString(PROP_CABAL_VERSION, "Set the cabal version number, consisting of a sequence of one or more integers separated by dots"));
        cliOptions.add(CliOption.newString(PROP_BASE_MODULE, "Set the base module namespace"));
        cliOptions.add(CliOption.newString(PROP_REQUEST_TYPE, "Set the name of the type used to generate requests"));
        cliOptions.add(CliOption.newString(PROP_CONFIG_TYPE, "Set the name of the type used for configuration"));

        cliOptions.add(CliOption.newBoolean(PROP_ALLOW_FROMJSON_NULLS, "allow JSON Null during model decoding from JSON").defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(PROP_ALLOW_TOJSON_NULLS, "allow emitting JSON Null during model encoding to JSON").defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(CliOption.newBoolean(PROP_ALLOW_NONUNIQUE_OPERATION_IDS, "allow different API modules to contain the same operationId. Each API must be imported qualified").defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(CliOption.newBoolean(PROP_GENERATE_LENSES, "Generate Lens optics for Models").defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(PROP_GENERATE_MODEL_CONSTRUCTORS, "Generate smart constructors (only supply required fields) for models").defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(PROP_GENERATE_ENUMS, "Generate specific datatypes for OpenAPI enums").defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(PROP_GENERATE_FORM_URLENCODED_INSTANCES, "Generate FromForm/ToForm instances for models that are used by operations that produce or consume application/x-www-form-urlencoded").defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(PROP_INLINE_MIME_TYPES, "Inline (hardcode) the content-type and accept parameters on operations, when there is only 1 option").defaultValue(Boolean.TRUE.toString()));


        cliOptions.add(CliOption.newString(PROP_MODEL_DERIVING, "Additional classes to include in the deriving() clause of Models"));
        cliOptions.add(CliOption.newBoolean(PROP_STRICT_FIELDS, "Add strictness annotations to all model fields").defaultValue((Boolean.TRUE.toString())));
        cliOptions.add(CliOption.newBoolean(PROP_USE_KATIP, "Sets the default value for the UseKatip cabal flag. If true, the katip package provides logging instead of monad-logger").defaultValue((Boolean.TRUE.toString())));

        cliOptions.add(CliOption.newString(PROP_DATETIME_FORMAT, "format string used to parse/render a datetime"));
        cliOptions.add(CliOption.newString(PROP_DATETIME_PARSE_FORMAT, "overrides the format string used to parse a datetime"));
        cliOptions.add(CliOption.newString(PROP_DATE_FORMAT, "format string used to parse/render a date").defaultValue(defaultDateFormat));

        cliOptions.add(CliOption.newString(PROP_CUSTOM_TEST_INSTANCE_MODULE, "test module used to provide typeclass instances for types not known by the generator"));

        cliOptions.add(CliOption.newBoolean(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC).defaultValue(Boolean.TRUE.toString()));

    }

    public void setAllowNonUniqueOperationIds(Boolean value) {
        additionalProperties.put(X_ALLOW_NONUNIQUE_OPERATION_IDS, value);
        this.allowNonUniqueOperationIds = value;
    }

    public void setAllowFromJsonNulls(Boolean value) {
        additionalProperties.put(PROP_ALLOW_FROMJSON_NULLS, value);
    }

    public void setAllowToJsonNulls(Boolean value) {
        additionalProperties.put(PROP_ALLOW_TOJSON_NULLS, value);
    }

    public void setGenerateModelConstructors(Boolean value) {
        additionalProperties.put(PROP_GENERATE_MODEL_CONSTRUCTORS, value);
    }

    public void setGenerateEnums(Boolean value) {
        additionalProperties.put(PROP_GENERATE_ENUMS, value);
        genEnums = value;
    }

    public void setGenerateFormUrlEncodedInstances(Boolean value) {
        additionalProperties.put(PROP_GENERATE_FORM_URLENCODED_INSTANCES, value);
    }

    public void setInlineMimeTypes(Boolean value) {
        additionalProperties.put(PROP_INLINE_MIME_TYPES, value);
    }

    public void setGenerateLenses(Boolean value) {
        additionalProperties.put(PROP_GENERATE_LENSES, value);
    }

    public void setModelDeriving(String value) {
        if (StringUtils.isBlank(value)) {
            additionalProperties.remove(PROP_MODEL_DERIVING);
        } else {
            additionalProperties.put(PROP_MODEL_DERIVING, StringUtils.join(value.split(" "), ","));
        }
    }

    public void setDateTimeFormat(String value) { setStringProp(PROP_DATETIME_FORMAT, value); }

    public void setDateTimeParseFormat(String value) { setStringProp(PROP_DATETIME_PARSE_FORMAT, value); }

    public void setDateFormat(String value) { setStringProp(PROP_DATE_FORMAT, value); }

    public void setCabalPackage(String value) {
        setStringProp(PROP_CABAL_PACKAGE, value);
    }

    public void setCabalVersion(String value) {
        setStringProp(PROP_CABAL_VERSION, value);
    }

    public void setBaseModule(String value) {
        setStringProp(PROP_BASE_MODULE, value);
    }

    public void setRequestType(String value) {
        setStringProp(PROP_REQUEST_TYPE, value);
    }

    public void setConfigType(String value) {
        setStringProp(PROP_CONFIG_TYPE, value);
    }

    public void setStrictFields(Boolean value) {
        additionalProperties.put(X_STRICT_FIELDS, value);
    }

    public void setUseKatip(Boolean value) {
        additionalProperties.put(X_USE_KATIP, value);
        this.useKatip = value;
    }

    public void setCustomTestInstanceModule(String value) { setStringProp(PROP_CUSTOM_TEST_INSTANCE_MODULE, value); }

    private void setStringProp(String key, String value) {
        if (StringUtils.isBlank(value)) {
            additionalProperties.remove(key);
        } else {
            additionalProperties.put(key, value);
        }
    }

    private String getStringProp(String key) {
        return (String) additionalProperties.get(key);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("HASKELL_POST_PROCESS_FILE"))) {
            LOGGER.info("Hint: Environment variable HASKELL_POST_PROCESS_FILE not defined so the Haskell code may not be properly formatted. To define it, try 'export HASKELL_POST_PROCESS_FILE=\"$HOME/.local/bin/hfmt -w\"' (Linux/Mac)");
        }

        if (additionalProperties.containsKey(PROP_ALLOW_FROMJSON_NULLS)) {
            setAllowFromJsonNulls(convertPropertyToBoolean(PROP_ALLOW_FROMJSON_NULLS));
        } else {
            setAllowFromJsonNulls(true);
        }

        if (additionalProperties.containsKey(PROP_ALLOW_TOJSON_NULLS)) {
            setAllowToJsonNulls(convertPropertyToBoolean(PROP_ALLOW_TOJSON_NULLS));
        } else {
            setAllowToJsonNulls(false);
        }

        if (additionalProperties.containsKey(PROP_ALLOW_NONUNIQUE_OPERATION_IDS)) {
            setAllowNonUniqueOperationIds(convertPropertyToBoolean(PROP_ALLOW_NONUNIQUE_OPERATION_IDS));
        } else {
            setAllowNonUniqueOperationIds(false);
        }

        if (additionalProperties.containsKey(PROP_GENERATE_MODEL_CONSTRUCTORS)) {
            setGenerateModelConstructors(convertPropertyToBoolean(PROP_GENERATE_MODEL_CONSTRUCTORS));
        } else {
            setGenerateModelConstructors(true);
        }

        if (additionalProperties.containsKey(PROP_GENERATE_ENUMS)) {
            setGenerateEnums(convertPropertyToBoolean(PROP_GENERATE_ENUMS));
        } else {
            setGenerateEnums(true);
        }

        if (additionalProperties.containsKey(PROP_GENERATE_FORM_URLENCODED_INSTANCES)) {
            setGenerateFormUrlEncodedInstances(convertPropertyToBoolean(PROP_GENERATE_FORM_URLENCODED_INSTANCES));
        } else {
            setGenerateFormUrlEncodedInstances(true);
        }

        if (additionalProperties.containsKey(PROP_INLINE_MIME_TYPES)) {
            setInlineMimeTypes(convertPropertyToBoolean(PROP_INLINE_MIME_TYPES));
        } else {
            setInlineMimeTypes(true);
        }

        if (additionalProperties.containsKey(PROP_GENERATE_LENSES)) {
            setGenerateLenses(convertPropertyToBoolean(PROP_GENERATE_LENSES));
        } else {
            setGenerateLenses(true);
        }

        if (additionalProperties.containsKey(PROP_MODEL_DERIVING)) {
            setModelDeriving(additionalProperties.get(PROP_MODEL_DERIVING).toString());
        } else {
            setModelDeriving("");
        }

        if (additionalProperties.containsKey(PROP_DATETIME_FORMAT)) {
            setDateTimeFormat(additionalProperties.get(PROP_DATETIME_FORMAT).toString());
        } else {
            setDateTimeFormat(null); // default should be null
        }

        if (additionalProperties.containsKey(PROP_DATETIME_PARSE_FORMAT)) {
            setDateTimeParseFormat(additionalProperties.get(PROP_DATETIME_PARSE_FORMAT).toString());
        } else {
            setDateTimeParseFormat(null); // default should be null
        }

        if (additionalProperties.containsKey(PROP_DATE_FORMAT)) {
            setDateFormat(additionalProperties.get(PROP_DATE_FORMAT).toString());
        } else {
            setDateFormat(defaultDateFormat);
        }

        if (additionalProperties.containsKey(PROP_STRICT_FIELDS)) {
            setStrictFields(convertPropertyToBoolean(PROP_STRICT_FIELDS));
        } else {
            setStrictFields(true);
        }
        if (additionalProperties.containsKey(PROP_USE_KATIP)) {
            setUseKatip(convertPropertyToBoolean(PROP_USE_KATIP));
        } else {
            setUseKatip(true);
        }

        if (additionalProperties.containsKey(PROP_CABAL_PACKAGE)) {
            setCabalPackage(additionalProperties.get(PROP_CABAL_PACKAGE).toString());
        }
        if (additionalProperties.containsKey(PROP_CABAL_VERSION)) {
            setCabalVersion(additionalProperties.get(PROP_CABAL_VERSION).toString());
        } else {
            setCabalVersion(defaultCabalVersion);
        }
        if (additionalProperties.containsKey(PROP_BASE_MODULE)) {
            setBaseModule(additionalProperties.get(PROP_BASE_MODULE).toString());
        }
        if (additionalProperties.containsKey(PROP_REQUEST_TYPE)) {
            setRequestType(additionalProperties.get(PROP_REQUEST_TYPE).toString());
        }
        if (additionalProperties.containsKey(PROP_CONFIG_TYPE)) {
            setConfigType(additionalProperties.get(PROP_CONFIG_TYPE).toString());
        }
        if (additionalProperties.containsKey(PROP_CUSTOM_TEST_INSTANCE_MODULE)) {
            setCustomTestInstanceModule(additionalProperties.get(PROP_CUSTOM_TEST_INSTANCE_MODULE).toString());
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        String baseTitle = openAPI.getInfo().getTitle();

        if (baseTitle == null) {
            baseTitle = "OpenAPI";
        } else {
            baseTitle = baseTitle.trim();
            // Drop any API suffix
            if (baseTitle.toUpperCase(Locale.ROOT).endsWith("API")) {
                baseTitle = baseTitle.substring(0, baseTitle.length() - 3);
            }
        }

        if (!additionalProperties.containsKey(PROP_CABAL_PACKAGE)) {
            List<String> words = new ArrayList<>();
            for (String word : baseTitle.split(" ")) {
                words.add(word.toLowerCase(Locale.ROOT));
            }
            setCabalPackage(StringUtils.join(words, "-"));
        }

        if (!additionalProperties.containsKey(PROP_BASE_MODULE)) {
            List<String> wordsCaps = new ArrayList<String>();
            for (String word : baseTitle.split(" ")) {
                wordsCaps.add(firstLetterToUpper(word));
            }
            setBaseModule(StringUtils.join(wordsCaps, ""));
        }

        modulePath = sourceFolder + File.separator + getStringProp(PROP_BASE_MODULE).replace('.', File.separatorChar);

        String topLevelPath = StringUtils.substringBeforeLast(modulePath, String.valueOf(File.separatorChar));
        String lastPath = StringUtils.substringAfterLast(modulePath, String.valueOf(File.separatorChar));

        if (!additionalProperties.containsKey(PROP_REQUEST_TYPE)) {
            setRequestType(lastPath + "Request");
        }

        if (!additionalProperties.containsKey(PROP_CONFIG_TYPE)) {
            setConfigType(lastPath + "Config");
        }

        // root
        supportingFiles.add(new SupportingFile("haskell-http-client.cabal.mustache", "", getStringProp(PROP_CABAL_PACKAGE) + ".cabal"));
        supportingFiles.add(new SupportingFile("openapi.mustache", "", "openapi.yaml"));

        // lib
        supportingFiles.add(new SupportingFile("TopLevel.mustache", topLevelPath, lastPath + ".hs"));
        supportingFiles.add(new SupportingFile("Client.mustache", modulePath, "Client.hs"));


        if (!allowNonUniqueOperationIds) {
            supportingFiles.add(new SupportingFile("APIS.mustache", modulePath, "API.hs"));
        }
        supportingFiles.add(new SupportingFile("Core.mustache", modulePath, "Core.hs"));
        supportingFiles.add(new SupportingFile("Model.mustache", modulePath, "Model.hs"));
        supportingFiles.add(new SupportingFile("MimeTypes.mustache", modulePath, "MimeTypes.hs"));

        // logger
        supportingFiles.add(new SupportingFile("Logging.mustache", modulePath, "Logging.hs"));
        supportingFiles.add(new SupportingFile("LoggingMonadLogger.mustache", modulePath, "LoggingMonadLogger.hs"));
        supportingFiles.add(new SupportingFile("LoggingKatip.mustache", modulePath, "LoggingKatip.hs"));

        apiTemplateFiles.put("API.mustache", ".hs");
        // modelTemplateFiles.put("Model.mustache", ".hs");

        // lens
        if ((boolean) additionalProperties.get(PROP_GENERATE_LENSES)) {
            supportingFiles.add(new SupportingFile("ModelLens.mustache", modulePath, "ModelLens.hs"));
        }

        additionalProperties.put("cabalName", getStringProp(PROP_CABAL_PACKAGE));
        additionalProperties.put("pathsName", getStringProp(PROP_CABAL_PACKAGE).replace('-', '_'));
        additionalProperties.put("requestType", getStringProp(PROP_REQUEST_TYPE));
        additionalProperties.put("configType", getStringProp(PROP_CONFIG_TYPE));
        additionalProperties.put("openApiVersion", openAPI.getOpenapi());

        List<String> allUniqueImportPaths = this.importMapping.values().stream().distinct().collect(Collectors.toList());
        if (allUniqueImportPaths.size() > 0) {
            additionalProperties.put(X_ALL_UNIQUE_IMPORT_PATHS, allUniqueImportPaths);
            supportingFiles.add(new SupportingFile("ImportMappings.mustache", modulePath, "ImportMappings.hs"));

            List<Map<String, String>> allImportMappings = new ArrayList<>();
            for (Map.Entry<String, String> entry : this.importMapping.entrySet()) {
                Map<String, String> importMappingEntry = new HashMap<>();
                importMappingEntry.put("dataType", entry.getKey());
                importMappingEntry.put("importPath", entry.getValue());
                allImportMappings.add(importMappingEntry);
            }
            additionalProperties.put(X_ALL_IMPORT_MAPPINGS, allImportMappings);
            additionalProperties.put(X_HAS_IMPORT_MAPPINGS, true);
        }

        super.preprocessOpenAPI(openAPI);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);

        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return "(Map.Map String " + getTypeDeclaration(inner) + ")";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);

        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        } else if ("object".equals(openAPIType)) {
            return "A.Value";
        } else {
            return toModelName(openAPIType);
        }
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            Schema additionalProperties2 = getAdditionalProperties(p);
            String type = additionalProperties2.getType();
            if (null == type) {
                LOGGER.error("No Type defined for Additional Schema " + additionalProperties2 + "\n" //
                        + "\tIn Schema: " + p);
            }
            String inner = getSchemaType(additionalProperties2);
            return "(Map.Map Text " + inner + ")";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            return getSchemaType(ap.getItems());
        } else {
            return null;
        }
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation op, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(tag);
        if (opList == null || opList.isEmpty()) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(tag, opList);
        }
        // check for operationId uniqueness
        String uniqueName = op.operationId;
        String uniqueNameType = toTypeName("Op", uniqueName);
        int counter = 0;

        HashSet<String> opIds = new HashSet<>();
        for (CodegenOperation o : opList) {
            opIds.add(o.operationId);
        }
        while (opIds.contains(uniqueName) ||
                (allowNonUniqueOperationIds
                        ? modelTypeNames.contains(uniqueNameType) // only check for model conflicts
                        : typeNames.contains(uniqueNameType))) {  // check globally across all types
            uniqueName = op.operationId + counter;
            uniqueNameType = toTypeName("Op", uniqueName);
            counter++;
        }
        if (!op.operationId.equals(uniqueName)) {
            LOGGER.warn("generated unique operationId `" + uniqueName + "`");
        }
        op.operationId = uniqueName;
        op.operationIdLowerCase = uniqueName.toLowerCase(Locale.ROOT);
        op.operationIdCamelCase = camelize(uniqueName);
        op.operationIdSnakeCase = underscore(uniqueName);
        opList.add(op);
        op.baseName = tag;

        // prevent aliasing/sharing of operation.vendorExtensions reference
        op.vendorExtensions = new LinkedHashMap();

        String operationType = toTypeName("Op", op.operationId);
        op.vendorExtensions.put(VENDOR_EXTENSION_X_OPERATION_TYPE, operationType);
        typeNames.add(operationType);

        String xHaddockPath = String.format(Locale.ROOT, "%s %s", op.httpMethod, op.path.replace("/", "\\/"));
        op.vendorExtensions.put(VENDOR_EXTENSION_X_HADDOCK_PATH, xHaddockPath);
        op.vendorExtensions.put(VENDOR_EXTENSION_X_HAS_BODY_OR_FORM_PARAM, op.getHasBodyParam() || op.getHasFormParams());

        for (CodegenParameter param : op.allParams) {
            param.vendorExtensions = new LinkedHashMap(); // prevent aliasing/sharing
            param.vendorExtensions.put(VENDOR_EXTENSION_X_OPERATION_TYPE, operationType);
            param.vendorExtensions.put(VENDOR_EXTENSION_X_IS_BODY_OR_FORM_PARAM, param.isBodyParam || param.isFormParam);
            if (!StringUtils.isBlank(param.collectionFormat)) {
                param.vendorExtensions.put(VENDOR_EXTENSION_X_COLLECTION_FORMAT, mapCollectionFormat(param.collectionFormat));
            } else if (!param.isBodyParam && (param.isListContainer || param.dataType.startsWith("["))) { // param.isListContainer is sometimes false for list types
                // defaulting due to https://github.com/wing328/openapi-generator/issues/72
                param.collectionFormat = "csv";
                param.vendorExtensions.put(VENDOR_EXTENSION_X_COLLECTION_FORMAT, mapCollectionFormat(param.collectionFormat));
            }
            if (!param.required) {
                op.vendorExtensions.put(VENDOR_EXTENSION_X_HAS_OPTIONAL_PARAMS, true);
            }

            if (typeMapping.containsKey(param.dataType)
                    || param.isMapContainer || param.isListContainer
                    || param.isPrimitiveType || param.isFile || (param.isEnum || param.allowableValues != null) || !param.isBodyParam) {

                String dataType = genEnums && param.isEnum ? param.datatypeWithEnum : param.dataType;

                String paramNameType = toDedupedModelName(toTypeName("Param", param.paramName), dataType, !(param.isEnum || param.allowableValues != null));
                param.vendorExtensions.put(VENDOR_EXTENSION_X_PARAM_NAME_TYPE, paramNameType);

                HashMap<String, Object> props = new HashMap<>();
                props.put(X_IS_BODY_PARAM, param.isBodyParam);
                addToUniques(X_NEWTYPE, paramNameType, dataType, props);
            }
        }

        processPathExpr(op);

        processProducesConsumes(op);

        processReturnType(op);

    }

    @Override
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> schemes) {
        List<CodegenSecurity> secs = super.fromSecurity(schemes);
        for (CodegenSecurity sec : secs) {
            String prefix = "";
            if (sec.isBasic) prefix = "AuthBasic";
            if (sec.isApiKey) prefix = "AuthApiKey";
            if (sec.isOAuth) prefix = "AuthOAuth";
            sec.name = prefix + toTypeName("", sec.name);
        }
        return secs;
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        updateGlobalAdditionalProps();
        return super.postProcessAllModels(objs);
    }

    public void updateGlobalAdditionalProps() {
        additionalProperties.put(X_HAS_UNKNOWN_MIME_TYPES, !unknownMimeTypes.isEmpty());

        Collections.sort(unknownMimeTypes, new Comparator<Map<String, String>>() {
            @Override
            public int compare(Map<String, String> o1, Map<String, String> o2) {
                return o1.get(MEDIA_TYPE).compareTo(o2.get(MEDIA_TYPE));
            }
        });
        additionalProperties.put(VENDOR_EXTENSION_X_UNKNOWN_MIME_TYPES, unknownMimeTypes);

        ArrayList<Map<String, Object>> params = new ArrayList<>(uniqueParamNameTypes.values());
        Collections.sort(params, new Comparator<Map<String, Object>>() {
            @Override
            public int compare(Map<String, Object> o1, Map<String, Object> o2) {
                return
                        ((String) o1.get(VENDOR_EXTENSION_X_PARAM_NAME_TYPE))
                                .compareTo(
                                        (String) o2.get(VENDOR_EXTENSION_X_PARAM_NAME_TYPE));
            }
        });
        additionalProperties.put(X_ALL_UNIQUE_PARAMS, params);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> ret = super.postProcessOperationsWithModels(objs, allModels);
        HashMap<String, Object> pathOps = (HashMap<String, Object>) ret.get("operations");
        ArrayList<CodegenOperation> ops = (ArrayList<CodegenOperation>) pathOps.get("operation");
        if (ops.size() > 0) {
            ops.get(0).vendorExtensions.put(VENDOR_EXTENSION_X_HAS_NEW_TAG, true);
        }

        updateGlobalAdditionalProps();

        for (Object o : allModels) {
            HashMap<String, Object> h = (HashMap<String, Object>) o;
            CodegenModel m = (CodegenModel) h.get("model");
            if (modelMimeTypes.containsKey(m.classname)) {
                Set<String> mimeTypes = modelMimeTypes.get(m.classname);

                m.vendorExtensions.put(VENDOR_EXTENSION_X_MIME_TYPES, mimeTypes);

                if ((boolean) additionalProperties.get(PROP_GENERATE_FORM_URLENCODED_INSTANCES) && mimeTypes.contains("MimeFormUrlEncoded")) {
                    Boolean hasMimeFormUrlEncoded = true;
                    for (CodegenProperty v : m.vars) {
                        if (!(v.isPrimitiveType || v.isString || v.isDate || v.isDateTime)) {
                            hasMimeFormUrlEncoded = false;
                        }
                    }
                    if (hasMimeFormUrlEncoded) {
                        m.vendorExtensions.put(VENDOR_EXTENSION_X_HAS_MIME_FORM_URL_ENCODED, true);
                    }
                }
            }
        }
        return ret;
    }

    @Override
    public CodegenModel fromModel(String name, Schema mod) {
        CodegenModel model = super.fromModel(name, mod);

//        while (typeNames.contains(model.classname)) {
//            model.classname = generateNextName(model.classname);
//        }
        typeNames.add(model.classname);
        modelTypeNames.add(model.classname);

        // From the model name, compute the prefix for the fields.
        String prefix = StringUtils.uncapitalize(model.classname);
        for (CodegenProperty prop : model.vars) {
            prop.name = toVarName(prefix, prop.name);
        }

        return model;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
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

    private void processReturnType(CodegenOperation op) {
        String returnType = op.returnType;
        if (returnType == null || returnType.equals("null")) {
            if (op.hasProduces) {
                returnType = "res";
                op.vendorExtensions.put(VENDOR_EXTENSION_X_HAS_UNKNOWN_RETURN, true);
            } else {
                returnType = "NoContent";
                if (!op.vendorExtensions.containsKey(VENDOR_EXTENSION_X_INLINE_ACCEPT)) {
                    SetNoContent(op, VENDOR_EXTENSION_X_INLINE_ACCEPT);
                }
            }
        }
        if (returnType.contains(" ")) {
            returnType = "(" + returnType + ")";
        }
        op.vendorExtensions.put(VENDOR_EXTENSION_X_RETURN_TYPE, returnType);
    }

    private void processProducesConsumes(CodegenOperation op) {
        if (!(Boolean) op.vendorExtensions.get(VENDOR_EXTENSION_X_HAS_BODY_OR_FORM_PARAM)) {
            SetNoContent(op, VENDOR_EXTENSION_X_INLINE_CONTENT_TYPE);
        }
        if (op.hasConsumes) {
            // deduplicate
            Map<String, Map<String, String>> consumes = new HashMap<>();
            for (Map<String, String> m : op.consumes) {
                consumes.put(m.get(MEDIA_TYPE), m);
            }
            op.consumes = new ArrayList<>(consumes.values());

            // add metadata
            for (Map<String, String> m : op.consumes) {
                processMediaType(op, m);
                processInlineConsumesContentType(op, m);

            }
            if (isMultipartOperation(op.consumes)) {
                op.isMultipart = Boolean.TRUE;
            }
        }
        if (op.hasProduces) {
            // deduplicate
            Map<String, Map<String, String>> produces = new HashMap<>();
            for (Map<String, String> m : op.produces) {
                produces.put(m.get(MEDIA_TYPE), m);
            }
            op.produces = new ArrayList<>(produces.values());

            // add metadata
            for (Map<String, String> m : op.produces) {
                processMediaType(op, m);
                processInlineProducesContentType(op, m);
            }
        }
    }

    private void processInlineConsumesContentType(CodegenOperation op, Map<String, String> m) {
        if (op.vendorExtensions.containsKey(VENDOR_EXTENSION_X_INLINE_CONTENT_TYPE)) return;

        if ((boolean) additionalProperties.get(PROP_INLINE_MIME_TYPES)
                && op.consumes.size() == 1
                && !MIME_ANY.equals(op.consumes.get(0).get(X_MEDIA_DATA_TYPE))
                && !MIME_NO_CONTENT.equals(op.consumes.get(0).get(X_MEDIA_DATA_TYPE))) {
            op.vendorExtensions.put(VENDOR_EXTENSION_X_INLINE_CONTENT_TYPE, m);
            for (CodegenParameter param : op.allParams) {
                if (param.isBodyParam && param.required) {
                    param.vendorExtensions.put(VENDOR_EXTENSION_X_INLINE_CONTENT_TYPE, m);
                }
            }
        }
    }

    private void processInlineProducesContentType(CodegenOperation op, Map<String, String> m) {
        if ((boolean) additionalProperties.get(PROP_INLINE_MIME_TYPES)
                && op.produces.size() == 1
                && !MIME_ANY.equals(op.produces.get(0).get(X_MEDIA_DATA_TYPE))
                && !MIME_NO_CONTENT.equals(op.produces.get(0).get(X_MEDIA_DATA_TYPE))) {

            op.vendorExtensions.put(VENDOR_EXTENSION_X_INLINE_ACCEPT, m);
        }
    }

    private void SetNoContent(CodegenOperation op, String inlineExtentionName) {
        Map<String, String> m = new HashMap<>();
        m.put(X_MEDIA_DATA_TYPE, MIME_NO_CONTENT);
        op.vendorExtensions.put(inlineExtentionName, m);
    }

    private String toDedupedModelName(String paramNameType, String dataType, Boolean appendDataType) {
        if (appendDataType
                && uniqueParamNameTypes.containsKey(paramNameType)
                && !isDuplicate(paramNameType, dataType)) {
            paramNameType = toTypeName("", paramNameType + dataType);
        }

        while (typeNames.contains(paramNameType)) {
            if (isDuplicate(paramNameType, dataType)) {
                break;
            }
            paramNameType = generateNextName(paramNameType);
        }

        typeNames.add(paramNameType);
        modelTypeNames.add(paramNameType);
        return paramNameType;
    }

    public Boolean isDuplicate(String paramNameType, String dataType) {
        Map<String, Object> lastParam = this.uniqueParamNameTypes.get(paramNameType);
        if (lastParam != null) {
            String comparisonKey = lastParam.containsKey(VENDOR_EXTENSION_X_ENUM) ? X_ENUM_VALUES : VENDOR_EXTENSION_X_DATA_TYPE;
            String lastParamDataType = (String) lastParam.get(comparisonKey);
            if (lastParamDataType != null && lastParamDataType.equals(dataType)) {
                return true;
            }
        }
        return false;
    }

    private Pair<Boolean, String> isDuplicateEnumValues(String enumValues) {
        for (Map<String, Object> vs : uniqueParamNameTypes.values()) {
            if (enumValues.equals(vs.get(X_ENUM_VALUES))) {
                return Pair.of(true, (String) vs.get(VENDOR_EXTENSION_X_PARAM_NAME_TYPE));
            }
        }
        return Pair.of(false, null);
    }


    private void addToUniques(String xGroup, String paramNameType, String dataType, Map<String, Object> props) {
        HashMap<String, Object> m = new HashMap<>();
        m.put(VENDOR_EXTENSION_X_PARAM_NAME_TYPE, paramNameType);
        m.put(VENDOR_EXTENSION_X_DATA_TYPE, dataType);
        m.put(xGroup, true);
        m.putAll(props);
        uniqueParamNameTypes.put(paramNameType, m);
    }

    private void addEnumToUniques(String paramNameType, String datatype, String enumValues, Map<String, Object> allowableValues, String description) {
        HashMap<String, Object> props = new HashMap<>();
        props.put("allowableValues", allowableValues);
        if (StringUtils.isNotBlank(description)) {
            props.put("description", description);
        }
        props.put(X_ENUM_VALUES, enumValues);
        addToUniques(VENDOR_EXTENSION_X_ENUM, paramNameType, datatype, props);
        additionalProperties.put(X_HAS_ENUM_SECTION, true);
    }


    // build the parameterized path segments, according to pathParams
    private void processPathExpr(CodegenOperation op) {
        String xPath = "[\"" + escapeText(op.path) + "\"]";
        if (op.getHasPathParams()) {
            for (CodegenParameter param : op.allParams) {
                if (param.isPathParam) {
                    xPath = xPath.replaceAll("\\{" + param.baseName + "\\}", "\",toPath " + param.paramName + ",\"");
                }
            }
            xPath = xPath.replaceAll(",\"\",", ",");
            xPath = xPath.replaceAll("\"\",", ",");
            xPath = xPath.replaceAll(",\"\"", ",");
            xPath = xPath.replaceAll("^\\[,", "[");
            xPath = xPath.replaceAll(",\\]$", "]");
        }
        op.vendorExtensions.put(VENDOR_EXTENSION_X_PATH, xPath);
    }


    private void processMediaType(CodegenOperation op, Map<String, String> m) {
        String mediaType = m.get(MEDIA_TYPE);

        if (StringUtils.isBlank(mediaType)) return;

        String mimeType = getMimeDataType(mediaType);
        typeNames.add(mimeType);
        m.put(X_MEDIA_DATA_TYPE, mimeType);
        if (isJsonMimeType(mediaType)) {
            m.put(X_MEDIA_IS_JSON, "true");
        }
        if (isWildcardMimeType(mediaType)) {
            m.put(X_MEDIA_IS_WILDCARD, "true");
        }
        if (!knownMimeDataTypes.containsValue(mimeType) && !unknownMimeTypesContainsType(mimeType)) {
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

    private Boolean unknownMimeTypesContainsType(String mimeType) {
        for (Map<String, String> m : unknownMimeTypes) {
            String mimeType0 = m.get(X_MEDIA_DATA_TYPE);
            if (mimeType0 != null && mimeType0.equals(mimeType)) {
                return true;
            }
        }

        return false;
    }

    public String firstLetterToUpper(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toUpperCase(Locale.ROOT);
        } else {
            return word.substring(0, 1).toUpperCase(Locale.ROOT) + word.substring(1);
        }
    }

    public String firstLetterToLower(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toLowerCase(Locale.ROOT);
        } else {
            return word.substring(0, 1).toLowerCase(Locale.ROOT) + word.substring(1);
        }
    }

    private String mapCollectionFormat(String collectionFormat) {
        switch (collectionFormat) {
            case "csv":
                return "CommaSeparated";
            case "tsv":
                return "TabSeparated";
            case "ssv":
            case "space":
                return "SpaceSeparated";
            case "pipes":
                return "PipeSeparated";
            case "multi":
                return "MultiParamArray";
            default:
                throw new UnsupportedOperationException(collectionFormat + " (collection format) not supported");
        }
    }

    private String getMimeDataType(String mimeType) {
        if (StringUtils.isBlank(mimeType)) {
            return MIME_NO_CONTENT;
        }
        if (knownMimeDataTypes.containsKey(mimeType)) {
            return knownMimeDataTypes.get(mimeType);
        }
        String shortenedName = mimeType.replaceFirst("application/", "");
        return "Mime" + toTypeName("", shortenedName);
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

    private static boolean isMultipartOperation(List<Map<String, String>> consumes) {
        for (Map<String, String> consume : consumes) {
            if (consume != null) {
                if ("multipart/form-data".equals(consume.get(MEDIA_TYPE))) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public String toVarName(String name) {
        return toVarName("", name);
    }

    public String toVarName(String prefix, String name) {
        Boolean hasPrefix = !StringUtils.isBlank(prefix);
        name = underscore(sanitizeName(name.replaceAll("-", "_")));
        name = camelize(name, !hasPrefix);
        if (hasPrefix) {
            return prefix + name;
        } else {
            if (name.matches("^\\d.*"))
                name = escapeReservedWord(name);
            if (isReservedWord(name))
                name = escapeReservedWord(name);
            return name;
        }
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        return toTypeName("Model", name);
    }

    @Override
    public String toModelFilename(String name) {
        return toTypeName("Model", name);
    }

    public String toApiName(String name) {
        if (name.length() == 0) {
            return "Default";
        }
        return toTypeName("Api", name);
    }

    @Override
    public String toApiFilename(String name) {
        return toTypeName("Api", name);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + this.modulePath + File.separator + "API";
    }

    public String toTypeName(String prefix, String name) {
        name = escapeIdentifier(prefix, camelize(sanitizeName(name)));
        return name;
    }

    @Override
    public String toOperationId(String operationId) {
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");
        }
        operationId = escapeIdentifier("op", camelize(sanitizeName(operationId), true));
        return operationId;
    }

    public String escapeIdentifier(String prefix, String name) {
        if (StringUtils.isBlank(prefix)) return name;

        if (isReservedWord(name)) {
            name = prefix + name;
        }
        if (name.matches("^\\d.*")) {
            name = prefix + name; // e.g. 200Response => Model200Response (after camelize)
        }
        if (languageSpecificPrimitives.contains(name)) {
            name = prefix + name;
        }
        if (typeMapping.containsValue(name)) {
            name = prefix + name;
        }
        return name;
    }

    static boolean isJsonMimeType(String mime) {
        return mime != null && JSON_MIME_PATTERN.matcher(mime).matches();
    }

    static boolean isWildcardMimeType(String mime) {
        return mime != null && mime.equals("*/*");
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + escapeText((String) p.getDefault()) + "\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                if (p.getDefault().toString().equalsIgnoreCase("false"))
                    return "False";
                else
                    return "True";
            }
        }

        return null;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");

        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.isEnum = genEnums && cm.isEnum;
            if (cm.isAlias) {
                String dataType = cm.dataType;
                if (dataType == null && cm.isArrayModel) { // isAlias + arrayModelType missing "datatype"
                    dataType = "[" + cm.arrayModelType + "]";
                }
                cm.vendorExtensions.put(VENDOR_EXTENSION_X_DATA_TYPE, dataType);
                if (dataType.equals("Maybe A.Value")) {
                    cm.vendorExtensions.put(VENDOR_EXTENSION_X_IS_MAYBE_VALUE, true);
                }
            }
            for (CodegenProperty var : cm.vars) {
                String datatype = genEnums && !StringUtils.isBlank(var.datatypeWithEnum)
                        ? var.datatypeWithEnum
                        : var.dataType;
                var.vendorExtensions.put(VENDOR_EXTENSION_X_DATA_TYPE, datatype);
                if (!var.required && datatype.equals("A.Value") || var.required && datatype.equals("Maybe A.Value")) {
                    var.vendorExtensions.put(VENDOR_EXTENSION_X_IS_MAYBE_VALUE, true);
                }
            }
        }
        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        Map<String, Object> objsEnum = super.postProcessModelsEnum(objs);
        if (genEnums) {
            List<Object> models = (List<Object>) objsEnum.get("models");
            for (Object _mo : models) {
                Map<String, Object> mo = (Map<String, Object>) _mo;
                CodegenModel cm = (CodegenModel) mo.get("model");
                if (cm.isEnum && cm.allowableValues != null) {
                    updateAllowableValuesNames(cm.classname, cm.allowableValues);
                    addEnumToUniques(cm.classname, cm.dataType, cm.allowableValues.values().toString(), cm.allowableValues, cm.description);
                }
            }
        }
        return objsEnum;
    }

    @Override
    protected void updateDataTypeWithEnumForMap(CodegenProperty property) {
        CodegenProperty baseItem = property.items;
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMapContainer) || Boolean.TRUE.equals(baseItem.isListContainer))) {
            baseItem = baseItem.items;
        }
        if (baseItem != null) {

            // this replacement is/needs to be language-specific
            property.datatypeWithEnum = property.datatypeWithEnum.replace(baseItem.baseType + ")", toEnumName(baseItem) + ")");

            property.enumName = toEnumName(property);
            if (property.defaultValue != null) {
                property.defaultValue = property.defaultValue.replace(", " + property.items.baseType, ", " + toEnumName(property.items));
            }
        }
    }

    @Override
    public String toEnumName(CodegenProperty var) {
        if (!genEnums) return super.toEnumName(var);

        if (var.items != null && var.items.isEnum) {
            return toEnumName(var.items);
        }
        String paramNameType = "E'" + toTypeName("", var.name);
        String enumValues = var._enum.toString();

        Pair<Boolean, String> duplicateEnum = isDuplicateEnumValues(enumValues);
        if (duplicateEnum.getLeft()) {
            paramNameType = duplicateEnum.getRight();
        } else {
            paramNameType = toDedupedModelName(paramNameType, enumValues, false);
            var.datatypeWithEnum = paramNameType;
            updateCodegenPropertyEnum(var);
            addEnumToUniques(paramNameType, var.dataType, enumValues, var.allowableValues, var.description);
        }

        return paramNameType;
    }

    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        super.updateCodegenPropertyEnum(var);
        if (!genEnums) return;
        updateCodegenPropertyEnumValues(var, var.datatypeWithEnum);
    }

    public void updateCodegenPropertyEnumValues(CodegenProperty var, String paramNameType) {
        if (var.items != null && var.items.allowableValues != null) {
            updateCodegenPropertyEnumValues(var.items, var.items.datatypeWithEnum);
            return;
        }
        if (var.isEnum && var.allowableValues != null) {
            updateAllowableValuesNames(paramNameType, var.allowableValues);
        }
    }

    private void updateAllowableValuesNames(String paramNameType, Map<String, Object> allowableValues) {
        if (allowableValues == null) {
            return;
        }
        for (Map<String, String> enumVar : (List<Map<String, String>>) allowableValues.get("enumVars")) {
            enumVar.put("name", paramNameType + enumVar.get("name"));
        }
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (!genEnums) return super.toEnumVarName(value, datatype);

        List<String> num = new ArrayList<>(Arrays.asList("integer", "int", "double", "long", "float"));
        if (value.length() == 0) {
            return "'Empty";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(value) != null) {
            return "'" + StringUtils.capitalize(sanitizeName(getSymbolName(value)));
        }

        // number
        if (num.contains(datatype.toLowerCase(Locale.ROOT))) {
            String varName = "Num" + value;
            varName = varName.replaceAll("-", "Minus_");
            varName = varName.replaceAll("\\+", "Plus_");
            varName = varName.replaceAll("\\.", "_Dot_");
            return "'" + StringUtils.capitalize(sanitizeName(varName));
        }

        return "'" + StringUtils.capitalize(sanitizeName(value));
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        List<String> num = new ArrayList<>(Arrays.asList("integer", "int", "double", "long", "float"));
        if (num.contains(datatype.toLowerCase(Locale.ROOT))) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    // override with any special text escaping logic
    @SuppressWarnings("static-method")
    public String escapeText(String input) {
        if (input == null) {
            return input;
        }

        // remove \t, \n, \r
        // replace \ with \\
        // replace " with \"
        // outter unescape to retain the original multi-byte characters
        // finally escalate characters avoiding code injection
        return escapeUnsafeCharacters(
                StringEscapeUtils.unescapeJava(
                        StringEscapeUtils.escapeJava(input)
                                .replace("\\/", "/"))
                        .replaceAll("[\\t\\n\\r]", " ")
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\""));
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }
        String haskellPostProcessFile = System.getenv("HASKELL_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(haskellPostProcessFile)) {
            return; // skip if HASKELL_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with hs extension
        if ("hs".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = haskellPostProcessFile + " " + file.toString();
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: " + command);
                }
            } catch (Exception e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
            }
        }
    }
}
