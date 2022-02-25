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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.time.OffsetDateTime;
import java.time.Instant;
import java.time.temporal.ChronoField;
import java.util.concurrent.TimeUnit;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class Swift5ClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(Swift5ClientCodegen.class);

    public static final String PROJECT_NAME = "projectName";
    public static final String RESPONSE_AS = "responseAs";
    public static final String OBJC_COMPATIBLE = "objcCompatible";
    public static final String POD_SOURCE = "podSource";
    public static final String POD_AUTHORS = "podAuthors";
    public static final String POD_SOCIAL_MEDIA_URL = "podSocialMediaURL";
    public static final String POD_LICENSE = "podLicense";
    public static final String POD_HOMEPAGE = "podHomepage";
    public static final String POD_SUMMARY = "podSummary";
    public static final String POD_DESCRIPTION = "podDescription";
    public static final String POD_SCREENSHOTS = "podScreenshots";
    public static final String POD_DOCUMENTATION_URL = "podDocumentationURL";
    public static final String READONLY_PROPERTIES = "readonlyProperties";
    public static final String REMOVE_MIGRATION_PROJECT_NAME_CLASS = "removeMigrationProjectNameClass";
    public static final String SWIFT_USE_API_NAMESPACE = "swiftUseApiNamespace";
    public static final String DEFAULT_POD_AUTHORS = "OpenAPI Generator";
    public static final String LENIENT_TYPE_CAST = "lenientTypeCast";
    public static final String USE_SPM_FILE_STRUCTURE = "useSPMFileStructure";
    public static final String SWIFT_PACKAGE_PATH = "swiftPackagePath";
    public static final String USE_CLASSES = "useClasses";
    public static final String USE_BACKTICK_ESCAPES = "useBacktickEscapes";
    public static final String GENERATE_MODEL_ADDITIONAL_PROPERTIES = "generateModelAdditionalProperties";
    public static final String HASHABLE_MODELS = "hashableModels";
    public static final String MAP_FILE_BINARY_TO_DATA = "mapFileBinaryToData";
    protected static final String LIBRARY_ALAMOFIRE = "alamofire";
    protected static final String LIBRARY_URLSESSION = "urlsession";
    protected static final String LIBRARY_VAPOR = "vapor";
    protected static final String RESPONSE_LIBRARY_PROMISE_KIT = "PromiseKit";
    protected static final String RESPONSE_LIBRARY_RX_SWIFT = "RxSwift";
    protected static final String RESPONSE_LIBRARY_RESULT = "Result";
    protected static final String RESPONSE_LIBRARY_COMBINE = "Combine";
    protected static final String RESPONSE_LIBRARY_ASYNC_AWAIT = "AsyncAwait";
    protected static final String[] RESPONSE_LIBRARIES = {RESPONSE_LIBRARY_PROMISE_KIT, RESPONSE_LIBRARY_RX_SWIFT, RESPONSE_LIBRARY_RESULT, RESPONSE_LIBRARY_COMBINE, RESPONSE_LIBRARY_ASYNC_AWAIT};
    protected String projectName = "OpenAPIClient";
    protected boolean nonPublicApi = false;
    protected boolean objcCompatible = false;
    protected boolean lenientTypeCast = false;
    protected boolean readonlyProperties = false;
    protected boolean removeMigrationProjectNameClass = false;
    protected boolean swiftUseApiNamespace = false;
    protected boolean useSPMFileStructure = false;
    protected String swiftPackagePath = "Classes" + File.separator + "OpenAPIs";
    protected boolean useClasses = false;
    protected boolean useBacktickEscapes = false;
    protected boolean generateModelAdditionalProperties = true;
    protected boolean hashableModels = true;
    protected boolean mapFileBinaryToData = false;
    protected String[] responseAs = new String[0];
    protected String sourceFolder = swiftPackagePath;
    protected HashSet objcReservedWords;
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    /**
     * Constructor for the swift5 language codegen module.
     */
    public Swift5ClientCodegen() {
        super();
        this.useOneOfInterfaces = true;

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

        outputFolder = "generated-code" + File.separator + "swift";
        modelTemplateFiles.put("model.mustache", ".swift");
        apiTemplateFiles.put("api.mustache", ".swift");
        embeddedTemplateDir = templateDir = "swift5";
        apiPackage = File.separator + "APIs";
        modelPackage = File.separator + "Models";
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "Int",
                        "Int32",
                        "Int64",
                        "Float",
                        "Double",
                        "Bool",
                        "Void",
                        "String",
                        "Data",
                        "Date",
                        "Character",
                        "UUID",
                        "URL",
                        "AnyObject",
                        "Any",
                        "Decimal")
        );
        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "Data",
                        "Date",
                        "URL", // for file
                        "UUID",
                        "Array",
                        "Dictionary",
                        "Set",
                        "Any",
                        "Empty",
                        "AnyObject",
                        "Any",
                        "Decimal")
        );

        objcReservedWords = new HashSet<>(
                Arrays.asList(
                        // Added for Objective-C compatibility
                        "id", "description", "NSArray", "NSURL", "CGFloat", "NSSet", "NSString", "NSInteger", "NSUInteger",
                        "NSError", "NSDictionary",
                        // 'Property 'hash' with type 'String' cannot override a property with type 'Int' (when objcCompatible=true)
                        "hash",
                        // Cannot override with a stored property 'className'
                        "className"
                )
        );

        reservedWords = new HashSet<>(
                Arrays.asList(
                        // name used by swift client
                        "ErrorResponse", "Response",

                        // Swift keywords. This list is taken from here:
                        // https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410
                        //
                        // Keywords used in declarations
                        "associatedtype", "class", "deinit", "enum", "extension", "fileprivate", "func", "import", "init",
                        "inout", "internal", "let", "open", "operator", "private", "protocol", "public", "static", "struct",
                        "subscript", "typealias", "var",
                        // Keywords uses in statements
                        "break", "case", "continue", "default", "defer", "do", "else", "fallthrough", "for", "guard", "if",
                        "in", "repeat", "return", "switch", "where", "while",
                        // Keywords used in expressions and types
                        "as", "Any", "catch", "false", "is", "nil", "rethrows", "super", "self", "Self", "throw", "throws", "true", "try",
                        // Keywords used in patterns
                        "_",
                        // Keywords that begin with a number sign
                        "#available", "#colorLiteral", "#column", "#else", "#elseif", "#endif", "#file", "#fileLiteral", "#function", "#if",
                        "#imageLiteral", "#line", "#selector", "#sourceLocation",
                        // Keywords reserved in particular contexts
                        "associativity", "convenience", "dynamic", "didSet", "final", "get", "infix", "indirect", "lazy", "left",
                        "mutating", "none", "nonmutating", "optional", "override", "postfix", "precedence", "prefix", "Protocol",
                        "required", "right", "set", "Type", "unowned", "weak", "willSet",

                        //
                        // Swift Standard Library types
                        // https://developer.apple.com/documentation/swift
                        //
                        // Numbers and Basic Values
                        "Bool", "Int", "Double", "Float", "Range", "ClosedRange", "Error", "Optional",
                        // Special-Use Numeric Types
                        "UInt", "UInt8", "UInt16", "UInt32", "UInt64", "Int8", "Int16", "Int32", "Int64", "Float80", "Float32", "Float64",
                        // Strings and Text
                        "String", "Character", "Unicode", "StaticString",
                        // Collections
                        "Array", "Dictionary", "Set", "OptionSet", "CountableRange", "CountableClosedRange",

                        // The following are commonly-used Foundation types
                        "URL", "Data", "Codable", "Encodable", "Decodable",

                        // The following are other words we want to reserve
                        "Void", "AnyObject", "Class", "dynamicType", "COLUMN", "FILE", "FUNCTION", "LINE"
                )
        );

        typeMapping = new HashMap<>();
        typeMapping.put("array", "Array");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("set", "Set");
        typeMapping.put("date", "Date");
        typeMapping.put("Date", "Date");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "String");
        typeMapping.put("char", "Character");
        typeMapping.put("short", "Int");
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Int64");
        typeMapping.put("integer", "Int");
        typeMapping.put("Integer", "Int");
        typeMapping.put("float", "Float");
        typeMapping.put("number", "Double");
        typeMapping.put("double", "Double");
        typeMapping.put("file", "URL");
        typeMapping.put("binary", "URL");
        typeMapping.put("ByteArray", "Data");
        typeMapping.put("UUID", "UUID");
        typeMapping.put("URI", "String");
        typeMapping.put("decimal", "Decimal");
        typeMapping.put("object", "AnyCodable");
        typeMapping.put("AnyType", "AnyCodable");

        importMapping = new HashMap<>();

        cliOptions.add(new CliOption(PROJECT_NAME, "Project name in Xcode"));
        cliOptions.add(new CliOption(RESPONSE_AS,
                "Optionally use libraries to manage response.  Currently "
                        + StringUtils.join(RESPONSE_LIBRARIES, ", ")
                        + " are available."));
        cliOptions.add(new CliOption(CodegenConstants.NON_PUBLIC_API,
                CodegenConstants.NON_PUBLIC_API_DESC
                        + "(default: false)"));
        cliOptions.add(new CliOption(OBJC_COMPATIBLE,
                "Add additional properties and methods for Objective-C "
                        + "compatibility (default: false)"));
        cliOptions.add(new CliOption(POD_SOURCE, "Source information used for Podspec"));
        cliOptions.add(new CliOption(CodegenConstants.POD_VERSION, "Version used for Podspec"));
        cliOptions.add(new CliOption(POD_AUTHORS, "Authors used for Podspec"));
        cliOptions.add(new CliOption(POD_SOCIAL_MEDIA_URL, "Social Media URL used for Podspec"));
        cliOptions.add(new CliOption(POD_LICENSE, "License used for Podspec"));
        cliOptions.add(new CliOption(POD_HOMEPAGE, "Homepage used for Podspec"));
        cliOptions.add(new CliOption(POD_SUMMARY, "Summary used for Podspec"));
        cliOptions.add(new CliOption(POD_DESCRIPTION, "Description used for Podspec"));
        cliOptions.add(new CliOption(POD_SCREENSHOTS, "Screenshots used for Podspec"));
        cliOptions.add(new CliOption(POD_DOCUMENTATION_URL,
                "Documentation URL used for Podspec"));
        cliOptions.add(new CliOption(READONLY_PROPERTIES, "Make properties "
                + "readonly (default: false)"));
        cliOptions.add(new CliOption(REMOVE_MIGRATION_PROJECT_NAME_CLASS, "Make properties "
                + "removeMigrationProjectNameClass (default: false)"));
        cliOptions.add(new CliOption(SWIFT_USE_API_NAMESPACE,
                "Flag to make all the API classes inner-class "
                        + "of {{projectName}}API"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(LENIENT_TYPE_CAST,
                "Accept and cast values for simple types (string->bool, "
                        + "string->int, int->string)")
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(USE_BACKTICK_ESCAPES,
                "Escape reserved words using backticks (default: false)")
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(GENERATE_MODEL_ADDITIONAL_PROPERTIES,
                "Generate model additional properties (default: true)")
                .defaultValue(Boolean.TRUE.toString()));

        cliOptions.add(new CliOption(CodegenConstants.API_NAME_PREFIX, CodegenConstants.API_NAME_PREFIX_DESC));
        cliOptions.add(new CliOption(USE_SPM_FILE_STRUCTURE, "Use SPM file structure"
                + " and set the source path to Sources" + File.separator + "{{projectName}} (default: false)."));
        cliOptions.add(new CliOption(SWIFT_PACKAGE_PATH, "Set a custom source path instead of "
                + projectName + File.separator + "Classes" + File.separator + "OpenAPIs" + "."));
        cliOptions.add(new CliOption(USE_CLASSES, "Use final classes for models instead of structs (default: false)")
                .defaultValue(Boolean.FALSE.toString()));

        cliOptions.add(new CliOption(HASHABLE_MODELS,
            "Make hashable models (default: true)")
            .defaultValue(Boolean.TRUE.toString()));

        cliOptions.add(new CliOption(MAP_FILE_BINARY_TO_DATA,
            "[WARNING] This option will be removed and enabled by default in the future once we've enhanced the code to work with `Data` in all the different situations. Map File and Binary to Data (default: false)")
            .defaultValue(Boolean.FALSE.toString()));

        supportedLibraries.put(LIBRARY_URLSESSION, "[DEFAULT] HTTP client: URLSession");
        supportedLibraries.put(LIBRARY_ALAMOFIRE, "HTTP client: Alamofire");
        supportedLibraries.put(LIBRARY_VAPOR, "HTTP client: Vapor");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "Library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        libraryOption.setDefault(LIBRARY_URLSESSION);
        cliOptions.add(libraryOption);
        setLibrary(LIBRARY_URLSESSION);
    }

    private static CodegenModel reconcileProperties(CodegenModel codegenModel,
                                                    CodegenModel parentCodegenModel) {
        // To support inheritance in this generator, we will analyze
        // the parent and child models, look for properties that match, and remove
        // them from the child models and leave them in the parent.
        // Because the child models extend the parents, the properties
        // will be available via the parent.

        // Get the properties for the parent and child models
        final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
        List<CodegenProperty> codegenProperties = codegenModel.vars;
        codegenModel.allVars = new ArrayList<CodegenProperty>(codegenProperties);
        codegenModel.parentVars = parentCodegenModel.allVars;

        // Iterate over all of the parent model properties
        boolean removedChildProperty = false;

        for (CodegenProperty parentModelCodegenProperty : parentModelCodegenProperties) {
            // Now that we have found a prop in the parent class,
            // and search the child class for the same prop.
            Iterator<CodegenProperty> iterator = codegenProperties.iterator();
            while (iterator.hasNext()) {
                CodegenProperty codegenProperty = iterator.next();
                if (codegenProperty.baseName.equals(parentModelCodegenProperty.baseName)) {
                    // We found a property in the child class that is
                    // a duplicate of the one in the parent, so remove it.
                    iterator.remove();
                    removedChildProperty = true;
                }
            }
        }

        if (removedChildProperty) {
            codegenModel.vars = codegenProperties;
        }


        return codegenModel;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "swift5";
    }

    @Override
    public String getHelp() {
        return "Generates a Swift 5.x client library.";
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel,
                                                       Schema schema) {

        final Schema additionalProperties = getAdditionalProperties(schema);

        if (additionalProperties != null) {
            Schema inner = null;
            if (ModelUtils.isArraySchema(schema)) {
                ArraySchema ap = (ArraySchema) schema;
                inner = ap.getItems();
            } else if (ModelUtils.isMapSchema(schema)) {
                inner = getAdditionalProperties(schema);
            }

            codegenModel.additionalPropertiesType = inner != null ? getTypeDeclaration(inner) : getSchemaType(additionalProperties);
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("SWIFT_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable SWIFT_POST_PROCESS_FILE not defined so the Swift code may not be properly formatted. To define it, try 'export SWIFT_POST_PROCESS_FILE=/usr/local/bin/swiftformat' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        // Setup project name
        if (additionalProperties.containsKey(PROJECT_NAME)) {
            setProjectName((String) additionalProperties.get(PROJECT_NAME));
        } else {
            additionalProperties.put(PROJECT_NAME, projectName);
        }
        sourceFolder = projectName + File.separator + sourceFolder;

        // Setup nonPublicApi option, which generates code with reduced access
        // modifiers; allows embedding elsewhere without exposing non-public API calls
        // to consumers
        if (additionalProperties.containsKey(CodegenConstants.NON_PUBLIC_API)) {
            setNonPublicApi(convertPropertyToBooleanAndWriteBack(CodegenConstants.NON_PUBLIC_API));
        }
        additionalProperties.put(CodegenConstants.NON_PUBLIC_API, nonPublicApi);

        // Setup objcCompatible option, which adds additional properties
        // and methods for Objective-C compatibility
        if (additionalProperties.containsKey(OBJC_COMPATIBLE)) {
            setObjcCompatible(convertPropertyToBooleanAndWriteBack(OBJC_COMPATIBLE));
        }
        additionalProperties.put(OBJC_COMPATIBLE, objcCompatible);

        // add objc reserved words
        if (Boolean.TRUE.equals(objcCompatible)) {
            reservedWords.addAll(objcReservedWords);
        }

        if (additionalProperties.containsKey(RESPONSE_AS)) {
            Object responseAsObject = additionalProperties.get(RESPONSE_AS);
            if (responseAsObject instanceof String) {
                setResponseAs(((String) responseAsObject).split(","));
            } else {
                setResponseAs((String[]) responseAsObject);
            }
        }
        additionalProperties.put(RESPONSE_AS, responseAs);
        if (ArrayUtils.contains(responseAs, RESPONSE_LIBRARY_PROMISE_KIT)) {
            additionalProperties.put("usePromiseKit", true);
        }
        if (ArrayUtils.contains(responseAs, RESPONSE_LIBRARY_RX_SWIFT)) {
            additionalProperties.put("useRxSwift", true);
        }
        if (ArrayUtils.contains(responseAs, RESPONSE_LIBRARY_RESULT)) {
            additionalProperties.put("useResult", true);
        }
        if (ArrayUtils.contains(responseAs, RESPONSE_LIBRARY_COMBINE)) {
            additionalProperties.put("useCombine", true);
        }
        if (ArrayUtils.contains(responseAs, RESPONSE_LIBRARY_ASYNC_AWAIT)) {
            additionalProperties.put("useAsyncAwait", true);
        }

        // Setup readonlyProperties option, which declares properties so they can only
        // be set at initialization
        if (additionalProperties.containsKey(READONLY_PROPERTIES)) {
            setReadonlyProperties(convertPropertyToBooleanAndWriteBack(READONLY_PROPERTIES));
        }
        additionalProperties.put(READONLY_PROPERTIES, readonlyProperties);

        // Setup removeMigrationProjectNameClass option, which keeps or remove the projectName class
        if (additionalProperties.containsKey(REMOVE_MIGRATION_PROJECT_NAME_CLASS)) {
            setRemoveMigrationProjectNameClass(convertPropertyToBooleanAndWriteBack(REMOVE_MIGRATION_PROJECT_NAME_CLASS));
        }
        additionalProperties.put(REMOVE_MIGRATION_PROJECT_NAME_CLASS, removeMigrationProjectNameClass);

        // Setup swiftUseApiNamespace option, which makes all the API
        // classes inner-class of {{projectName}}API
        if (additionalProperties.containsKey(SWIFT_USE_API_NAMESPACE)) {
            setSwiftUseApiNamespace(convertPropertyToBooleanAndWriteBack(SWIFT_USE_API_NAMESPACE));
        }

        if (!additionalProperties.containsKey(POD_AUTHORS)) {
            additionalProperties.put(POD_AUTHORS, DEFAULT_POD_AUTHORS);
        }

        if (additionalProperties.containsKey(USE_SPM_FILE_STRUCTURE)) {
            setUseSPMFileStructure(convertPropertyToBooleanAndWriteBack(USE_SPM_FILE_STRUCTURE));
            sourceFolder = "Sources" + File.separator + projectName;
        }

        if (additionalProperties.containsKey(SWIFT_PACKAGE_PATH) && ((String)additionalProperties.get(SWIFT_PACKAGE_PATH)).length() > 0) {
            setSwiftPackagePath((String)additionalProperties.get(SWIFT_PACKAGE_PATH));
            sourceFolder = swiftPackagePath;
        }

        if (additionalProperties.containsKey(USE_BACKTICK_ESCAPES)) {
            setUseBacktickEscapes(convertPropertyToBooleanAndWriteBack(USE_BACKTICK_ESCAPES));
        }

        if (additionalProperties.containsKey(GENERATE_MODEL_ADDITIONAL_PROPERTIES)) {
            setGenerateModelAdditionalProperties(convertPropertyToBooleanAndWriteBack(GENERATE_MODEL_ADDITIONAL_PROPERTIES));
        }
        additionalProperties.put(GENERATE_MODEL_ADDITIONAL_PROPERTIES, generateModelAdditionalProperties);

        if (additionalProperties.containsKey(HASHABLE_MODELS)) {
            setHashableModels(convertPropertyToBooleanAndWriteBack(HASHABLE_MODELS));
        }
        additionalProperties.put(HASHABLE_MODELS, hashableModels);

        if (additionalProperties.containsKey(MAP_FILE_BINARY_TO_DATA)) {
            setMapFileBinaryToData(convertPropertyToBooleanAndWriteBack(MAP_FILE_BINARY_TO_DATA));
        }
        additionalProperties.put(MAP_FILE_BINARY_TO_DATA, mapFileBinaryToData);
        if (mapFileBinaryToData) {
            typeMapping.put("file", "Data");
            typeMapping.put("binary", "Data");
        }

        if (additionalProperties.containsKey(USE_CLASSES)) {
            setUseClasses(convertPropertyToBooleanAndWriteBack(USE_CLASSES));
        }
        additionalProperties.put(USE_CLASSES, useClasses);

        setLenientTypeCast(convertPropertyToBooleanAndWriteBack(LENIENT_TYPE_CAST));

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        if (!getLibrary().equals(LIBRARY_VAPOR)) {
            supportingFiles.add(new SupportingFile("Podspec.mustache",
                    "",
                    projectName + ".podspec"));
            supportingFiles.add(new SupportingFile("Cartfile.mustache",
                    "",
                    "Cartfile"));
            supportingFiles.add(new SupportingFile("CodableHelper.mustache",
                    sourceFolder,
                    "CodableHelper.swift"));
            supportingFiles.add(new SupportingFile("OpenISO8601DateFormatter.mustache",
                    sourceFolder,
                    "OpenISO8601DateFormatter.swift"));
            supportingFiles.add(new SupportingFile("JSONDataEncoding.mustache",
                    sourceFolder,
                    "JSONDataEncoding.swift"));
            supportingFiles.add(new SupportingFile("JSONEncodingHelper.mustache",
                    sourceFolder,
                    "JSONEncodingHelper.swift"));
            supportingFiles.add(new SupportingFile("git_push.sh.mustache",
                    "",
                    "git_push.sh"));
            supportingFiles.add(new SupportingFile("SynchronizedDictionary.mustache",
                    sourceFolder,
                    "SynchronizedDictionary.swift"));
            supportingFiles.add(new SupportingFile("XcodeGen.mustache",
                    "",
                    "project.yml"));
            supportingFiles.add(new SupportingFile("APIHelper.mustache",
                    sourceFolder,
                    "APIHelper.swift"));
            supportingFiles.add(new SupportingFile("Models.mustache",
                    sourceFolder,
                    "Models.swift"));
        }
        supportingFiles.add(new SupportingFile("Package.swift.mustache",
                "",
                "Package.swift"));
        supportingFiles.add(new SupportingFile("Configuration.mustache",
                sourceFolder,
                "Configuration.swift"));
        supportingFiles.add(new SupportingFile("Extensions.mustache",
                sourceFolder,
                "Extensions.swift"));
        supportingFiles.add(new SupportingFile("APIs.mustache",
                sourceFolder,
                "APIs.swift"));
        supportingFiles.add(new SupportingFile("gitignore.mustache",
                "",
                ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache",
                "",
                "README.md"));

        switch (getLibrary()) {
            case LIBRARY_ALAMOFIRE:
                additionalProperties.put("useAlamofire", true);
                supportingFiles.add(new SupportingFile("AlamofireImplementations.mustache",
                        sourceFolder,
                        "AlamofireImplementations.swift"));
                break;
            case LIBRARY_URLSESSION:
                additionalProperties.put("useURLSession", true);
                supportingFiles.add(new SupportingFile("URLSessionImplementations.mustache",
                        sourceFolder,
                        "URLSessionImplementations.swift"));
                break;
            case LIBRARY_VAPOR:
                additionalProperties.put("useVapor", true);
                break;
            default:
                break;
        }

    }

    public boolean isMapFileBinaryToData() {
        return mapFileBinaryToData;
    }

    public void setMapFileBinaryToData(boolean mapFileBinaryToData) {
        this.mapFileBinaryToData = mapFileBinaryToData;
    }

    @Override
    protected boolean isReservedWord(String word) {
        return word != null && reservedWords.contains(word); //don't lowercase as super does
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return useBacktickEscapes && !objcCompatible ? "`" + name + "`" : "_" + name;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder
                + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder
                + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return ModelUtils.isSet(p) ? "Set<" + getTypeDeclaration(inner) + ">" : "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return "[String: " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type) || defaultIncludes.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public boolean isDataTypeFile(String dataType) {
        return "URL".equals(dataType);
    }

    @Override
    public boolean isDataTypeBinary(final String dataType) {
        return "Data".equals(dataType);
    }

    /**
     * Output the proper model name (capitalized).
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    @Override
    public String toModelName(String name) {
        // FIXME parameter should not be assigned. Also declare it as "final"
        name = sanitizeName(name);

        if (!StringUtils.isEmpty(modelNameSuffix)) { // set model suffix
            name = name + "_" + modelNameSuffix;
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) { // set model prefix
            name = modelNamePrefix + "_" + name;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        name = camelize(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = "Model" + name;
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            // e.g. 200Response => Model200Response (after camelize)
            String modelName = "Model" + name;
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    modelName);
            return modelName;
        }

        return name;
    }

    /**
     * Return the capitalized file name of the model.
     *
     * @param name the model name
     * @return the file name of the model
     */
    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getEnum() != null && !p.getEnum().isEmpty()) {
            if (p.getDefault() != null) {
                if (ModelUtils.isStringSchema(p)) {
                    return "." + toEnumVarName(escapeText((String) p.getDefault()), p.getType());
                } else {
                    return "." + toEnumVarName(escapeText(p.getDefault().toString()), p.getType());
                }
            }
        }
        if (p.getDefault() != null) {
            if (ModelUtils.isIntegerSchema(p) || ModelUtils.isNumberSchema(p) || ModelUtils.isBooleanSchema(p)) {
                return p.getDefault().toString();
            } else if (ModelUtils.isDateTimeSchema(p)) {
                // Datetime time stamps in Swift are expressed as Seconds with Microsecond precision.
                // In Java, we need to be creative to get the Timestamp in Microseconds as a long.
                Instant instant = ((OffsetDateTime) p.getDefault()).toInstant();
                long epochMicro = TimeUnit.SECONDS.toMicros(instant.getEpochSecond()) + (instant.get(ChronoField.MICRO_OF_SECOND));
                return "Date(timeIntervalSince1970: " + epochMicro + ".0 / 1_000_000)";
            } else if (ModelUtils.isStringSchema(p)) {
                return "\"" + escapeText((String) p.getDefault()) + "\"";
            }
            // TODO: Handle more cases from `ModelUtils`, such as Date
        }
        return null;
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            return getSchemaType(getAdditionalProperties(p));
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            return ModelUtils.isSet(p) ? "Set<" + inner + ">" : "[" + inner + "]";
        }
        return null;
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultAPI";
        }
        return camelize(apiNamePrefix + "_" + name) + "API";
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace("/", File.separator);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace("/", File.separator);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String toOperationId(String operationId) {
        operationId = camelize(sanitizeName(operationId), true);

        // Throw exception if method name is empty.
        // This should not happen but keep the check just in case
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize(("call_" + operationId), true);
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, camelize(sanitizeName("call_" + operationId), true));
            operationId = camelize(sanitizeName("call_" + operationId), true);
        }


        return operationId;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved words surround with `` or append _
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // for words starting with number, append _
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize(lower) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved words surround with ``
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // for words starting with number, append _
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }

        return name;
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        CodegenModel codegenModel = super.fromModel(name, model);
        if (codegenModel.description != null) {
            codegenModel.imports.add("ApiModel");
        }
        if (allDefinitions != null) {
            String parentSchema = codegenModel.parentSchema;

            // multilevel inheritance: reconcile properties of all the parents
            while (parentSchema != null) {
                final Schema parentModel = allDefinitions.get(parentSchema);
                final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent,
                        parentModel);
                codegenModel = Swift5ClientCodegen.reconcileProperties(codegenModel, parentCodegenModel);

                // get the next parent
                parentSchema = parentCodegenModel.parentSchema;
            }
        }
        if (hashableModels) {
            codegenModel.vendorExtensions.put("x-swift-hashable", true);
        }
        return codegenModel;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public void setNonPublicApi(boolean nonPublicApi) {
        this.nonPublicApi = nonPublicApi;
    }

    public void setObjcCompatible(boolean objcCompatible) {
        this.objcCompatible = objcCompatible;
    }

    public void setLenientTypeCast(boolean lenientTypeCast) {
        this.lenientTypeCast = lenientTypeCast;
    }

    public void setReadonlyProperties(boolean readonlyProperties) {
        this.readonlyProperties = readonlyProperties;
    }

    public void setRemoveMigrationProjectNameClass(boolean removeMigrationProjectNameClass) {
        this.removeMigrationProjectNameClass = removeMigrationProjectNameClass;
    }

    public void setResponseAs(String[] responseAs) {
        this.responseAs = responseAs;
    }

    public void setSwiftUseApiNamespace(boolean swiftUseApiNamespace) {
        this.swiftUseApiNamespace = swiftUseApiNamespace;
    }

    public void setUseSPMFileStructure(boolean useSPMFileStructure) {
        this.useSPMFileStructure = useSPMFileStructure;
    }

    public void setSwiftPackagePath(String swiftPackagePath) {
        this.swiftPackagePath = swiftPackagePath;
    }

    public void setUseClasses(boolean useClasses) {
        this.useClasses = useClasses;
    }

    public void setUseBacktickEscapes(boolean useBacktickEscapes) {
        this.useBacktickEscapes = useBacktickEscapes;
    }

    public void setGenerateModelAdditionalProperties(boolean generateModelAdditionalProperties) {
        this.generateModelAdditionalProperties = generateModelAdditionalProperties;
    }

    public void setHashableModels(boolean hashableModels) {
        this.hashableModels = hashableModels;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        // for string, array of string
        if ("String".equals(datatype) || "[String]".equals(datatype) || "[String: String]".equals(datatype)) {
            return "\"" + value + "\"";
        } else {
            return String.valueOf(value);
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "empty";
        }

        if (enumUnknownDefaultCase) {
            if (name.equals(enumUnknownDefaultCaseName)) {
                return camelize(name, true);
            }
        }

        // Reserved Name
        String nameLowercase = StringUtils.lowerCase(name);
        if (isReservedWord(nameLowercase)) {
            return escapeReservedWord(nameLowercase);
        }

        // Prefix with underscore if name starts with number
        if (name.matches("\\d.*")) {
            return "_" + replaceSpecialCharacters(camelize(name, true));
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return camelize(WordUtils.capitalizeFully(getSymbolName(name).toUpperCase(Locale.ROOT)), true);
        }
        
        // Camelize only when we have a structure defined below
        Boolean camelized = false;
        if (name.matches("[A-Z][a-z0-9]+[a-zA-Z0-9]*")) {
            name = camelize(name, true);
            camelized = true;
        }

        // Check for numerical conversions
        if ("Int".equals(datatype) || "Int32".equals(datatype) || "Int64".equals(datatype)
                || "Float".equals(datatype) || "Double".equals(datatype)) {
            String varName = "number" + camelize(name);
            return replaceSpecialCharacters(varName);
        }

        // If we have already camelized the word, don't progress
        // any further
        if (camelized) {
            return replaceSpecialCharacters(name);
        }

        char[] separators = {'-', '_', ' ', ':', '(', ')'};
        return camelize(replaceSpecialCharacters(WordUtils.capitalizeFully(StringUtils.lowerCase(name), separators)
                        .replaceAll("[-_ :\\(\\)]", "")),
                true);
    }

    private String replaceSpecialCharacters(String name) {
        for (Map.Entry<String, String> specialCharacters : specialCharReplacements.entrySet()) {
            String specialChar = specialCharacters.getKey();
            String replacement = specialCharacters.getValue();
            // Underscore is the only special character we'll allow
            if (!specialChar.equals("_") && name.contains(specialChar)) {
                name = replaceCharacters(name, specialChar, replacement);
            }
        }

        // Fallback, replace unknowns with underscore.
        name = name.replaceAll("\\W+", "_");

        return name;
    }

    private String replaceCharacters(String word, String oldValue, String newValue) {
        if (!word.contains(oldValue)) {
            return word;
        }
        if (word.equals(oldValue)) {
            return newValue;
        }
        int i = word.indexOf(oldValue);
        String start = word.substring(0, i);
        String end = recurseOnEndOfWord(word, oldValue, newValue, i);
        return start + newValue + end;
    }

    private String recurseOnEndOfWord(String word, String oldValue, String newValue, int lastReplacedValue) {
        String end = word.substring(lastReplacedValue + 1);
        if (!end.isEmpty()) {
            end = titleCase(end);
            end = replaceCharacters(end, oldValue, newValue);
        }
        return end;
    }

    private String titleCase(final String input) {
        return input.substring(0, 1).toUpperCase(Locale.ROOT) + input.substring(1);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name);

        // Ensure that the enum type doesn't match a reserved word or
        // the variable name doesn't match the generated enum type or the
        // Swift compiler will generate an error
        if (isReservedWord(property.datatypeWithEnum)
                || toVarName(property.name).equals(property.datatypeWithEnum)) {
            enumName = property.datatypeWithEnum + "Enum";
        }

        // TODO: toModelName already does something for names starting with number,
        // so this code is probably never called
        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        Map<String, Object> postProcessedModelsEnum = postProcessModelsEnum(objs);

        // We iterate through the list of models, and also iterate through each of the
        // properties for each model. For each property, if:
        //
        // CodegenProperty.name != CodegenProperty.baseName
        //
        // then we set
        //
        // CodegenProperty.vendorExtensions["x-codegen-escaped-property-name"] = true
        //
        // Also, if any property in the model has x-codegen-escaped-property-name=true, then we mark:
        //
        // CodegenModel.vendorExtensions["x-codegen-has-escaped-property-names"] = true
        //
        List<Object> models = (List<Object>) postProcessedModelsEnum.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            boolean modelHasPropertyWithEscapedName = false;
            for (CodegenProperty prop : cm.allVars) {
                if (!prop.name.equals(prop.baseName)) {
                    prop.vendorExtensions.put("x-codegen-escaped-property-name", true);
                    modelHasPropertyWithEscapedName = true;
                }

                if (prop.vendorExtensions.containsKey("x-null-encodable")) {
                    if (prop.vendorExtensions.get("x-null-encodable").toString().equals("true")) {
                        if (prop.defaultValue == null || prop.defaultValue.equals("null")) {
                            prop.vendorExtensions.put("x-null-encodable-default-value", ".encodeNull");
                        } else {
                            prop.vendorExtensions.put("x-null-encodable-default-value", ".encodeValue(" + prop.defaultValue + ")");
                        }
                    }
                }
            }
            if (modelHasPropertyWithEscapedName) {
                cm.vendorExtensions.put("x-codegen-has-escaped-property-names", true);
            }
        }

        return postProcessedModelsEnum;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        boolean isSwiftScalarType = property.isInteger || property.isLong || property.isFloat
                || property.isDouble || property.isBoolean;
        if ((!property.required || property.isNullable) && isSwiftScalarType) {
            // Optional scalar types like Int?, Int64?, Float?, Double?, and Bool?
            // do not translate to Objective-C. So we want to flag those
            // properties in case we want to put special code in the templates
            // which provide Objective-C compatibility.
            property.vendorExtensions.put("x-swift-optional-scalar", true);
        }
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

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }
        String swiftPostProcessFile = System.getenv("SWIFT_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(swiftPostProcessFile)) {
            return; // skip if SWIFT_POST_PROCESS_FILE env variable is not defined
        }
        // only process files with swift extension
        if ("swift".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = swiftPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");

        HashMap<String, CodegenModel> modelMaps = new HashMap<String, CodegenModel>();
        for (Object o : allModels) {
            HashMap<String, Object> h = (HashMap<String, Object>) o;
            CodegenModel m = (CodegenModel) h.get("model");
            modelMaps.put(m.classname, m);
        }

        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            for (CodegenParameter cp : operation.allParams) {
                cp.vendorExtensions.put("x-swift-example", constructExampleCode(cp, modelMaps, new HashSet<>()));
            }
        }
        return objs;
    }

    public String constructExampleCode(CodegenParameter codegenParameter, HashMap<String, CodegenModel> modelMaps, Set<String> visitedModels) {
        if (codegenParameter.isArray) { // array
            return "[" + constructExampleCode(codegenParameter.items, modelMaps, visitedModels) + "]";
        } else if (codegenParameter.isMap) { // TODO: map, file type
            return "\"TODO\"";
        } else if (languageSpecificPrimitives.contains(codegenParameter.dataType)) { // primitive type
            if ("String".equals(codegenParameter.dataType) || "Character".equals(codegenParameter.dataType)) {
                if (StringUtils.isEmpty(codegenParameter.example)) {
                    return "\"" + codegenParameter.example + "\"";
                } else {
                    return "\"" + codegenParameter.paramName + "_example\"";
                }
            } else if ("Bool".equals(codegenParameter.dataType)) { // boolean
                if (Boolean.parseBoolean(codegenParameter.example)) {
                    return "true";
                } else {
                    return "false";
                }
            } else if ("URL".equals(codegenParameter.dataType)) { // URL
                return "URL(string: \"https://example.com\")!";
            } else if ("Data".equals(codegenParameter.dataType)) { // URL
                return "Data([9, 8, 7])";
            } else if ("Date".equals(codegenParameter.dataType)) { // date
                return "Date()";
            } else { // numeric
                if (StringUtils.isEmpty(codegenParameter.example)) {
                    return codegenParameter.example;
                } else {
                    return "987";
                }
            }
        } else { // model
            // look up the model
            if (modelMaps.containsKey(codegenParameter.dataType)) {
                if (visitedModels.contains(codegenParameter.dataType)) {
                    // recursive/self-referencing model, simply return nil to avoid stackoverflow
                    return "nil";
                } else {
                    visitedModels.add(codegenParameter.dataType);
                    return constructExampleCode(modelMaps.get(codegenParameter.dataType), modelMaps, visitedModels);
                }
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenParameter.dataType);
                return "TODO";
            }
        }
    }

    public String constructExampleCode(CodegenProperty codegenProperty, HashMap<String, CodegenModel> modelMaps, Set<String> visitedModels) {
        if (codegenProperty.isArray) { // array
            return "[" + constructExampleCode(codegenProperty.items, modelMaps, visitedModels) + "]";
        } else if (codegenProperty.isMap) { // TODO: map, file type
            return "\"TODO\"";
        } else if (languageSpecificPrimitives.contains(codegenProperty.dataType)) { // primitive type
            if ("String".equals(codegenProperty.dataType) || "Character".equals(codegenProperty.dataType)) {
                if (StringUtils.isEmpty(codegenProperty.example)) {
                    return "\"" + codegenProperty.example + "\"";
                } else {
                    return "\"" + codegenProperty.name + "_example\"";
                }
            } else if ("Bool".equals(codegenProperty.dataType)) { // boolean
                if (Boolean.parseBoolean(codegenProperty.example)) {
                    return "true";
                } else {
                    return "false";
                }
            } else if ("URL".equals(codegenProperty.dataType)) { // URL
                return "URL(string: \"https://example.com\")!";
            } else if ("Date".equals(codegenProperty.dataType)) { // date
                return "Date()";
            } else { // numeric
                if (StringUtils.isEmpty(codegenProperty.example)) {
                    return codegenProperty.example;
                } else {
                    return "123";
                }
            }
        } else {
            // look up the model
            if (modelMaps.containsKey(codegenProperty.dataType)) {
                if (visitedModels.contains(codegenProperty.dataType)) {
                    // recursive/self-referencing model, simply return nil to avoid stackoverflow
                    return "nil";
                } else {
                    visitedModels.add(codegenProperty.dataType);
                    return constructExampleCode(modelMaps.get(codegenProperty.dataType), modelMaps, visitedModels);
                }
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenProperty.dataType);
                return "\"TODO\"";
            }
        }
    }

    public String constructExampleCode(CodegenModel codegenModel, HashMap<String, CodegenModel> modelMaps, Set<String> visitedModels) {
        String example;
        example = codegenModel.name + "(";
        List<String> propertyExamples = new ArrayList<>();
        for (CodegenProperty codegenProperty : codegenModel.vars) {
            propertyExamples.add(codegenProperty.name + ": " + constructExampleCode(codegenProperty, modelMaps, visitedModels));
        }
        example += StringUtils.join(propertyExamples, ", ");
        example += ")";
        return example;
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# swift5 generator is contributed by Bruno Coelho (https://github.com/4brunu). #");
        System.out.println("# Please support his work directly via https://paypal.com/paypalme/4brunu \uD83D\uDE4F   #");
        System.out.println("################################################################################");
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.SWIFT; }
}
