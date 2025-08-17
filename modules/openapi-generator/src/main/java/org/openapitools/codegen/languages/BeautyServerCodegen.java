package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.Locale;

import static org.openapitools.codegen.utils.StringUtils.underscore;

public class BeautyServerCodegen extends AbstractCppCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(BeautyServerCodegen.class);

    protected String implFolder = "impl";
    protected boolean isAddExternalLibs = true;
    protected boolean enableValidation = true;
    
    public static final String OPTIONAL_EXTERNAL_LIB = "addExternalLibs";
    public static final String OPTIONAL_EXTERNAL_LIB_DESC = "Add the possibility to fetch and compile external libraries needed by this framework.";
    public static final String HELPERS_PACKAGE_NAME = "helpersPackage";
    public static final String HELPERS_PACKAGE_NAME_DESC = "Specify the package name to be used for the helpers (e.g. beauty.server.helpers).";
    public static final String ENABLE_VALIDATION = "enableValidation";
    public static final String ENABLE_VALIDATION_DESC = "Enable request/response validation based on OpenAPI spec";
    
    protected final String PREFIX = "Beauty";
    protected String helpersPackage = "";

    private static final String STD_STRING = "std::string";
    private static final String STD_MAP = "std::map";
    private static final String STD_VECTOR = "std::vector";
    private static final String CJSON_OBJECT = "cJSON*";

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "beauty-server";
    }

    @Override
    public String getHelp() {
        return "Generates a C++ API server (based on Beauty web server) with request validation";
    }

    public BeautyServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        if (StringUtils.isEmpty(modelNamePrefix)) {
            modelNamePrefix = PREFIX;
        }

        helpersPackage = "beauty.server.helpers";
        apiPackage = "beauty.server.api";
        modelPackage = "beauty.server.model";

        // Set the output folder here
        outputFolder = "generated-code/beauty-server";

        // Templates for APIs only (skip models for now)
        // modelTemplateFiles.put("model-header.mustache", ".hpp");
        // modelTemplateFiles.put("model-source.mustache", ".cpp");

        apiTemplateFiles.put("api-header.mustache", ".hpp");
        apiTemplateFiles.put("api-source.mustache", ".cpp");
        apiTemplateFiles.put("api-impl-header.mustache", ".hpp");
        apiTemplateFiles.put("api-impl-source.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "beauty-server";

        cliOptions.clear();
        addSwitch(OPTIONAL_EXTERNAL_LIB, OPTIONAL_EXTERNAL_LIB_DESC, this.isAddExternalLibs);
        addSwitch(ENABLE_VALIDATION, ENABLE_VALIDATION_DESC, this.enableValidation);
        addOption(HELPERS_PACKAGE_NAME, HELPERS_PACKAGE_NAME_DESC, this.helpersPackage);
        addOption(RESERVED_WORD_PREFIX_OPTION, RESERVED_WORD_PREFIX_DESC, this.reservedWordPrefix);

        setupSupportingFiles();

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("int", "char", "bool", "long", "float", "double", "int32_t", "int64_t"));

        typeMapping = new HashMap<>();
        typeMapping.put("date", STD_STRING);
        typeMapping.put("DateTime", STD_STRING);
        typeMapping.put("string", STD_STRING);
        typeMapping.put("integer", "int32_t");
        typeMapping.put("long", "int64_t");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", STD_VECTOR);
        typeMapping.put("map", STD_MAP);
        typeMapping.put("set", "std::set");
        typeMapping.put("file", STD_STRING);
        typeMapping.put("object", CJSON_OBJECT);
        typeMapping.put("binary", STD_STRING);
        typeMapping.put("number", "double");
        typeMapping.put("UUID", STD_STRING);
        typeMapping.put("URI", STD_STRING);
        typeMapping.put("ByteArray", STD_STRING);

        super.importMapping = new HashMap<>();
        importMapping.put(STD_VECTOR, "#include <vector>");
        importMapping.put(STD_MAP, "#include <map>");
        importMapping.put(STD_STRING, "#include <string>");
        importMapping.put(CJSON_OBJECT, "#include <cjson/cJSON.h>");
    }

    private void setupSupportingFiles() {
        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("api-base-header.mustache", "include", "ApiBase.hpp"));
        supportingFiles.add(new SupportingFile("api-base-source.mustache", "src", "ApiBase.cpp"));
        supportingFiles.add(new SupportingFile("helpers-header.mustache", "include", "BeautyHelpers.hpp"));
        supportingFiles.add(new SupportingFile("helpers-source.mustache", "src", "BeautyHelpers.cpp"));
        supportingFiles.add(new SupportingFile("validation-header.mustache", "include", "BeautyValidation.hpp"));
        supportingFiles.add(new SupportingFile("validation-source.mustache", "src", "BeautyValidation.cpp"));
        supportingFiles.add(new SupportingFile("main-server.mustache", "src", "main.cpp"));
        supportingFiles.add(new SupportingFile("cmake.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("http_result.mustache", "include", "http_result.hpp"));
        supportingFiles.add(new SupportingFile("mime_types.mustache", "include", "mime_types.hpp"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(HELPERS_PACKAGE_NAME)) {
            helpersPackage = (String) additionalProperties.get(HELPERS_PACKAGE_NAME);
        }
        if (additionalProperties.containsKey("modelNamePrefix")) {
            additionalProperties().put("prefix", modelNamePrefix);
            setupSupportingFiles();
        }
        if (additionalProperties.containsKey(RESERVED_WORD_PREFIX_OPTION)) {
            reservedWordPrefix = (String) additionalProperties.get(RESERVED_WORD_PREFIX_OPTION);
        }
        if (additionalProperties.containsKey(ENABLE_VALIDATION)) {
            enableValidation = convertPropertyToBooleanAndWriteBack(ENABLE_VALIDATION);
        } else {
            additionalProperties.put(ENABLE_VALIDATION, enableValidation);
        }

        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
        additionalProperties.put("helpersNamespaceDeclarations", helpersPackage.split("\\."));
        additionalProperties.put("helpersNamespace", helpersPackage.replaceAll("\\.", "::"));
        additionalProperties.put(RESERVED_WORD_PREFIX_OPTION, reservedWordPrefix);
        additionalProperties.put("enableValidation", enableValidation);

        if (additionalProperties.containsKey(OPTIONAL_EXTERNAL_LIB)) {
            setAddExternalLibs(convertPropertyToBooleanAndWriteBack(OPTIONAL_EXTERNAL_LIB));
        } else {
            additionalProperties.put(OPTIONAL_EXTERNAL_LIB, isAddExternalLibs);
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap operations = objs.getOperations();
        String classname = operations.getClassname();
        operations.put("classnameSnakeUpperCase", underscore(classname).toUpperCase(Locale.ROOT));
        operations.put("classnameSnakeLowerCase", underscore(classname).toLowerCase(Locale.ROOT));
        
        List<CodegenOperation> operationList = operations.getOperation();
        for (CodegenOperation op : operationList) {
            postProcessSingleOperation(operations, op);
        }

        return objs;
    }

    private void postProcessSingleOperation(OperationMap operations, CodegenOperation op) {
        // Customize operation processing for Beauty
        op.vendorExtensions.put("x-beauty-path", op.path);
        op.httpMethod = op.httpMethod.toUpperCase(Locale.ROOT);
        
        // Add validation flags
        boolean hasValidation = false;
        
        // Process parameters for Beauty-style handling and validation
        for (CodegenParameter param : op.allParams) {
            if (param.isQueryParam) {
                param.vendorExtensions.put("x-beauty-query-param", true);
            }
            if (param.isPathParam) {
                param.vendorExtensions.put("x-beauty-path-param", true);
            }
            
            // Check if parameter has validation constraints
            if (hasValidationConstraints(param)) {
                hasValidation = true;
                param.vendorExtensions.put("x-beauty-has-validation", true);
            }
        }
        
        // Check body validation
        if (op.getHasBodyParam()) {
            hasValidation = true;
            op.vendorExtensions.put("x-beauty-validate-body", true);
        }
        
        op.vendorExtensions.put("x-beauty-has-validation", true);
    }

    private boolean hasValidationConstraints(CodegenParameter param) {
        return param.minimum != null || param.maximum != null || param.minLength != null || param.maxLength != null || 
               param.pattern != null || param.hasValidation || param.isEnum;
    }

    @Override
    public String toModelImport(String name) {
        if (name.startsWith("#include")) {
            return null;
        }

        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        } else {
            return "#include \"" + name + ".hpp\"";
        }
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if (templateName.endsWith("impl-header.mustache")) {
            result = implFilenameFromApiFilename(result, ".hpp");
        } else if (templateName.endsWith("impl-source.mustache")) {
            result = implFilenameFromApiFilename(result, ".cpp");
        }
        return result;
    }

    private String implFilenameFromApiFilename(String filename, String suffix) {
        String result = filename.substring(0, filename.length() - suffix.length()) + "Impl" + suffix;
        result = result.replace(apiFileFolder(), implFileFolder());
        return result;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + "/api").replace("/", File.separator);
    }

    private String implFileFolder() {
        return (outputFolder + "/" + implFolder).replace("/", File.separator);
    }

    public void setAddExternalLibs(boolean value) {
        isAddExternalLibs = value;
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.C_PLUS_PLUS;
    }
}
