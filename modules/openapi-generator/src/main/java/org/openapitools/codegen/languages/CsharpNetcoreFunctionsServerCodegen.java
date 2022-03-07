/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static java.util.UUID.randomUUID;

public class CsharpNetcoreFunctionsServerCodegen extends AbstractCSharpCodegen {

    public static final String NET_CORE_VERSION = "netCoreVersion";
    public static final String AZURE_FUNCTIONS_VERSION = "azureFunctionsVersion";
    public static final String CLASS_MODIFIER = "classModifier";
    public static final String OPERATION_MODIFIER = "operationModifier";
    public static final String OPERATION_IS_ASYNC = "operationIsAsync";
    public static final String OPERATION_RESULT_TASK = "operationResultTask";
    public static final String GENERATE_BODY = "generateBody";
    public static final String BUILD_TARGET = "buildTarget";
    public static final String MODEL_CLASS_MODIFIER = "modelClassModifier";
    public static final String TARGET_FRAMEWORK= "targetFramework";
    public static final String FUNCTIONS_SDK_VERSION = "functionsSDKVersion";

    public static final String COMPATIBILITY_VERSION = "compatibilityVersion";
    public static final String USE_NEWTONSOFT = "useNewtonsoft";
    public static final String NEWTONSOFT_VERSION = "newtonsoftVersion";

    private String packageGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";
    private String userSecretsGuid = randomUUID().toString();

    protected final Logger LOGGER = LoggerFactory.getLogger(AspNetCoreServerCodegen.class);

    protected int serverPort = 8080;
    protected String serverHost = "0.0.0.0";
    protected CliOption netCoreVersion = new CliOption(NET_CORE_VERSION, ".NET Core version: 6.0, 5.0, 3.1, 3.0");
    protected CliOption azureFunctionsVersion = new CliOption(AZURE_FUNCTIONS_VERSION, "Azure functions version: v4, v3");
    private CliOption classModifier = new CliOption(CLASS_MODIFIER, "Class Modifier for function classes: Empty string or abstract.");
    private CliOption operationModifier = new CliOption(OPERATION_MODIFIER, "Operation Modifier can be virtual or abstract");
    private CliOption modelClassModifier = new CliOption(MODEL_CLASS_MODIFIER, "Model Class Modifier can be nothing or partial");
    private boolean generateBody = true;
    private CliOption buildTarget = new CliOption("buildTarget", "Target to build an application or library");
    private String projectSdk = "Microsoft.NET.Sdk";
    private boolean operationIsAsync = false;
    private boolean operationResultTask = false;
    private boolean isLibrary = false;
    private boolean useFrameworkReference = false;
    private boolean useNewtonsoft = true;
    private String newtonsoftVersion = "3.0.0";

    public CsharpNetcoreFunctionsServerCodegen() {
        super();

        // TODO: AspnetCore community review
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .excludeWireFormatFeatures(WireFormatFeature.PROTOBUF)
                .includeSecurityFeatures(
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken
                )
                .excludeSecurityFeatures(
                        SecurityFeature.OpenIDConnect,
                        SecurityFeature.OAuth2_Password,
                        SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_ClientCredentials,
                        SecurityFeature.OAuth2_Implicit
                )
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code" + File.separator + getName();

        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("function.mustache", ".cs");

        embeddedTemplateDir = templateDir = "csharp-netcore-functions";

        // contextually reserved words
        // NOTE: C# uses camel cased reserved words, while models are title cased. We don't want lowercase comparisons.
        reservedWords.addAll(
                Arrays.asList("var", "async", "await", "dynamic", "yield")
        );

        cliOptions.clear();

        typeMapping.put("boolean", "bool");
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("long", "long");
        typeMapping.put("double", "double");
        typeMapping.put("number", "decimal");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("date", "DateTime");
        typeMapping.put("UUID", "Guid");
        typeMapping.put("URI", "string");

        setSupportNullable(Boolean.TRUE);

        // CLI options
        addOption(CodegenConstants.PACKAGE_DESCRIPTION,
                CodegenConstants.PACKAGE_DESCRIPTION_DESC,
                packageDescription);

        addOption(CodegenConstants.LICENSE_URL,
                CodegenConstants.LICENSE_URL_DESC,
                licenseUrl);

        addOption(CodegenConstants.LICENSE_NAME,
                CodegenConstants.LICENSE_NAME_DESC,
                licenseName);

        addOption(CodegenConstants.PACKAGE_COPYRIGHT,
                CodegenConstants.PACKAGE_COPYRIGHT_DESC,
                packageCopyright);

        addOption(CodegenConstants.PACKAGE_AUTHORS,
                CodegenConstants.PACKAGE_AUTHORS_DESC,
                packageAuthors);

        addOption(CodegenConstants.PACKAGE_TITLE,
                CodegenConstants.PACKAGE_TITLE_DESC,
                packageTitle);

        addOption(CodegenConstants.PACKAGE_NAME,
                "C# package name (convention: Title.Case).",
                packageName);

        addOption(CodegenConstants.PACKAGE_VERSION,
                "C# package version.",
                packageVersion);

        addOption(CodegenConstants.OPTIONAL_PROJECT_GUID,
                CodegenConstants.OPTIONAL_PROJECT_GUID_DESC,
                null);

        addOption(CodegenConstants.SOURCE_FOLDER,
                CodegenConstants.SOURCE_FOLDER_DESC,
                sourceFolder);

        netCoreVersion.addEnum("3.0", ".NET Core 3.0");
        netCoreVersion.addEnum("3.1", ".NET Core 3.1");
        netCoreVersion.addEnum("5.0", ".NET Core 5.0");
        netCoreVersion.addEnum("6.0", ".NET Core 6.0");
        netCoreVersion.setDefault("3.1");
        netCoreVersion.setOptValue(netCoreVersion.getDefault());
        cliOptions.add(netCoreVersion);

        azureFunctionsVersion.addEnum("v4", "Azure Functions v4");
        azureFunctionsVersion.addEnum("v3", "Azure Functions v3");
        azureFunctionsVersion.setDefault("v4");
        azureFunctionsVersion.setOptValue(azureFunctionsVersion.getDefault());
        cliOptions.add(azureFunctionsVersion);

        // CLI Switches
        addSwitch(CodegenConstants.NULLABLE_REFERENCE_TYPES,
                CodegenConstants.NULLABLE_REFERENCE_TYPES_DESC,
                this.nullReferenceTypesFlag);

        addSwitch(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC,
                sortParamsByRequiredFlag);

        addSwitch(CodegenConstants.USE_DATETIME_OFFSET,
                CodegenConstants.USE_DATETIME_OFFSET_DESC,
                useDateTimeOffsetFlag);

        addSwitch(CodegenConstants.USE_COLLECTION,
                CodegenConstants.USE_COLLECTION_DESC,
                useCollection);

        addSwitch(CodegenConstants.RETURN_ICOLLECTION,
                CodegenConstants.RETURN_ICOLLECTION_DESC,
                returnICollection);

        addSwitch(USE_NEWTONSOFT,
                "Uses the Newtonsoft JSON library.",
                useNewtonsoft);

        addOption(NEWTONSOFT_VERSION,
                "Version for Newtonsoft.Json for .NET Core 3.0+",
                newtonsoftVersion);

        addOption(CodegenConstants.ENUM_NAME_SUFFIX,
                CodegenConstants.ENUM_NAME_SUFFIX_DESC,
                enumNameSuffix);

        addOption(CodegenConstants.ENUM_VALUE_SUFFIX,
                "Suffix that will be appended to all enum values.",
                enumValueSuffix);

        addSwitch(GENERATE_BODY,
                "Generates method body.",
                generateBody);

        classModifier.addEnum("", "Keep class default with no modifier");
        classModifier.addEnum("abstract", "Make class abstract");
        classModifier.setDefault("");
        classModifier.setOptValue(classModifier.getDefault());
        addOption(classModifier.getOpt(), classModifier.getDescription(), classModifier.getOptValue());

        operationModifier.addEnum("virtual", "Keep method virtual");
        operationModifier.addEnum("abstract", "Make method abstract");
        operationModifier.setDefault("virtual");
        operationModifier.setOptValue(operationModifier.getDefault());
        cliOptions.add(operationModifier);

        buildTarget.addEnum("program", "Generate code for a standalone server");
        buildTarget.addEnum("library", "Generate code for a server abstract class library");
        buildTarget.setDefault("program");
        buildTarget.setOptValue(buildTarget.getDefault());
        cliOptions.add(buildTarget);

        addSwitch(GENERATE_BODY,
                "Generates method body.",
                generateBody);

        addSwitch(OPERATION_IS_ASYNC,
                "Set methods to async or sync (default).",
                operationIsAsync);

        addSwitch(OPERATION_RESULT_TASK,
                "Set methods result to Task<>.",
                operationResultTask);

        modelClassModifier.setType("String");
        modelClassModifier.addEnum("", "Keep model class default with no modifier");
        modelClassModifier.addEnum("partial", "Make model class partial");
        modelClassModifier.setDefault("partial");
        modelClassModifier.setOptValue(modelClassModifier.getDefault());
        addOption(modelClassModifier.getOpt(), modelClassModifier.getDescription(), modelClassModifier.getOptValue());
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "csharp-netcore-functions";
    }

    @Override
    public String getHelp() {
        return "Generates an ASP.NET Core Web API server.";
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
        additionalProperties.put("serverHost", url.getHost());
        additionalProperties.put("serverPort", URLPathUtils.getPort(url, 8080));

        setApiBasePath();
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_GUID)) {
            setPackageGuid((String) additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_GUID));
        }
        additionalProperties.put("packageGuid", packageGuid);
        additionalProperties.put("userSecretsGuid", userSecretsGuid);

        if (!additionalProperties.containsKey(NEWTONSOFT_VERSION)) {
            additionalProperties.put(NEWTONSOFT_VERSION, newtonsoftVersion);
        } else {
            newtonsoftVersion = (String) additionalProperties.get(NEWTONSOFT_VERSION);
        }

        // Check for the modifiers etc.
        // The order of the checks is important.
        setClassModifier();
        setOperationModifier();
        setModelClassModifier();
        setOperationIsAsync();


        additionalProperties.put("dockerTag", packageName.toLowerCase(Locale.ROOT));

        if (!additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            apiPackage = packageName + ".Functions";
            additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        }

        if (!additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            modelPackage = packageName + ".Models";
            additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        }

        String packageFolder = sourceFolder + File.separator + packageName;

        // determine the ASP.NET core version setting
        setAzureFunctionsVersion();
        setNetCoreVersion(packageFolder);
        setUseNewtonsoft();

        // Check for class modifier if not present set the default value.

        supportingFiles.add(new SupportingFile("build.sh.mustache", "", "build.sh"));
        supportingFiles.add(new SupportingFile("build.bat.mustache", "", "build.bat"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
        supportingFiles.add(new SupportingFile("gitignore", packageFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("OpenApi" + File.separator + "TypeExtensions.mustache", packageFolder + File.separator + "OpenApi", "TypeExtensions.cs"));
        supportingFiles.add(new SupportingFile("Project.csproj.mustache", packageFolder, packageName + ".csproj"));
        supportingFiles.add(new SupportingFile("typeConverter.mustache", packageFolder + File.separator + "Converters", "CustomEnumConverter.cs"));
        supportingFiles.add(new SupportingFile("host.json.mustache", packageFolder, "host.json"));
        supportingFiles.add(new SupportingFile("local.settings.json.mustache", packageFolder, "local.settings.json"));
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + packageName + File.separator + "Functions";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + packageName + File.separator + "Models";
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateJSONSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    protected void processOperation(CodegenOperation operation) {
        super.processOperation(operation);

        // HACK: Unlikely in the wild, but we need to clean operation paths for MVC Routing
        if (operation.path != null) {
            if (operation.path.startsWith("/"))
            {
                operation.path = operation.path.substring(1);
            }
            String original = operation.path;
            operation.path = operation.path.replace("?", "/");
            if (!original.equals(operation.path)) {
                LOGGER.warn("Normalized {} to {}. Please verify generated source.", original, operation.path);
            }
        }

        // Converts, for example, PUT to HttpPut for function attributes
        operation.httpMethod = operation.httpMethod.charAt(0) + operation.httpMethod.substring(1).toLowerCase(Locale.ROOT);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        // We need to postprocess the operations to add proper consumes tags and fix form file handling
        if (objs != null) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    if (operation.consumes == null) {
                        continue;
                    }
                    if (operation.consumes.size() == 0) {
                        continue;
                    }

                    // Build a consumes string for the operation we cannot iterate in the template as we need a ','
                    // after each entry but the last

                    StringBuilder consumesString = new StringBuilder();
                    for (Map<String, String> consume : operation.consumes) {
                        if (!consume.containsKey("mediaType")) {
                            continue;
                        }

                        if(consumesString.toString().isEmpty()) {
                            consumesString = new StringBuilder("\"" + consume.get("mediaType") + "\"");
                        }
                        else {
                            consumesString.append(", \"").append(consume.get("mediaType")).append("\"");
                        }

                        // In a multipart/form-data consuming context binary data is best handled by an IFormFile
                        if (!consume.get("mediaType").equals("multipart/form-data")) {
                            continue;
                        }

                        // Change dataType of binary parameters to IFormFile for formParams in multipart/form-data
                        for (CodegenParameter param : operation.formParams) {
                            if (param.isBinary) {
                                param.dataType = "IFormFile";
                                param.baseType = "IFormFile";
                            }
                        }

                        for (CodegenParameter param : operation.allParams) {
                            if (param.isBinary && param.isFormParam) {
                                param.dataType = "IFormFile";
                                param.baseType = "IFormFile";
                            }
                        }
                    }

                    if(!consumesString.toString().isEmpty()) {
                        operation.vendorExtensions.put("x-aspnetcore-consumes", consumesString.toString());
                    }
                }
            }
        }
        return objs;
    }

    @Override
    public Mustache.Compiler processCompiler(Mustache.Compiler compiler) {
        // To avoid unexpected behaviors when options are passed programmatically such as { "useCollection": "" }
        return super.processCompiler(compiler).emptyStringIsFalse(true);
    }

    @Override
    public String toRegularExpression(String pattern) {
        return escapeText(pattern);
    }

    @SuppressWarnings("rawtypes")
    @Override
    public String getNullableType(Schema p, String type) {
        if (languageSpecificPrimitives.contains(type)) {
            if (isSupportNullable() && ModelUtils.isNullable(p) && (nullableType.contains(type) || nullReferenceTypesFlag)) {
                return type + "?";
            } else {
                return type;
            }
        } else {
            return null;
        }
    }

    private void setCliOption(CliOption cliOption) throws IllegalArgumentException {
        if (additionalProperties.containsKey(cliOption.getOpt())) {
            // TODO Hack - not sure why the empty strings become boolean.
            Object obj = additionalProperties.get(cliOption.getOpt());
            if (!SchemaTypeUtil.BOOLEAN_TYPE.equals(cliOption.getType())) {
                if (obj instanceof Boolean) {
                    obj = "";
                    additionalProperties.put(cliOption.getOpt(), obj);
                }
            }
            cliOption.setOptValue(obj.toString());
        } else {
            additionalProperties.put(cliOption.getOpt(), cliOption.getOptValue());
        }
        if (cliOption.getOptValue() == null) {
            cliOption.setOptValue(cliOption.getDefault());
            throw new IllegalArgumentException(cliOption.getOpt() + ": Invalid value '" + additionalProperties.get(cliOption.getOpt()).toString() + "'" +
                    ". " + cliOption.getDescription());
        }
    }

    private void setClassModifier() {
        // CHeck for class modifier if not present set the default value.
        setCliOption(classModifier);

        // If class modifier is abstract then the methods need to be abstract too.
        if ("abstract".equals(classModifier.getOptValue())) {
            operationModifier.setOptValue(classModifier.getOptValue());
            additionalProperties.put(OPERATION_MODIFIER, operationModifier.getOptValue());
            LOGGER.warn("classModifier is {} so forcing operationModifier to {}", classModifier.getOptValue(), operationModifier.getOptValue());
        }
    }

    private void setOperationModifier() {
        setCliOption(operationModifier);

        // If operation modifier is abstract then dont generate any body
        if ("abstract".equals(operationModifier.getOptValue())) {
            generateBody = false;
            additionalProperties.put(GENERATE_BODY, generateBody);
            LOGGER.warn("operationModifier is {} so forcing generateBody to {}", operationModifier.getOptValue(), generateBody);
        } else if (additionalProperties.containsKey(GENERATE_BODY)) {
            generateBody = convertPropertyToBooleanAndWriteBack(GENERATE_BODY);
        } else {
            additionalProperties.put(GENERATE_BODY, generateBody);
        }
    }

    private void setModelClassModifier() {
        setCliOption(modelClassModifier);

        // If operation modifier is abstract then dont generate any body
        if (isLibrary) {
            modelClassModifier.setOptValue("");
            additionalProperties.put(MODEL_CLASS_MODIFIER, modelClassModifier.getOptValue());
            LOGGER.warn("buildTarget is {} so removing any modelClassModifier ", buildTarget.getOptValue());
        }
    }

    private void setNetCoreVersion(String packageFolder) {
        setCliOption(netCoreVersion);

        LOGGER.info("ASP.NET core version: {}", netCoreVersion.getOptValue());
    }

    private void setAzureFunctionsVersion() {
        setCliOption(azureFunctionsVersion);
        String functionsSDKVersion = "3.0.13";

        if ("v4".equals(azureFunctionsVersion.getOptValue())) {
            functionsSDKVersion = "4.0.1";

            if (!netCoreVersion.getOptValue().startsWith("6.")) {
                LOGGER.warn("ASP.NET core version: {} is not compatible with Azure functions v4. Using version 6.0.", netCoreVersion.getOptValue());
                netCoreVersion.setOptValue("6.0");
            }
        }

        additionalProperties.put(FUNCTIONS_SDK_VERSION, functionsSDKVersion);

        //set .NET target version
        String targetFrameworkVersion = "net" + netCoreVersion.getOptValue();
        additionalProperties.put(TARGET_FRAMEWORK, targetFrameworkVersion);
    }

    private void setOperationIsAsync() {
        if (isLibrary) {
            operationIsAsync = false;
            additionalProperties.put(OPERATION_IS_ASYNC, operationIsAsync);
        } else if (additionalProperties.containsKey(OPERATION_IS_ASYNC)) {
            operationIsAsync = convertPropertyToBooleanAndWriteBack(OPERATION_IS_ASYNC);
        } else {
            additionalProperties.put(OPERATION_IS_ASYNC, operationIsAsync);
        }
    }

    private void setUseNewtonsoft() {
        if (additionalProperties.containsKey(USE_NEWTONSOFT)) {
            useNewtonsoft = convertPropertyToBooleanAndWriteBack(USE_NEWTONSOFT);
        } else {
            additionalProperties.put(USE_NEWTONSOFT, useNewtonsoft);
        }
    }

    private void setApiBasePath() {
        URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
        String apiBasePath = encodePath(url.getPath()).replaceAll("/$", "");

        // if a base path exists, remove leading '/' and append trailing '/'
        if (apiBasePath != null && apiBasePath.length() > 0) {
            if (apiBasePath.startsWith("/"))
                apiBasePath = apiBasePath.substring(1);

            if (apiBasePath.endsWith("/"))
                apiBasePath = apiBasePath.substring(0, apiBasePath.lastIndexOf("/"));
        }

        additionalProperties.put("apiBasePath", apiBasePath);
    }
}
