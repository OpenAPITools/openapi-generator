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
import java.util.Locale;
import java.util.Map;

import static java.util.UUID.randomUUID;

public class AspNetCoreServerCodegen extends AbstractCSharpCodegen {

    public static final String USE_SWASHBUCKLE = "useSwashbuckle";
    public static final String ASPNET_CORE_VERSION = "aspnetCoreVersion";
    public static final String SWASHBUCKLE_VERSION = "swashbuckleVersion";
    public static final String CLASS_MODIFIER = "classModifier";
    public static final String OPERATION_MODIFIER = "operationModifier";
    public static final String OPERATION_IS_ASYNC = "operationIsAsync";
    public static final String OPERATION_RESULT_TASK = "operationResultTask";
    public static final String GENERATE_BODY = "generateBody";
    public static final String BUILD_TARGET = "buildTarget";
    public static final String MODEL_CLASS_MODIFIER = "modelClassModifier";

    public static final String PROJECT_SDK = "projectSdk";
    public static final String SDK_WEB = "Microsoft.NET.Sdk.Web";
    public static final String SDK_LIB = "Microsoft.NET.Sdk";
    public static final String COMPATIBILITY_VERSION = "compatibilityVersion";
    public static final String IS_LIBRARY = "isLibrary";
    public static final String USE_FRAMEWORK_REFERENCE = "useFrameworkReference";
    public static final String USE_NEWTONSOFT = "useNewtonsoft";
    public static final String USE_DEFAULT_ROUTING = "useDefaultRouting";
    public static final String NEWTONSOFT_VERSION = "newtonsoftVersion";

    private String packageGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";
    private String userSecretsGuid = randomUUID().toString();

    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(AspNetCoreServerCodegen.class);

    private boolean useSwashbuckle = true;
    protected int serverPort = 8080;
    protected String serverHost = "0.0.0.0";
    protected CliOption swashbuckleVersion = new CliOption(SWASHBUCKLE_VERSION, "Swashbuckle version: 3.0.0, 4.0.0, 5.0.0");
    protected CliOption aspnetCoreVersion = new CliOption(ASPNET_CORE_VERSION, "ASP.NET Core version: 3.1, 3.0, 2.2, 2.1, 2.0 (deprecated)");
    private CliOption classModifier = new CliOption(CLASS_MODIFIER, "Class Modifier can be empty, abstract");
    private CliOption operationModifier = new CliOption(OPERATION_MODIFIER, "Operation Modifier can be virtual, abstract or partial");
    private CliOption modelClassModifier = new CliOption(MODEL_CLASS_MODIFIER, "Model Class Modifier can be nothing or partial");
    private boolean generateBody = true;
    private CliOption buildTarget = new CliOption("buildTarget", "Target to build an application or library");
    private String projectSdk = SDK_WEB;
    private String compatibilityVersion = "Version_2_2";
    private boolean operationIsAsync = false;
    private boolean operationResultTask = false;
    private boolean isLibrary = false;
    private boolean useFrameworkReference = false;
    private boolean useNewtonsoft = true;
    private boolean useDefaultRouting = true;
    private String newtonsoftVersion = "3.0.0";

    public AspNetCoreServerCodegen() {
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
        apiTemplateFiles.put("controller.mustache", ".cs");

        embeddedTemplateDir = templateDir = "aspnetcore/2.1";

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

        addOption(COMPATIBILITY_VERSION, "ASP.Net Core CompatibilityVersion", compatibilityVersion);

        aspnetCoreVersion.addEnum("2.0", "ASP.NET Core 2.0");
        aspnetCoreVersion.addEnum("2.1", "ASP.NET Core 2.1");
        aspnetCoreVersion.addEnum("2.2", "ASP.NET Core 2.2");
        aspnetCoreVersion.addEnum("3.0", "ASP.NET Core 3.0");
        aspnetCoreVersion.addEnum("3.1", "ASP.NET Core 3.1");
        aspnetCoreVersion.setDefault("2.2");
        aspnetCoreVersion.setOptValue(aspnetCoreVersion.getDefault());
        addOption(aspnetCoreVersion.getOpt(), aspnetCoreVersion.getDescription(), aspnetCoreVersion.getOptValue());

        swashbuckleVersion.addEnum("3.0.0", "Swashbuckle 3.0.0");
        swashbuckleVersion.addEnum("4.0.0", "Swashbuckle 4.0.0");
        swashbuckleVersion.addEnum("5.0.0", "Swashbuckle 5.0.0");
        swashbuckleVersion.setDefault("3.0.0");
        swashbuckleVersion.setOptValue(swashbuckleVersion.getDefault());
        addOption(swashbuckleVersion.getOpt(), swashbuckleVersion.getDescription(), swashbuckleVersion.getOptValue());

        // CLI Switches
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

        addSwitch(USE_SWASHBUCKLE,
                "Uses the Swashbuckle.AspNetCore NuGet package for documentation.",
                useSwashbuckle);

        addSwitch(IS_LIBRARY,
                "Is the build a library",
                isLibrary);

        addSwitch(USE_FRAMEWORK_REFERENCE,
                "Use frameworkReference for ASP.NET Core 3.0+ and PackageReference ASP.NET Core 2.2 or earlier.",
                useFrameworkReference);

        addSwitch(USE_NEWTONSOFT,
                "Uses the Newtonsoft JSON library.",
                useNewtonsoft);

        addOption(NEWTONSOFT_VERSION,
                "Version for Microsoft.AspNetCore.Mvc.NewtonsoftJson for ASP.NET Core 3.0+",
                newtonsoftVersion);

        addSwitch(USE_DEFAULT_ROUTING,
                "Use default routing for the ASP.NET Core version.",
                useDefaultRouting);

        addOption(CodegenConstants.ENUM_NAME_SUFFIX,
                CodegenConstants.ENUM_NAME_SUFFIX_DESC,
                enumNameSuffix);

        addOption(CodegenConstants.ENUM_VALUE_SUFFIX,
                "Suffix that will be appended to all enum values.",
                enumValueSuffix);

        classModifier.addEnum("", "Keep class default with no modifier");
        classModifier.addEnum("abstract", "Make class abstract");
        classModifier.setDefault("");
        classModifier.setOptValue(classModifier.getDefault());
        addOption(classModifier.getOpt(), classModifier.getDescription(), classModifier.getOptValue());

        operationModifier.addEnum("virtual", "Keep method virtual");
        operationModifier.addEnum("abstract", "Make method abstract");
        operationModifier.setDefault("virtual");
        operationModifier.setOptValue(operationModifier.getDefault());
        addOption(operationModifier.getOpt(), operationModifier.getDescription(), operationModifier.getOptValue());

        buildTarget.addEnum("program", "Generate code for a standalone server");
        buildTarget.addEnum("library", "Generate code for a server abstract class library");
        buildTarget.setDefault("program");
        buildTarget.setOptValue(buildTarget.getDefault());
        addOption(buildTarget.getOpt(), buildTarget.getDescription(), buildTarget.getOptValue());

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
        return "aspnetcore";
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

        // CHeck for the modifiers etc.
        // The order of the checks is important.
        setBuildTarget();
        setClassModifier();
        setOperationModifier();
        setModelClassModifier();
        setUseSwashbuckle();
        setOperationIsAsync();

        // CHeck for class modifier if not present set the default value.
        additionalProperties.put(PROJECT_SDK, projectSdk);

        additionalProperties.put("dockerTag", packageName.toLowerCase(Locale.ROOT));

        if (!additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            apiPackage = packageName + ".Controllers";
            additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        }

        if (!additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            modelPackage = packageName + ".Models";
            additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        }

        String packageFolder = sourceFolder + File.separator + packageName;

        // determine the ASP.NET core version setting
        setAspnetCoreVersion(packageFolder);
        setSwashbuckleVersion();
        setIsFramework();
        setUseNewtonsoft();
        setUseEndpointRouting();

        supportingFiles.add(new SupportingFile("build.sh.mustache", "", "build.sh"));
        supportingFiles.add(new SupportingFile("build.bat.mustache", "", "build.bat"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
        supportingFiles.add(new SupportingFile("gitignore", packageFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("validateModel.mustache", packageFolder + File.separator + "Attributes", "ValidateModelStateAttribute.cs"));
        supportingFiles.add(new SupportingFile("typeConverter.mustache", packageFolder + File.separator + "Converters", "CustomEnumConverter.cs"));
        if (aspnetCoreVersion.getOptValue().startsWith("3.")) {
            supportingFiles.add(new SupportingFile("OpenApi" + File.separator + "TypeExtensions.mustache", packageFolder + File.separator + "OpenApi", "TypeExtensions.cs"));
        }
        supportingFiles.add(new SupportingFile("Project.csproj.mustache", packageFolder, packageName + ".csproj"));
        if (!isLibrary) {
            supportingFiles.add(new SupportingFile("Dockerfile.mustache", packageFolder, "Dockerfile"));
            supportingFiles.add(new SupportingFile("appsettings.json", packageFolder, "appsettings.json"));

            supportingFiles.add(new SupportingFile("Startup.mustache", packageFolder, "Startup.cs"));
            supportingFiles.add(new SupportingFile("Program.mustache", packageFolder, "Program.cs"));
            supportingFiles.add(new SupportingFile("Properties" + File.separator + "launchSettings.json",
                    packageFolder + File.separator + "Properties", "launchSettings.json"));
            // wwwroot files.
            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "README.md", packageFolder + File.separator + "wwwroot", "README.md"));
            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "index.html", packageFolder + File.separator + "wwwroot", "index.html"));
            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "openapi-original.mustache",
                    packageFolder + File.separator + "wwwroot", "openapi-original.json"));
        } else {
            supportingFiles.add(new SupportingFile("Project.nuspec.mustache", packageFolder, packageName + ".nuspec"));
        }

        if (useSwashbuckle) {
            supportingFiles.add(new SupportingFile("Filters" + File.separator + "BasePathFilter.mustache",
                    packageFolder + File.separator + "Filters", "BasePathFilter.cs"));
            supportingFiles.add(new SupportingFile("Filters" + File.separator + "GeneratePathParamsValidationFilter.mustache",
                    packageFolder + File.separator + "Filters", "GeneratePathParamsValidationFilter.cs"));
        }

        supportingFiles.add(new SupportingFile("Authentication" + File.separator + "ApiAuthentication.mustache", packageFolder + File.separator + "Authentication", "ApiAuthentication.cs"));
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + packageName + File.separator + "Controllers";
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
            String original = operation.path;
            operation.path = operation.path.replace("?", "/");
            if (!original.equals(operation.path)) {
                LOGGER.warn("Normalized " + original + " to " + operation.path + ". Please verify generated source.");
            }
        }

        // Converts, for example, PUT to HttpPut for controller attributes
        operation.httpMethod = "Http" + operation.httpMethod.substring(0, 1) + operation.httpMethod.substring(1).toLowerCase(Locale.ROOT);
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

    @Override
    public String getNullableType(Schema p, String type) {
        if (languageSpecificPrimitives.contains(type)) {
            if (isSupportNullable() && ModelUtils.isNullable(p) && nullableType.contains(type)) {
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
            LOGGER.warn("classModifier is " + classModifier.getOptValue() + " so forcing operatonModifier to " + operationModifier.getOptValue());
        }
    }

    private void setOperationModifier() {
        setCliOption(operationModifier);

        // If operation modifier is abstract then dont generate any body
        if ("abstract".equals(operationModifier.getOptValue())) {
            generateBody = false;
            additionalProperties.put(GENERATE_BODY, generateBody);
            LOGGER.warn("operationModifier is " + operationModifier.getOptValue() + " so forcing generateBody to " + generateBody);
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
            LOGGER.warn("buildTarget is " + buildTarget.getOptValue() + " so removing any modelClassModifier ");
        }
    }

    private void setBuildTarget() {
        setCliOption(buildTarget);
        if ("library".equals(buildTarget.getOptValue())) {
            isLibrary = true;
            projectSdk = SDK_LIB;
            additionalProperties.put(CLASS_MODIFIER, "abstract");
        } else {
            isLibrary = false;
            projectSdk = SDK_WEB;
        }
        additionalProperties.put(IS_LIBRARY, isLibrary);
    }

    private void setAspnetCoreVersion(String packageFolder) {
        setCliOption(aspnetCoreVersion);

        if (aspnetCoreVersion.getOptValue().startsWith("3.")) {
            compatibilityVersion = null;
        } else if ("2.0".equals(aspnetCoreVersion.getOptValue())) {
            compatibilityVersion = null;
        } else {
            // default, do nothing
            compatibilityVersion = "Version_" + aspnetCoreVersion.getOptValue().replace(".", "_");
        }
        LOGGER.info("ASP.NET core version: " + aspnetCoreVersion.getOptValue());
        embeddedTemplateDir = "aspnetcore/" + determineTemplateVersion(aspnetCoreVersion.getOptValue());
        additionalProperties.put(COMPATIBILITY_VERSION, compatibilityVersion);
    }

    private String determineTemplateVersion(String frameworkVersion) {
        switch (frameworkVersion) {
            case "3.1":
                return "3.0";

            case "2.2":
                return "2.1";

            default:
                return frameworkVersion;
        }
    }

    private void setUseSwashbuckle() {
        if (isLibrary) {
            LOGGER.warn("buildTarget is " + buildTarget.getOptValue() + " so changing default isLibrary to false ");
            useSwashbuckle = false;
        } else {
            useSwashbuckle = true;
        }
        if (additionalProperties.containsKey(USE_SWASHBUCKLE)) {
            useSwashbuckle = convertPropertyToBooleanAndWriteBack(USE_SWASHBUCKLE);
        } else {
            additionalProperties.put(USE_SWASHBUCKLE, useSwashbuckle);
        }
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

    private void setIsFramework() {
        if (aspnetCoreVersion.getOptValue().startsWith("3.")) {// default, do nothing
            LOGGER.warn("ASP.NET core version is " + aspnetCoreVersion.getOptValue() + " so changing to use frameworkReference instead of packageReference ");
            useFrameworkReference = true;
            additionalProperties.put(USE_FRAMEWORK_REFERENCE, useFrameworkReference);
        } else {
            if (additionalProperties.containsKey(USE_FRAMEWORK_REFERENCE)) {
                useFrameworkReference = convertPropertyToBooleanAndWriteBack(USE_FRAMEWORK_REFERENCE);
            } else {
                additionalProperties.put(USE_FRAMEWORK_REFERENCE, useFrameworkReference);
            }
        }
    }

    private void setUseNewtonsoft() {
        if (aspnetCoreVersion.getOptValue().startsWith("2.")) {
            LOGGER.warn("ASP.NET core version is " + aspnetCoreVersion.getOptValue() + " so staying on default json library.");
            useNewtonsoft = false;
            additionalProperties.put(USE_NEWTONSOFT, useNewtonsoft);
        } else {
            if (additionalProperties.containsKey(USE_NEWTONSOFT)) {
                useNewtonsoft = convertPropertyToBooleanAndWriteBack(USE_NEWTONSOFT);
            } else {
                additionalProperties.put(USE_NEWTONSOFT, useNewtonsoft);
            }
        }
    }

    private void setUseEndpointRouting() {
        if (aspnetCoreVersion.getOptValue().startsWith("3.")) {
            LOGGER.warn("ASP.NET core version is " + aspnetCoreVersion.getOptValue() + " so switching to old style endpoint routing.");
            useDefaultRouting = false;
            additionalProperties.put(USE_DEFAULT_ROUTING, useDefaultRouting);
        } else {
            if (additionalProperties.containsKey(USE_DEFAULT_ROUTING)) {
                useDefaultRouting = convertPropertyToBooleanAndWriteBack(USE_DEFAULT_ROUTING);
            } else {
                additionalProperties.put(USE_DEFAULT_ROUTING, useDefaultRouting);
            }
        }
    }

    private void setSwashbuckleVersion() {
        setCliOption(swashbuckleVersion);

        if (aspnetCoreVersion.getOptValue().startsWith("3.")) {
            LOGGER.warn("ASP.NET core version is " + aspnetCoreVersion.getOptValue() + " so changing default Swashbuckle version to 5.0.0.");
            swashbuckleVersion.setOptValue("5.0.0");
            additionalProperties.put(SWASHBUCKLE_VERSION, swashbuckleVersion.getOptValue());
        } else {
            // default, do nothing
            LOGGER.info("Swashbuckle version: " + swashbuckleVersion.getOptValue());
        }
    }
}
