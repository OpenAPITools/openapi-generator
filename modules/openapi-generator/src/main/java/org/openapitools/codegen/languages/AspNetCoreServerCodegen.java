/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
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
    public static final String CLASS_MODIFIER = "classModifier";
    public static final String OPERATION_MODIFIER = "operationModifier";
    public static final String GENERATE_BODY = "generateBody";
    public static final String BUILD_TARGET = "buildTarget";

    public static final String PROJECT_SDK = "projectSdk";
    public static final String SDK_WEB = "Microsoft.NET.Sdk.Web";
    public static final String SDK_LIB = "Microsoft.NET.Sdk";

    private String packageGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";

    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(AspNetCoreServerCodegen.class);

    private boolean useSwashbuckle = true;
    protected int serverPort = 8080;
    protected String serverHost = "0.0.0.0";
    protected String aspnetCoreVersion= "2.1"; // default to 2.1
    // TODO Make next two enums toensure fixed list.
    private String classModifier = "";
    private String operationModifier = "virtual";
    private boolean generateBody = true;
    private String buildTarget = "program";
    private String projectSdk = SDK_WEB;

    public AspNetCoreServerCodegen() {
        super();

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

        addOption(ASPNET_CORE_VERSION,
                "ASP.NET Core version: 2.1 (default), 2.0 (deprecated)",
                aspnetCoreVersion);

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

        addOption(CLASS_MODIFIER,
                "Class modifiers such as abstract or partial",
                classModifier);

        addOption(OPERATION_MODIFIER,
                "Operation modifiers such as virtual or abstract.",
                operationModifier);

        addSwitch(GENERATE_BODY,
                "Generates method body.",
                generateBody);

        addOption(BUILD_TARGET,
                "Target the build for a program or library.",
                buildTarget);

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
        URL url = URLPathUtils.getServerURL(openAPI);
        additionalProperties.put("serverHost", url.getHost());
        additionalProperties.put("serverPort", URLPathUtils.getPort(url, 8080));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        boolean isLibrary = false;

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_GUID)) {
            setPackageGuid((String) additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_GUID));
        }
        additionalProperties.put("packageGuid", packageGuid);

        if (additionalProperties.containsKey(USE_SWASHBUCKLE)) {
            useSwashbuckle = convertPropertyToBooleanAndWriteBack(USE_SWASHBUCKLE);
        } else {
            additionalProperties.put(USE_SWASHBUCKLE, useSwashbuckle);
        }

        // determine the ASP.NET core version setting
        if (additionalProperties.containsKey(ASPNET_CORE_VERSION)) {
            setAspnetCoreVersion((String) additionalProperties.get(ASPNET_CORE_VERSION));
        }

        // CHeck for class modifier if not present set the default value.
        if (additionalProperties.containsKey(CLASS_MODIFIER)) {
             classModifier = additionalProperties.get(CLASS_MODIFIER).toString();
        } else {
            additionalProperties.put(CLASS_MODIFIER, classModifier);
        }

        // TODO Validate modifier values
        // If class modifierier is abstract then the methods need to be abstrat too.
        if ("abstract".equals(classModifier)) {
            operationModifier = classModifier;
            additionalProperties.put(OPERATION_MODIFIER, operationModifier);
        }

        if (additionalProperties.containsKey(OPERATION_MODIFIER)) {
            operationModifier = additionalProperties.get(OPERATION_MODIFIER).toString();
        } else {
            additionalProperties.put(OPERATION_MODIFIER, operationModifier);
        }

        // TODO Validate modifier values
        // If operation modifier is abstract then dont generate any body
        if ("abstract".equals(operationModifier)) {
            generateBody = false;
            additionalProperties.put(GENERATE_BODY, generateBody);
        }
        if (additionalProperties.containsKey(GENERATE_BODY)) {
            generateBody = convertPropertyToBooleanAndWriteBack(GENERATE_BODY);
        } else {
            additionalProperties.put(GENERATE_BODY, generateBody);
        }

        // CHeck for class modifier if not present set the default value.
        if (additionalProperties.containsKey(BUILD_TARGET)) {
            buildTarget = additionalProperties.get(BUILD_TARGET).toString();
        } else {
            additionalProperties.put(BUILD_TARGET, buildTarget);
        }
        if ("library".equals(buildTarget)) {
            isLibrary = true;
            projectSdk = SDK_LIB;
        }
        additionalProperties.put(PROJECT_SDK, projectSdk);

        additionalProperties.put("dockerTag", packageName.toLowerCase(Locale.ROOT));

        apiPackage = packageName + ".Controllers";
        modelPackage = packageName + ".Models";

        String packageFolder = sourceFolder + File.separator + packageName;

        if ("2.0".equals(aspnetCoreVersion)) {
            embeddedTemplateDir = templateDir = "aspnetcore/2.0";
            supportingFiles.add(new SupportingFile("web.config", packageFolder, "web.config"));
            LOGGER.info("ASP.NET core version: 2.0");
        } else if ("2.1".equals(aspnetCoreVersion)) {
            // default, do nothing
            LOGGER.info("ASP.NET core version: 2.1");
        } else {
            throw new IllegalArgumentException("aspnetCoreVersion must be '2.1', '2.0' but found " + aspnetCoreVersion);
        }


        supportingFiles.add(new SupportingFile("build.sh.mustache", "", "build.sh"));
        supportingFiles.add(new SupportingFile("build.bat.mustache", "", "build.bat"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
        supportingFiles.add(new SupportingFile("gitignore", packageFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("validateModel.mustache", packageFolder + File.separator + "Attributes", "ValidateModelStateAttribute.cs"));
        supportingFiles.add(new SupportingFile("Project.csproj.mustache", packageFolder, packageName + ".csproj"));
        if (!isLibrary) {
            supportingFiles.add(new SupportingFile("Dockerfile.mustache", packageFolder, "Dockerfile"));
            supportingFiles.add(new SupportingFile("appsettings.json", packageFolder, "appsettings.json"));

            supportingFiles.add(new SupportingFile("Startup.mustache", packageFolder, "Startup.cs"));
            supportingFiles.add(new SupportingFile("Program.mustache", packageFolder, "Program.cs"));
            supportingFiles.add(new SupportingFile("Properties" + File.separator + "launchSettings.json",
                    packageFolder + File.separator + "Properties", "launchSettings.json"));
        } else {
            supportingFiles.add(new SupportingFile("Project.nuspec.mustache", packageFolder, packageName + ".nuspec"));
            // wwwroot files.
            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "README.md", packageFolder + File.separator + "wwwroot", "README.md"));
            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "index.html", packageFolder + File.separator + "wwwroot", "index.html"));
            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "web.config", packageFolder + File.separator + "wwwroot", "web.config"));

            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "openapi-original.mustache",
                    packageFolder + File.separator + "wwwroot", "openapi-original.json"));
        }
        supportingFiles.add(new SupportingFile("validateModel.mustache", packageFolder + File.separator + "Attributes", "ValidateModelStateAttribute.cs"));
        supportingFiles.add(new SupportingFile("Project.csproj.mustache", packageFolder, packageName + ".csproj"));
        if (!isLibrary) {
            supportingFiles.add(new SupportingFile("Dockerfile.mustache", packageFolder, "Dockerfile"));
            supportingFiles.add(new SupportingFile("appsettings.json", packageFolder, "appsettings.json"));

            supportingFiles.add(new SupportingFile("Startup.mustache", packageFolder, "Startup.cs"));
            supportingFiles.add(new SupportingFile("Program.mustache", packageFolder, "Program.cs"));
            supportingFiles.add(new SupportingFile("Properties" + File.separator + "launchSettings.json",
                    packageFolder + File.separator + "Properties", "launchSettings.json"));
        } else {
            supportingFiles.add(new SupportingFile("Project.nuspec.mustache", packageFolder, packageName + ".nuspec"));
            // wwwroot files.
            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "README.md", packageFolder + File.separator + "wwwroot", "README.md"));
            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "index.html", packageFolder + File.separator + "wwwroot", "index.html"));
            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "web.config", packageFolder + File.separator + "wwwroot", "web.config"));

            supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "openapi-original.mustache",
                    packageFolder + File.separator + "wwwroot", "openapi-original.json"));
        }


        if (useSwashbuckle) {
            supportingFiles.add(new SupportingFile("Filters" + File.separator + "BasePathFilter.mustache",
                    packageFolder + File.separator + "Filters", "BasePathFilter.cs"));
            supportingFiles.add(new SupportingFile("Filters" + File.separator + "GeneratePathParamsValidationFilter.mustache",
                    packageFolder + File.separator + "Filters", "GeneratePathParamsValidationFilter.cs"));
        }
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    public void setAspnetCoreVersion(String aspnetCoreVersion) {
        this.aspnetCoreVersion= aspnetCoreVersion;
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
        boolean isNullableExpected = p.getNullable() == null || (p.getNullable() != null && p.getNullable());

        if (isNullableExpected && languageSpecificPrimitives.contains(type + "?")) {
            return type + "?";
        } else if (languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return null;
        }
    }
}
