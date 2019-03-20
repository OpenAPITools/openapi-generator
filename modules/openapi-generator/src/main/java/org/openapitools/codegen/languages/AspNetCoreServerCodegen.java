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
import org.openapitools.codegen.*;
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
    public static final String COMPATIBILITY_VERSION = "compatibilityVersion";

    private String packageGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";

    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(AspNetCoreServerCodegen.class);

    private boolean useSwashbuckle = true;
    protected int serverPort = 8080;
    protected String serverHost = "0.0.0.0";
    protected CliOption aspnetCoreVersion= new CliOption(ASPNET_CORE_VERSION,"ASP.NET Core version: 2.2 (default), 2.1, 2.0 (deprecated)");; // default to 2.1
    private CliOption classModifier = new CliOption(CLASS_MODIFIER,"Class Modifier can be empty, abstract");
    private CliOption operationModifier = new CliOption(OPERATION_MODIFIER, "Operation Modifier can be virtual, abstract or partial");
    private boolean generateBody = true;
    private CliOption buildTarget = new CliOption("buildTarget", "Target to build an application or library");
    private String projectSdk = SDK_WEB;
    private String compatibilityVersion = "Version_2_1";

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

        aspnetCoreVersion.addEnum("2.0", "ASP.NET COre V2.0");
        aspnetCoreVersion.addEnum("2.1", "ASP.NET COre V2.1");
        aspnetCoreVersion.addEnum("2.2", "ASP.NET COre V2.2");
        aspnetCoreVersion.setDefault("2.2");
        aspnetCoreVersion.setOptValue(aspnetCoreVersion.getDefault());
        addOption(aspnetCoreVersion.getOpt(),aspnetCoreVersion.getDescription(),aspnetCoreVersion.getOptValue());

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

        classModifier.addEnum("", "Keep class default with no modifier");
        classModifier.addEnum("abstract", "Make class abstract");
        classModifier.setDefault("");
        classModifier.setOptValue(classModifier.getDefault());
        addOption(classModifier.getOpt(),classModifier.getDescription(),classModifier.getOptValue());

        operationModifier.addEnum("virtual", "Keep method virtual ");
        operationModifier.addEnum("abstract", "Make method abstract");
        operationModifier.setDefault("virtual");
        operationModifier.setOptValue(operationModifier.getDefault());
        addOption(operationModifier.getOpt(),operationModifier.getDescription(),operationModifier.getOptValue());

        buildTarget.addEnum("program", "Generate code for standalone server");
        buildTarget.addEnum("library", "Generate code for a server abstract class lbrary");
        buildTarget.setDefault("program");
        buildTarget.setOptValue(buildTarget.getDefault());
        addOption(buildTarget.getOpt(),buildTarget.getDescription(),buildTarget.getOptValue());

        addSwitch(GENERATE_BODY,
                "Generates method body.",
                generateBody);

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


        // CHeck for the modifiers etc.
        // The order of the checks is important.
        isLibrary = setBuildTarget();
        setClassModifier();
        setOperationModifier();


        // CHeck for class modifier if not present set the default value.
        additionalProperties.put(PROJECT_SDK, projectSdk);

        additionalProperties.put("dockerTag", packageName.toLowerCase(Locale.ROOT));

        apiPackage = packageName + ".Controllers";
        modelPackage = packageName + ".Models";

        String packageFolder = sourceFolder + File.separator + packageName;

        // determine the ASP.NET core version setting
        setAspnetCoreVersion(packageFolder);

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

    private void  setCliOption(CliOption cliOption) throws IllegalArgumentException {
        if (additionalProperties.containsKey(cliOption.getOpt())) {
            cliOption.setOptValue(additionalProperties.get(cliOption.getOpt()).toString());
            if (classModifier.getOptValue() == null) {
                cliOption.setOptValue(cliOption.getDefault());
                throw new IllegalArgumentException(cliOption.getOpt() + ": Invalid value '" + additionalProperties.get(cliOption.getOpt()).toString() + "'" +
                        ". " + cliOption.getDescription());
            }
        } else {
            additionalProperties.put(cliOption.getOpt(), cliOption.getOptValue());
        }
    }

    private void setClassModifier() {
        // CHeck for class modifier if not present set the default value.
        setCliOption(classModifier);

        // If class modifier is abstract then the methods need to be abstract too.
        if ("abstract".equals(classModifier.getOptValue())) {
            operationModifier.setOptValue(classModifier.getOptValue());
            additionalProperties.put(OPERATION_MODIFIER, operationModifier.getOptValue());
            LOGGER.warn("classModifier is " + classModifier.getOptValue() + " so forcing operatonModifier to "+ operationModifier.getOptValue());
        } else {
            setCliOption(operationModifier);
        }
    }

    private void setOperationModifier() {
        setCliOption(operationModifier);

        // If operation modifier is abstract then dont generate any body
        if ("abstract".equals(operationModifier.getOptValue())) {
            generateBody = false;
            additionalProperties.put(GENERATE_BODY, generateBody);
            LOGGER.warn("operationModifier is " + operationModifier.getOptValue() + " so forcing generateBody to "+ generateBody);
        } else  if (additionalProperties.containsKey(GENERATE_BODY)) {
            generateBody = convertPropertyToBooleanAndWriteBack(GENERATE_BODY);
        } else {
            additionalProperties.put(GENERATE_BODY, generateBody);
        }
    }

    private boolean setBuildTarget() {
        boolean isLibrary = false;
        setCliOption(buildTarget);
        if ("library".equals(buildTarget.getOptValue())) {
            isLibrary = true;
            projectSdk = SDK_LIB;
            additionalProperties.put(CLASS_MODIFIER, "abstract");
        }
        return isLibrary;
    }

    private void setAspnetCoreVersion(String packageFolder) {
        setCliOption(aspnetCoreVersion);
        if ("2.0".equals(aspnetCoreVersion.getOptValue())) {
            embeddedTemplateDir = templateDir = "aspnetcore/2.0";
            supportingFiles.add(new SupportingFile("web.config", packageFolder, "web.config"));
            LOGGER.info("ASP.NET core version: 2.0");
            compatibilityVersion = null;
        } else {
            // default, do nothing
            LOGGER.info("ASP.NET core version: " + aspnetCoreVersion.getOptValue());
            compatibilityVersion = "Version_" + aspnetCoreVersion.getOptValue().replace(".","_");
        }
        additionalProperties.put(COMPATIBILITY_VERSION, compatibilityVersion);
    }
}
