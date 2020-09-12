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

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.Locale;
import java.util.Map;

import static java.util.UUID.randomUUID;

public class FsharpGiraffeServerCodegen extends AbstractFSharpCodegen {

    public static final String USE_SWASHBUCKLE = "useSwashbuckle";
    public static final String GENERATE_BODY = "generateBody";
    public static final String BUILD_TARGET = "buildTarget";

    public static final String PROJECT_SDK = "projectSdk";
    public static final String SDK_WEB = "Microsoft.NET.Sdk.Web";
    public static final String SDK_LIB = "Microsoft.NET.Sdk";

    private String packageGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";

    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(FsharpGiraffeServerCodegen.class);

    private boolean useSwashbuckle = false;
    protected int serverPort = 8080;
    protected String serverHost = "0.0.0.0";
    private boolean generateBody = true;
    private String buildTarget = "program";
    private String projectSdk = SDK_WEB;

    public FsharpGiraffeServerCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        modelPackage = "Model";

        apiTemplateFiles.put("Handler.mustache", "Handler.fs");
        apiTemplateFiles.put("HandlerParams.mustache", "HandlerParams.fs");
        apiTemplateFiles.put("ServiceInterface.mustache", "ServiceInterface.fs");
        apiTemplateFiles.put("ServiceImpl.mustache", "Service.fs");
        apiTestTemplateFiles.put("HandlerTests.mustache", ".fs");
        apiTestTemplateFiles.put("HandlerTestsHelper.mustache", "Helper.fs");
        modelTemplateFiles.put("Model.mustache", ".fs");

        embeddedTemplateDir = templateDir = "fsharp-giraffe-server";

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
                "F# module name (convention: Title.Case).",
                packageName);

        addOption(CodegenConstants.PACKAGE_VERSION,
                "F# package version.",
                packageVersion);

        addOption(CodegenConstants.OPTIONAL_PROJECT_GUID,
                CodegenConstants.OPTIONAL_PROJECT_GUID_DESC,
                null);

        addOption(CodegenConstants.SOURCE_FOLDER,
                CodegenConstants.SOURCE_FOLDER_DESC,
                sourceFolder);

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
        return "fsharp-giraffe-server";
    }

    @Override
    public String getHelp() {
        return "Generates a F# Giraffe server (beta).";
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

        additionalProperties.put(PROJECT_SDK, projectSdk);

        // TODO - should we be supporting a Giraffe class library?
        if (isLibrary)
            LOGGER.warn("Library flag not currently supported.");

        String authFolder = sourceFolder + File.separator + "auth";
        String implFolder = sourceFolder + File.separator + "impl";
        String helperFolder = sourceFolder + File.separator + "helpers";

        supportingFiles.add(new SupportingFile("build.sh.mustache", projectFolder, "build.sh"));
        supportingFiles.add(new SupportingFile("build.bat.mustache", projectFolder, "build.bat"));
        supportingFiles.add(new SupportingFile("README.mustache", projectFolder, "README.md"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", projectFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("Project.fsproj.mustache", sourceFolder, packageName + ".fsproj"));
        supportingFiles.add(new SupportingFile("Program.mustache", sourceFolder, "Program.fs"));
        supportingFiles.add(new SupportingFile("AuthSchemes.mustache", authFolder, "AuthSchemes.fs"));
        supportingFiles.add(new SupportingFile("Helpers.mustache", helperFolder, "Helpers.fs"));
        supportingFiles.add(new SupportingFile("CustomHandlers.mustache", implFolder, "CustomHandlers.fs"));
        supportingFiles.add(new SupportingFile("Project.Tests.fsproj.mustache", testFolder, packageName + "Tests.fsproj"));
        supportingFiles.add(new SupportingFile("TestHelper.mustache", testFolder, "TestHelper.fs"));

        // TODO - support Swashbuckle
        if (useSwashbuckle)
            LOGGER.warn("Swashbuckle flag not currently supported, this will be ignored.");
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    @Override
    public String modelFileFolder() {
        return super.modelFileFolder().replace("Model", "model");
    }

    @Override
    public String apiFileFolder() {
        return super.apiFileFolder() + File.separator + "api";
    }

    private String implFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "impl";
    }

    @Override()
    public String toModelImport(String name) {
        return packageName + "." + modelPackage() + "." + name;
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);
        if (templateName.endsWith("Impl.mustache")) {
            int ix = result.lastIndexOf(File.separatorChar);
            result = result.substring(0, ix) + result.substring(ix, result.length() - 2) + "fs";
            result = result.replace(apiFileFolder(), implFileFolder());
        }
        return result;
    }


    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateJSONSpecFile(objs);
        generateYAMLSpecFile(objs);
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
}