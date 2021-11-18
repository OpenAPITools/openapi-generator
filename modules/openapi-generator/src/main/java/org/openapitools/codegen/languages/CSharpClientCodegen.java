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

import com.google.common.collect.ImmutableMap;
import io.swagger.models.Model;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.text.WordUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.AbstractCSharpCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CSharpClientCodegen extends AbstractCSharpCodegen {
    @SuppressWarnings({"unused", "hiding"})
    private static final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);
    private static final String NET45 = "v4.5";
    private static final String NET35 = "v3.5";
    private static final String UWP = "uwp";
    private static final String DATA_TYPE_WITH_ENUM_EXTENSION = "plainDatatypeWithEnum";

    protected String packageGuid = "{" + java.util.UUID.randomUUID().toString().toUpperCase(Locale.getDefault()) + "}";
    protected String packageTitle = "Swagger Library";
    protected String packageProductName = "SwaggerLibrary";
    protected String packageDescription = "A library generated from a Swagger doc";
    protected String packageCompany = "Swagger";
    protected String packageCopyright = "No Copyright";
    protected String clientPackage = "IO.Swagger.Client";
    protected String localVariablePrefix = "";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    protected String targetFramework = NET45;
    protected String targetFrameworkNuget = "net45";
    protected boolean supportsAsync = Boolean.TRUE;
    protected boolean supportsUWP = Boolean.FALSE;


    protected final Map<String, String> frameworks;

    public CSharpClientCodegen() {
        super();
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");

        modelTestTemplateFiles.put("model_test.mustache", ".cs");
        apiTestTemplateFiles.put("api_test.mustache", ".cs");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        cliOptions.clear();

        // CLI options
        addOption(CodegenConstants.PACKAGE_NAME,
                "C# package name (convention: Title.Case).",
                this.packageName);

        addOption(CodegenConstants.PACKAGE_VERSION,
                "C# package version.",
                this.packageVersion);

        addOption(CodegenConstants.PACKAGE_TITLE,
                "Package's title",
                null);

        addOption(CodegenConstants.PACKAGE_DESCRIPTION,
                "Package's description",
                null);

        addOption(CodegenConstants.PACKAGE_CONFIGURATION,
                "Package's configuration",
                null);

        addOption(CodegenConstants.PACKAGE_COMPANY,
                "Package's company",
                null);

        addOption(CodegenConstants.PACKAGE_PRODUCT_NAME,
                "Package's product name",
                null);

        addOption(CodegenConstants.PACKAGE_COPYRIGHT,
                "Package's copyright information",
                null);

        addOption(CodegenConstants.PACKAGE_TRADEMARK,
                "Package's trademark information",
                null);

        addOption(CodegenConstants.PACKAGE_CULTURE,
                "Package's culture string (e.g. en-us)",
                null);

        addOption(CodegenConstants.SOURCE_FOLDER,
                CodegenConstants.SOURCE_FOLDER_DESC,
                sourceFolder);

        addOption(CodegenConstants.OPTIONAL_PROJECT_GUID,
                CodegenConstants.OPTIONAL_PROJECT_GUID_DESC,
                null);

        CliOption framework = new CliOption(
                CodegenConstants.DOTNET_FRAMEWORK,
                CodegenConstants.DOTNET_FRAMEWORK_DESC
        );
        frameworks = new ImmutableMap.Builder<String, String>()
                .put(NET35, ".NET Framework 3.5 compatible")
                .put(NET45, ".NET Framework 4.5+ compatible")
                .put(UWP, "Universal Windows Platform - beta support")
                .build();
        framework.defaultValue(this.targetFramework);
        framework.setEnum(frameworks);
        cliOptions.add(framework);

        // CLI Switches
        addSwitch(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC,
                this.sortParamsByRequiredFlag);

        addSwitch(CodegenConstants.USE_DATETIME_OFFSET,
                CodegenConstants.USE_DATETIME_OFFSET_DESC,
                this.useDateTimeOffsetFlag);

        addSwitch(CodegenConstants.USE_COLLECTION,
                CodegenConstants.USE_COLLECTION_DESC,
                this.useCollection);

        addSwitch(CodegenConstants.RETURN_ICOLLECTION,
                CodegenConstants.RETURN_ICOLLECTION_DESC,
                this.returnICollection);

        addSwitch(CodegenConstants.OPTIONAL_METHOD_ARGUMENT,
                "C# Optional method argument, e.g. void square(int x=10) (.net 4.0+ only).",
                this.optionalMethodArgumentFlag);

        addSwitch(CodegenConstants.OPTIONAL_ASSEMBLY_INFO,
                CodegenConstants.OPTIONAL_ASSEMBLY_INFO_DESC,
                this.optionalAssemblyInfoFlag);

        addSwitch(CodegenConstants.OPTIONAL_PROJECT_FILE,
                CodegenConstants.OPTIONAL_PROJECT_FILE_DESC,
                this.optionalProjectFileFlag);

        addSwitch(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES,
                CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES_DESC,
                this.optionalEmitDefaultValue);
    }

    @Override
    public void processOpts() {
        super.processOpts();
        Boolean excludeTests = false;

        if(additionalProperties.containsKey(CodegenConstants.EXCLUDE_TESTS)) {
            excludeTests = Boolean.valueOf(additionalProperties.get(CodegenConstants.EXCLUDE_TESTS).toString());
        }

        apiPackage = "Api";
        modelPackage = "Model";
        clientPackage = "Client";

        additionalProperties.put("clientPackage", clientPackage);

        // Add properties used by AssemblyInfo.mustache
        // {{packageTitle}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_TITLE)) {
            packageTitle = (String) additionalProperties.get(CodegenConstants.PACKAGE_TITLE);
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_TITLE, packageTitle);
        }
        // {{packageProductName}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_PRODUCT_NAME)) {
            packageProductName = (String) additionalProperties.get(CodegenConstants.PACKAGE_PRODUCT_NAME);
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_PRODUCT_NAME, packageProductName);
        }
        // {{packageDescription}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_DESCRIPTION)) {
            packageDescription = (String) additionalProperties.get(CodegenConstants.PACKAGE_DESCRIPTION);
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_DESCRIPTION, packageDescription);
        }
        // {{packageCompany}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_COMPANY)) {
            packageCompany = (String) additionalProperties.get(CodegenConstants.PACKAGE_COMPANY);
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_COMPANY, packageCompany);
        }
        // {{packageCopyright}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_COPYRIGHT)) {
            packageCopyright = (String) additionalProperties.get(CodegenConstants.PACKAGE_COPYRIGHT);
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_COPYRIGHT, packageCopyright);
        }

        additionalProperties.put("emitDefaultValue", optionalEmitDefaultValue);

        if (additionalProperties.containsKey(CodegenConstants.DOTNET_FRAMEWORK)) {
            setTargetFramework((String) additionalProperties.get(CodegenConstants.DOTNET_FRAMEWORK));
        } else {
            // Ensure default is set.
            setTargetFramework(NET45);
            additionalProperties.put("targetFramework", this.targetFramework);
        }

        if (NET35.equals(this.targetFramework)) {
            setTargetFrameworkNuget("net35");
            setSupportsAsync(Boolean.FALSE);
            if(additionalProperties.containsKey("supportsAsync")){
                additionalProperties.remove("supportsAsync");
            }
        } else if (UWP.equals(this.targetFramework)){
            setTargetFrameworkNuget("uwp");
            setSupportsAsync(Boolean.TRUE);
            setSupportsUWP(Boolean.TRUE);
            additionalProperties.put("supportsAsync", this.supportsUWP);
            additionalProperties.put("supportsUWP", this.supportsAsync);

        } else {
            setTargetFrameworkNuget("net45");
            setSupportsAsync(Boolean.TRUE);
            additionalProperties.put("supportsAsync", this.supportsAsync);
        }

        additionalProperties.put("targetFrameworkNuget", this.targetFrameworkNuget);

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_FILE)) {
            setOptionalProjectFileFlag(Boolean.parseBoolean(
                    additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_FILE).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_GUID)) {
            setPackageGuid((String) additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_GUID));
        }
        additionalProperties.put("packageGuid", packageGuid);

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_METHOD_ARGUMENT)) {
            setOptionalMethodArgumentFlag(Boolean.parseBoolean(additionalProperties
                    .get(CodegenConstants.OPTIONAL_METHOD_ARGUMENT).toString()));
        }
        additionalProperties.put("optionalMethodArgument", optionalMethodArgumentFlag);

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_ASSEMBLY_INFO)) {
            setOptionalAssemblyInfoFlag(Boolean.parseBoolean(additionalProperties
                    .get(CodegenConstants.OPTIONAL_ASSEMBLY_INFO).toString()));
        }

        final String testPackageName = testPackageName();
        String packageFolder = sourceFolder + File.separator + packageName;
        String clientPackageDir = packageFolder + File.separator + clientPackage;
        String testPackageFolder = testFolder + File.separator + testPackageName;

        additionalProperties.put("testPackageName", testPackageName);

        //Compute the relative path to the bin directory where the external assemblies live
        //This is necessary to properly generate the project file
        int packageDepth = packageFolder.length() - packageFolder.replace(java.io.File.separator, "").length();
        String binRelativePath = "..\\";
        for (int i = 0; i < packageDepth; i = i + 1)
            binRelativePath += "..\\";
        binRelativePath += "vendor";
        additionalProperties.put("binRelativePath", binRelativePath);

        supportingFiles.add(new SupportingFile("IApiAccessor.mustache",
                clientPackageDir, "IApiAccessor.cs"));
        supportingFiles.add(new SupportingFile("Configuration.mustache",
                clientPackageDir, "Configuration.cs"));
        supportingFiles.add(new SupportingFile("Logger.mustache",
                clientPackageDir, "Logger.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                clientPackageDir, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse.mustache",
                clientPackageDir, "ApiResponse.cs"));

        supportingFiles.add(new SupportingFile("compile.mustache", "", "build.bat"));
        supportingFiles.add(new SupportingFile("compile-mono.sh.mustache", "", "build.sh"));

        // copy package.config to nuget's standard location for project-level installs
        supportingFiles.add(new SupportingFile("packages.config.mustache", packageFolder + File.separator, "packages.config"));

        if(Boolean.FALSE.equals(excludeTests)) {
            supportingFiles.add(new SupportingFile("packages_test.config.mustache", testPackageFolder + File.separator, "packages.config"));
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        if (optionalAssemblyInfoFlag) {
            supportingFiles.add(new SupportingFile("AssemblyInfo.mustache", packageFolder + File.separator + "Properties", "AssemblyInfo.cs"));
        }
        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
            supportingFiles.add(new SupportingFile("Project.mustache", packageFolder, packageName + ".csproj"));

            if(Boolean.FALSE.equals(excludeTests)) {
                supportingFiles.add(new SupportingFile("TestProject.mustache", testPackageFolder, testPackageName + ".csproj"));
            }
        }

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        if (objs != null) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    if (operation.returnType != null) {
                        operation.returnContainer = operation.returnType;
                        if (this.returnICollection && (
                                operation.returnType.startsWith("List") ||
                                        operation.returnType.startsWith("Collection"))) {
                            // NOTE: ICollection works for both List<T> and Collection<T>
                            int genericStart = operation.returnType.indexOf("<");
                            if (genericStart > 0) {
                                operation.returnType = "ICollection" + operation.returnType.substring(genericStart);
                            }
                        }
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "csharp";
    }

    @Override
    public String getHelp() {
        return "Generates a CSharp client library.";
    }

    public void setOptionalAssemblyInfoFlag(boolean flag) {
        this.optionalAssemblyInfoFlag = flag;
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema) {
        CodegenModel codegenModel = super.fromModel(name, schema);
        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        if (allDefinitions != null && codegenModel != null && codegenModel.parent != null && codegenModel.hasEnums) {
            final Schema parentModel = allDefinitions.get(toModelName(codegenModel.parent));
            final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent, parentModel);
            codegenModel = this.reconcileInlineEnums(codegenModel, parentCodegenModel);
        }

        return codegenModel;
    }

    public void setOptionalProjectFileFlag(boolean flag) {
        this.optionalProjectFileFlag = flag;
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    public void setTargetFramework(String dotnetFramework) {
        if(!frameworks.containsKey(dotnetFramework)){
            LOGGER.warn("Invalid .NET framework version, defaulting to " + this.targetFramework);
        } else {
            this.targetFramework = dotnetFramework;
        }
        LOGGER.info("Generating code for .NET Framework " + this.targetFramework);
    }

    private CodegenModel reconcileInlineEnums(CodegenModel codegenModel, CodegenModel parentCodegenModel) {
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

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("int?".equalsIgnoreCase(datatype) || "long?".equalsIgnoreCase(datatype) ||
                "double?".equalsIgnoreCase(datatype) || "float?".equalsIgnoreCase(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        // number
        if ("int?".equals(datatype) || "long?".equals(datatype) ||
                "double?".equals(datatype) || "float?".equals(datatype)) {
            String varName = "NUMBER_" + value;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String var = value.replaceAll("_", " ");
        var = WordUtils.capitalizeFully(var);
        var = var.replaceAll("\\W+", "");


        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }


    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setTargetFrameworkNuget(String targetFrameworkNuget) {
        this.targetFrameworkNuget = targetFrameworkNuget;
    }

    public void setSupportsAsync(Boolean supportsAsync){
        this.supportsAsync = supportsAsync;
    }

    public void setSupportsUWP(Boolean supportsUWP){
        this.supportsUWP = supportsUWP;
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelFilename(name);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + testPackageName() + File.separator + apiPackage();
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + testPackageName() + File.separator + modelPackage();
    }
}
