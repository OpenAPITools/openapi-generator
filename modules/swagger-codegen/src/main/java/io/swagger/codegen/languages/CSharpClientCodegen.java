package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.models.properties.*;
import io.swagger.codegen.CliOption;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CSharpClientCodegen extends AbstractCSharpCodegen {
    @SuppressWarnings({"unused", "hiding"})
    private static final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);

    protected String packageGuid = "{" + java.util.UUID.randomUUID().toString().toUpperCase() + "}";
    protected String packageTitle = "Swagger Library";
    protected String packageProductName = "SwaggerLibrary";
    protected String packageDescription = "A library generated from a Swagger doc";
    protected String packageCompany = "Swagger";
    protected String packageCopyright = "No Copyright";
    protected String clientPackage = "IO.Swagger.Client";

    public CSharpClientCodegen() {
        super();
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");

        modelTestTemplateFiles.put("model_test.mustache", ".cs");
        apiTestTemplateFiles.put("api_test.mustache", ".cs");

        // C# client default
        setSourceFolder("src" + File.separator + "main" + File.separator + "csharp");

        cliOptions.clear();

        // CLI options
        addOption(CodegenConstants.PACKAGE_NAME,
                "C# package name (convention: Title.Case).",
                this.packageName);

        addOption(CodegenConstants.PACKAGE_VERSION,
                "C# package version.",
                this.packageVersion);

        addOption(CodegenConstants.SOURCE_FOLDER,
                CodegenConstants.SOURCE_FOLDER_DESC,
                sourceFolder);

        addOption(CodegenConstants.OPTIONAL_PROJECT_GUID,
                CodegenConstants.OPTIONAL_PROJECT_GUID_DESC,
                null);

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
    }

    @Override
    public void processOpts() {
        super.processOpts();

        apiPackage = packageName + ".Api";
        modelPackage = packageName + ".Model";
        clientPackage = packageName + ".Client";

        additionalProperties.put("clientPackage", clientPackage);

        // Add properties used by AssemblyInfo.mustache
        additionalProperties.put("packageTitle", packageTitle);
        additionalProperties.put("packageProductName", packageProductName);
        additionalProperties.put("packageDescription", packageDescription);
        additionalProperties.put("packageCompany", packageCompany);
        additionalProperties.put("packageCopyright", packageCopyright);

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_FILE)) {
            setOptionalProjectFileFlag(Boolean.valueOf(
                    additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_FILE).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_GUID)) {
            setPackageGuid((String) additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_GUID));
        }
        additionalProperties.put("packageGuid", packageGuid);

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_METHOD_ARGUMENT)) {
            setOptionalMethodArgumentFlag(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.OPTIONAL_METHOD_ARGUMENT).toString()));
        }
        additionalProperties.put("optionalMethodArgument", optionalMethodArgumentFlag);

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_ASSEMBLY_INFO)) {
            setOptionalAssemblyInfoFlag(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.OPTIONAL_ASSEMBLY_INFO).toString()));
        }

        String packageFolder = sourceFolder + File.separator + packageName.replace(".", java.io.File.separator);
        String clientPackageDir = sourceFolder + File.separator + clientPackage.replace(".", java.io.File.separator);

        //Compute the relative path to the bin directory where the external assemblies live
        //This is necessary to properly generate the project file
        int packageDepth = packageFolder.length() - packageFolder.replace(java.io.File.separator, "").length();
        String binRelativePath = "..\\";
        for (int i = 0; i < packageDepth; i = i + 1)
            binRelativePath += "..\\";
        binRelativePath += "bin\\";
        additionalProperties.put("binRelativePath", binRelativePath);

        supportingFiles.add(new SupportingFile("Configuration.mustache",
                clientPackageDir, "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                clientPackageDir, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse.mustache",
                clientPackageDir, "ApiResponse.cs"));

        supportingFiles.add(new SupportingFile("Newtonsoft.Json.dll", "bin", "Newtonsoft.Json.dll"));
        supportingFiles.add(new SupportingFile("RestSharp.dll", "bin", "RestSharp.dll"));
        supportingFiles.add(new SupportingFile("compile.mustache", "", "compile.bat"));
        supportingFiles.add(new SupportingFile("compile-mono.sh.mustache", "", "compile-mono.sh"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", "vendor" + java.io.File.separator, "packages.config"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));

        if (optionalAssemblyInfoFlag) {
            supportingFiles.add(new SupportingFile("AssemblyInfo.mustache", packageFolder + File.separator + "Properties", "AssemblyInfo.cs"));
        }
        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Project.mustache", packageFolder, clientPackage + ".csproj"));
        }
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        super.postProcessOperations(objs);
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

    public void setOptionalProjectFileFlag(boolean flag) {
        this.optionalProjectFileFlag = flag;
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

}
