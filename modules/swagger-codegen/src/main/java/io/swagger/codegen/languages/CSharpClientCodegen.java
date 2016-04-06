package io.swagger.codegen.languages;

import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.models.properties.*;
import io.swagger.codegen.CliOption;
import io.swagger.models.Model;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CSharpClientCodegen extends AbstractCSharpCodegen {
    @SuppressWarnings({"unused", "hiding"})
    private static final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);
    private static final String NET45 = "v4.5";
    private static final String NET35 = "v3.5";
    private static final String UWP = "uwp";
    private static final String DATA_TYPE_WITH_ENUM_EXTENSION = "plainDatatypeWithEnum";

    protected String packageGuid = "{" + java.util.UUID.randomUUID().toString().toUpperCase() + "}";
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
        additionalProperties.put("emitDefaultValue", optionalEmitDefaultValue);

        if (additionalProperties.containsKey(CodegenConstants.DOTNET_FRAMEWORK)) {
            setTargetFramework((String) additionalProperties.get(CodegenConstants.DOTNET_FRAMEWORK));
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
        binRelativePath += "vendor\\";
        additionalProperties.put("binRelativePath", binRelativePath);

        supportingFiles.add(new SupportingFile("Configuration.mustache",
                clientPackageDir, "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                clientPackageDir, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse.mustache",
                clientPackageDir, "ApiResponse.cs"));

        supportingFiles.add(new SupportingFile("compile.mustache", "", "compile.bat"));
        supportingFiles.add(new SupportingFile("compile-mono.sh.mustache", "", "compile-mono.sh"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", "vendor" + java.io.File.separator, "packages.config"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        if (optionalAssemblyInfoFlag) {
            supportingFiles.add(new SupportingFile("AssemblyInfo.mustache", packageFolder + File.separator + "Properties", "AssemblyInfo.cs"));
        }
        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Project.mustache", packageFolder, clientPackage + ".csproj"));
        }

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
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

    @Override
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);
        if (allDefinitions != null && codegenModel != null && codegenModel.parent != null && codegenModel.hasEnums) {
            final Model parentModel = allDefinitions.get(toModelName(codegenModel.parent));
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

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objMap) {
    	return super.postProcessModels(objMap);
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

}
