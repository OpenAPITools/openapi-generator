package io.swagger.codegen.languages;

import com.google.common.collect.ImmutableMap;

import com.sun.org.apache.bcel.internal.classfile.Code;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CliOption;
import io.swagger.models.Model;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.apache.commons.lang3.StringUtils.isEmpty;

public class CSharpClientCodegen extends AbstractCSharpCodegen {
    @SuppressWarnings({"unused", "hiding"})
    private static final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);
    private static final String NET45 = "v4.5";
    private static final String NET35 = "v3.5";
    private static final String NETSTANDARD = "v5.0";
    private static final String UWP = "uwp";
    private static final String DATA_TYPE_WITH_ENUM_EXTENSION = "plainDatatypeWithEnum";

    protected String packageGuid = "{" + java.util.UUID.randomUUID().toString().toUpperCase() + "}";
    protected String clientPackage = "IO.Swagger.Client";
    protected String localVariablePrefix = "";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    protected String targetFramework = NET45;
    protected String targetFrameworkNuget = "net45";
    protected boolean supportsAsync = Boolean.TRUE;
    protected boolean supportsUWP = Boolean.FALSE;
    protected boolean netStandard = Boolean.FALSE;
    protected boolean generatePropertyChanged = Boolean.FALSE;
    protected Map<Character, String> regexModifiers;
    protected final Map<String, String> frameworks;

    // By default, generated code is considered public
    protected boolean nonPublicApi = Boolean.FALSE;

    public CSharpClientCodegen() {
        super();
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");

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

        addOption(CodegenConstants.SOURCE_FOLDER,
                CodegenConstants.SOURCE_FOLDER_DESC,
                sourceFolder);

        addOption(CodegenConstants.OPTIONAL_PROJECT_GUID,
                CodegenConstants.OPTIONAL_PROJECT_GUID_DESC,
                null);

        addOption(CodegenConstants.INTERFACE_PREFIX,
                CodegenConstants.INTERFACE_PREFIX_DESC,
                interfacePrefix);

        CliOption framework = new CliOption(
                CodegenConstants.DOTNET_FRAMEWORK,
                CodegenConstants.DOTNET_FRAMEWORK_DESC
        );
        frameworks = new ImmutableMap.Builder<String, String>()
                .put(NET35, ".NET Framework 3.5 compatible")
                .put(NET45, ".NET Framework 4.5+ compatible")
                .put(NETSTANDARD, ".NET Standard 1.3 compatible")
                .put(UWP, "Universal Windows Platform (IMPORTANT: this will be decommissioned and replaced by v5.0)")
                .build();
        framework.defaultValue(this.targetFramework);
        framework.setEnum(frameworks);
        cliOptions.add(framework);

        // CLI Switches
        addSwitch(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC,
                this.hideGenerationTimestamp);

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

        addSwitch(CodegenConstants.GENERATE_PROPERTY_CHANGED,
                CodegenConstants.PACKAGE_DESCRIPTION_DESC,
                this.generatePropertyChanged);

        // NOTE: This will reduce visibility of all public members in templates. Users can use InternalsVisibleTo
        // https://msdn.microsoft.com/en-us/library/system.runtime.compilerservices.internalsvisibletoattribute(v=vs.110).aspx
        // to expose to shared code if the generated code is not embedded into another project. Otherwise, users of codegen
        // should rely on default public visibility.
        addSwitch(CodegenConstants.NON_PUBLIC_API,
                CodegenConstants.NON_PUBLIC_API_DESC,
                this.nonPublicApi);

        addSwitch(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS,
                CodegenConstants.ALLOW_UNICODE_IDENTIFIERS_DESC,
                this.allowUnicodeIdentifiers);

        addSwitch(CodegenConstants.NETCORE_PROJECT_FILE,
                CodegenConstants.NETCORE_PROJECT_FILE_DESC,
                this.netCoreProjectFileFlag);

        regexModifiers = new HashMap<Character, String>();
        regexModifiers.put('i', "IgnoreCase");
        regexModifiers.put('m', "Multiline");
        regexModifiers.put('s', "Singleline");
        regexModifiers.put('x', "IgnorePatternWhitespace");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // default HIDE_GENERATION_TIMESTAMP to true
        if (!additionalProperties.containsKey(CodegenConstants.HIDE_GENERATION_TIMESTAMP)) {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, Boolean.TRUE.toString());
        } else {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                    Boolean.valueOf(additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP).toString()));
        }

        if(isEmpty(apiPackage)) {
            apiPackage = "Api";
        }
        if(isEmpty(modelPackage)) {
            modelPackage = "Model";
        }
        clientPackage = "Client";

        Boolean excludeTests = false;
        if(additionalProperties.containsKey(CodegenConstants.EXCLUDE_TESTS)) {
            excludeTests = Boolean.valueOf(additionalProperties.get(CodegenConstants.EXCLUDE_TESTS).toString());
        }

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        additionalProperties.put("clientPackage", clientPackage);
        additionalProperties.put("emitDefaultValue", optionalEmitDefaultValue);

        if (!additionalProperties.containsKey("validatable")) {
            // default validatable to true if not set
            additionalProperties.put("validatable", true);
        }

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
            additionalProperties.put("validatable", false);
            additionalProperties.put("net35", true);
        } else if (NETSTANDARD.equals(this.targetFramework)){
            setTargetFrameworkNuget("netstandard1.3");
            setSupportsAsync(Boolean.TRUE);
            setSupportsUWP(Boolean.FALSE);
            setNetStandard(Boolean.TRUE);
            additionalProperties.put("supportsAsync", this.supportsAsync);
            additionalProperties.put("supportsUWP", this.supportsUWP);
            additionalProperties.put("netStandard", this.netStandard);

            //Tests not yet implemented for .NET Standard codegen
            //Todo implement it
            excludeTests = true;
            if(additionalProperties.containsKey(CodegenConstants.EXCLUDE_TESTS)){
                additionalProperties.remove(CodegenConstants.EXCLUDE_TESTS);
            }
            additionalProperties.put(CodegenConstants.EXCLUDE_TESTS, excludeTests);
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

        if(additionalProperties.containsKey(CodegenConstants.GENERATE_PROPERTY_CHANGED)) {
            if(NET35.equals(targetFramework)) {
                LOGGER.warn(CodegenConstants.GENERATE_PROPERTY_CHANGED + " is only supported by generated code for .NET 4+.");
            } else if(NETSTANDARD.equals(targetFramework)) {
                LOGGER.warn(CodegenConstants.GENERATE_PROPERTY_CHANGED + " is not supported in .NET Standard generated code.");
            } else if(Boolean.TRUE.equals(netCoreProjectFileFlag)) {
                LOGGER.warn(CodegenConstants.GENERATE_PROPERTY_CHANGED + " is not supported in .NET Core csproj project format.");
            } else {
                setGeneratePropertyChanged(Boolean.valueOf(additionalProperties.get(CodegenConstants.GENERATE_PROPERTY_CHANGED).toString()));
            }

            if(Boolean.FALSE.equals(this.generatePropertyChanged)) {
                additionalProperties.remove(CodegenConstants.GENERATE_PROPERTY_CHANGED);
            }
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

        if (additionalProperties.containsKey(CodegenConstants.NON_PUBLIC_API)) {
            setNonPublicApi(Boolean.valueOf(additionalProperties.get(CodegenConstants.NON_PUBLIC_API).toString()));
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
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                clientPackageDir, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse.mustache",
                clientPackageDir, "ApiResponse.cs"));
        supportingFiles.add(new SupportingFile("ExceptionFactory.mustache",
                clientPackageDir, "ExceptionFactory.cs"));
        supportingFiles.add(new SupportingFile("SwaggerDateConverter.mustache",
                clientPackageDir, "SwaggerDateConverter.cs"));

        if(Boolean.FALSE.equals(this.netStandard) && Boolean.FALSE.equals(this.netCoreProjectFileFlag)) {
            supportingFiles.add(new SupportingFile("compile.mustache", "", "build.bat"));
            supportingFiles.add(new SupportingFile("compile-mono.sh.mustache", "", "build.sh"));

            // copy package.config to nuget's standard location for project-level installs
            supportingFiles.add(new SupportingFile("packages.config.mustache", packageFolder + File.separator, "packages.config"));
            // .travis.yml for travis-ci.org CI
            supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
        } else if(Boolean.FALSE.equals(this.netCoreProjectFileFlag)) {
            supportingFiles.add(new SupportingFile("project.json.mustache", packageFolder + File.separator, "project.json"));
        }

        supportingFiles.add(new SupportingFile("IReadableConfiguration.mustache",
                clientPackageDir, "IReadableConfiguration.cs"));
        supportingFiles.add(new SupportingFile("GlobalConfiguration.mustache",
                clientPackageDir, "GlobalConfiguration.cs"));

        // Only write out test related files if excludeTests is unset or explicitly set to false (see start of this method)
        if(Boolean.FALSE.equals(excludeTests)) {
            // shell script to run the nunit test
            supportingFiles.add(new SupportingFile("mono_nunit_test.mustache", "", "mono_nunit_test.sh"));

            modelTestTemplateFiles.put("model_test.mustache", ".cs");
            apiTestTemplateFiles.put("api_test.mustache", ".cs");

            if (Boolean.FALSE.equals(this.netCoreProjectFileFlag)) {
                supportingFiles.add(new SupportingFile("packages_test.config.mustache", testPackageFolder + File.separator, "packages.config"));
            }
        }

        if(Boolean.TRUE.equals(generatePropertyChanged)) {
            supportingFiles.add(new SupportingFile("FodyWeavers.xml", packageFolder, "FodyWeavers.xml"));
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        // apache v2 license
        // UPDATE (20160612) no longer needed as the Apache v2 LICENSE is added globally
        //supportingFiles.add(new SupportingFile("LICENSE", "", "LICENSE"));

        if (optionalAssemblyInfoFlag && Boolean.FALSE.equals(this.netCoreProjectFileFlag)) {
            supportingFiles.add(new SupportingFile("AssemblyInfo.mustache", packageFolder + File.separator + "Properties", "AssemblyInfo.cs"));
        }
        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
            
            if(Boolean.TRUE.equals(this.netCoreProjectFileFlag)) {
                supportingFiles.add(new SupportingFile("netcore_project.mustache", packageFolder, packageName + ".csproj"));
            } else {
                supportingFiles.add(new SupportingFile("Project.mustache", packageFolder, packageName + ".csproj"));
                if(Boolean.FALSE.equals(this.netStandard)) {
                    supportingFiles.add(new SupportingFile("nuspec.mustache", packageFolder, packageName + ".nuspec"));
                }
            }

            if(Boolean.FALSE.equals(excludeTests)) {
                // NOTE: This exists here rather than previous excludeTests block because the test project is considered an optional project file.
                if(Boolean.TRUE.equals(this.netCoreProjectFileFlag)) {
                    supportingFiles.add(new SupportingFile("netcore_testproject.mustache", testPackageFolder, testPackageName + ".csproj"));
                } else {
                    supportingFiles.add(new SupportingFile("TestProject.mustache", testPackageFolder, testPackageName + ".csproj"));
                }
            }
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

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        postProcessPattern(parameter.pattern, parameter.vendorExtensions);
        super.postProcessParameter(parameter);
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        postProcessPattern(property.pattern, property.vendorExtensions);
        super.postProcessModelProperty(model, property);
    }


    /*
    * The swagger pattern spec follows the Perl convention and style of modifiers. .NET
    * does not support this syntax directly so we need to convert the pattern to a .NET compatible
    * format and apply modifiers in a compatible way.
    * See https://msdn.microsoft.com/en-us/library/yd1hzczs(v=vs.110).aspx for .NET options.
    * See https://github.com/swagger-api/swagger-codegen/pull/2794 for Python's initial implementation from which this is copied.
    */
    public void postProcessPattern(String pattern, Map<String, Object> vendorExtensions) {
        if(pattern != null) {
            int i = pattern.lastIndexOf('/');

            //Must follow Perl /pattern/modifiers convention
            if(pattern.charAt(0) != '/' || i < 2) {
                throw new IllegalArgumentException("Pattern must follow the Perl "
                        + "/pattern/modifiers convention. "+pattern+" is not valid.");
            }

            String regex = pattern.substring(1, i).replace("'", "\'");
            List<String> modifiers = new ArrayList<String>();

            // perl requires an explicit modifier to be culture specific and .NET is the reverse.
            modifiers.add("CultureInvariant");

            for(char c : pattern.substring(i).toCharArray()) {
                if(regexModifiers.containsKey(c)) {
                    String modifier = regexModifiers.get(c);
                    modifiers.add(modifier);
                } else if (c == 'l') {
                    modifiers.remove("CultureInvariant");
                }
            }

            vendorExtensions.put("x-regex", regex);
            vendorExtensions.put("x-modifiers", modifiers);
        }
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
                    codegenProperty.hasMore = count < numVars;
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
        } else if ("float?".equalsIgnoreCase(datatype)) {
            // for float in C#, append "f". e.g. 3.14 => 3.14f
            return value + "f";
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (value.length() == 0) {
            return "Empty";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(value) != null) {
            return camelize(getSymbolName(value));
        }

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
        //var = WordUtils.capitalizeFully(var);
        var = camelize(var);
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

    public void setNetStandard(Boolean netStandard){
        this.netStandard = netStandard;
    }

    public void setGeneratePropertyChanged(final Boolean generatePropertyChanged){
        this.generatePropertyChanged = generatePropertyChanged;
    }

    public boolean isNonPublicApi() {
        return nonPublicApi;
    }

    public void setNonPublicApi(final boolean nonPublicApi) {
        this.nonPublicApi = nonPublicApi;
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
