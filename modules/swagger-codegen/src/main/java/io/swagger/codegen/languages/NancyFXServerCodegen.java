package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;

public class NancyFXServerCodegen extends AbstractCSharpCodegen {

    protected String packageGuid = "{" + java.util.UUID.randomUUID().toString().toUpperCase() + "}";

    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(NancyFXServerCodegen.class);

    public NancyFXServerCodegen() {
        outputFolder = "generated-code" + File.separator + this.getName();

        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");

        // contextually reserved words
        setReservedWordsLowerCase(
            Arrays.asList("var", "async", "await", "dynamic", "yield")
        );

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

        // CLI Switches
        addSwitch(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC,
                this.sortParamsByRequiredFlag);

        addSwitch(CodegenConstants.OPTIONAL_PROJECT_FILE,
                CodegenConstants.OPTIONAL_PROJECT_FILE_DESC,
                this.optionalProjectFileFlag);

        addSwitch(CodegenConstants.USE_DATETIME_OFFSET,
                CodegenConstants.USE_DATETIME_OFFSET_DESC,
                this.useDateTimeOffsetFlag);

        addSwitch(CodegenConstants.USE_COLLECTION,
                CodegenConstants.USE_COLLECTION_DESC,
                this.useCollection);

        addSwitch(CodegenConstants.RETURN_ICOLLECTION,
                CodegenConstants.RETURN_ICOLLECTION_DESC,
                this.returnICollection);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "nancyfx";
    }

    @Override
    public String getHelp() {
        return "Generates a NancyFX Web API server.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        apiPackage = packageName + ".Module";
        modelPackage = packageName + ".Model";

        supportingFiles.add(new SupportingFile("RequestExtensions.mustache", sourceFolder(), "RequestExtensions.cs"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", sourceFolder(), "packages.config"));

        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
            supportingFiles.add(new SupportingFile("Project.mustache", sourceFolder(), packageName + ".csproj"));
        }
        additionalProperties.put("packageGuid", packageGuid);
    }

    private String sourceFolder() {
        return "src" + File.separator + packageName;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder() + File.separator + "Module";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder() + File.separator + "Model";
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
        operation.httpMethod = operation.httpMethod.substring(0, 1) + operation.httpMethod.substring(1).toLowerCase();
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        String enumName = sanitizeName(name);

        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        enumName = camelize(enumName);

        LOGGER.info("toEnumVarName = " + enumName);

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }
}
