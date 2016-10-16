package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;

import static java.util.UUID.randomUUID;

public class AspNetCoreServerCodegen extends AbstractCSharpCodegen {

    protected String sourceFolder = "src" + File.separator + packageName;

    private final String packageGuid = "{" + randomUUID().toString().toUpperCase() + "}";

    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(AspNetCoreServerCodegen.class);

    public AspNetCoreServerCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + this.getName();

        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("controller.mustache", ".cs");

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
        return "aspnetcore";
    }

    @Override
    public String getHelp() {
        return "Generates an ASP.NET Core Web API server.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        additionalProperties.put("packageGuid", packageGuid);

        apiPackage = packageName + ".Controllers";
        modelPackage = packageName + ".Models";

        supportingFiles.add(new SupportingFile("NuGet.Config", "", "NuGet.Config"));
        supportingFiles.add(new SupportingFile("global.json", "", "global.json"));
        supportingFiles.add(new SupportingFile("build.sh.mustache", "", "build.sh"));
        supportingFiles.add(new SupportingFile("build.bat.mustache", "", "build.bat"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Solution.mustache", "", this.packageName + ".sln"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", this.sourceFolder, "Dockerfile"));
        supportingFiles.add(new SupportingFile("gitignore", this.sourceFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("appsettings.json", this.sourceFolder, "appsettings.json"));

        supportingFiles.add(new SupportingFile("project.json.mustache", this.sourceFolder, "project.json"));
        supportingFiles.add(new SupportingFile("Startup.mustache", this.sourceFolder, "Startup.cs"));
        supportingFiles.add(new SupportingFile("Program.mustache", this.sourceFolder, "Program.cs"));
        supportingFiles.add(new SupportingFile("web.config", this.sourceFolder, "web.config"));

        supportingFiles.add(new SupportingFile("Project.xproj.mustache", this.sourceFolder, this.packageName + ".xproj"));

        supportingFiles.add(new SupportingFile("Properties" + File.separator + "launchSettings.json", this.sourceFolder + File.separator + "Properties", "launchSettings.json"));

        supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "README.md", this.sourceFolder + File.separator + "wwwroot", "README.md"));
        supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "index.html", this.sourceFolder + File.separator + "wwwroot", "index.html"));
        supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "web.config", this.sourceFolder + File.separator + "wwwroot", "web.config"));
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "Controllers";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "Models";
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
        operation.httpMethod = "Http" + operation.httpMethod.substring(0, 1) + operation.httpMethod.substring(1).toLowerCase();
    }
}
