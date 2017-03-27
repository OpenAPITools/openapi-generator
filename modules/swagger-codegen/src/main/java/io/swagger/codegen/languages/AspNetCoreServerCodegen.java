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

    private String packageGuid = "{" + randomUUID().toString().toUpperCase() + "}";

    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(AspNetCoreServerCodegen.class);

    public AspNetCoreServerCodegen() {
        super();

        setSourceFolder("src");
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

        addOption(CodegenConstants.OPTIONAL_PROJECT_GUID,
                CodegenConstants.OPTIONAL_PROJECT_GUID_DESC,
                null);

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

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_GUID)) {
            setPackageGuid((String) additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_GUID));
        }
        additionalProperties.put("packageGuid", packageGuid);
        
        additionalProperties.put("dockerTag", this.packageName.toLowerCase());

        apiPackage = packageName + ".Controllers";
        modelPackage = packageName + ".Models";

        String packageFolder = sourceFolder + File.separator + packageName;

        supportingFiles.add(new SupportingFile("NuGet.Config", "", "NuGet.Config"));
        supportingFiles.add(new SupportingFile("global.json", "", "global.json"));
        supportingFiles.add(new SupportingFile("build.sh.mustache", "", "build.sh"));
        supportingFiles.add(new SupportingFile("build.bat.mustache", "", "build.bat"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Solution.mustache", "", this.packageName + ".sln"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", packageFolder, "Dockerfile"));
        supportingFiles.add(new SupportingFile("gitignore", packageFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("appsettings.json", packageFolder, "appsettings.json"));

        supportingFiles.add(new SupportingFile("project.json.mustache", packageFolder, "project.json"));
        supportingFiles.add(new SupportingFile("Startup.mustache", packageFolder, "Startup.cs"));
        supportingFiles.add(new SupportingFile("Program.mustache", packageFolder, "Program.cs"));
        supportingFiles.add(new SupportingFile("web.config", packageFolder, "web.config"));

        supportingFiles.add(new SupportingFile("Project.xproj.mustache", packageFolder, this.packageName + ".xproj"));

        supportingFiles.add(new SupportingFile("Properties" + File.separator + "launchSettings.json", packageFolder + File.separator + "Properties", "launchSettings.json"));

        supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "README.md", packageFolder + File.separator + "wwwroot", "README.md"));
        supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "index.html", packageFolder + File.separator + "wwwroot", "index.html"));
        supportingFiles.add(new SupportingFile("wwwroot" + File.separator + "web.config", packageFolder + File.separator + "wwwroot", "web.config"));
    }

    @Override
    public void setSourceFolder(final String sourceFolder) {
        if(sourceFolder == null) {
            LOGGER.warn("No sourceFolder specified, using default");
            this.sourceFolder =  "src" + File.separator + this.packageName;
        } else if(!sourceFolder.equals("src") && !sourceFolder.startsWith("src")) {
            LOGGER.warn("ASP.NET Core requires source code exists under src. Adjusting.");
            this.sourceFolder =  "src" + File.separator + sourceFolder;
        } else {
            this.sourceFolder = sourceFolder;
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
        return outputFolder + File.separator + sourceFolder + File.separator + packageName + File.separator  + "Models";
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
