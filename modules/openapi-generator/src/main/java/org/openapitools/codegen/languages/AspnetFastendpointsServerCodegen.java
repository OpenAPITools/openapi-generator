package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static java.util.UUID.randomUUID;

public class AspnetFastendpointsServerCodegen extends AbstractCSharpCodegen implements CodegenConfig {

    public static final String PROJECT_NAME = "projectName";
    public static final String USE_PROBLEM_DETAILS = "useProblemDetails";

    private final Logger LOGGER = LoggerFactory.getLogger(AspnetFastendpointsServerCodegen.class);

    private boolean useProblemDetails = false;


    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "aspnet-fastendpoints";
    }

    public String getHelp() {
        return "Generates a server for FastEndpoints (https://fast-endpoints.com/).";
    }

    public AspnetFastendpointsServerCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "aspnet-fastendpoints";

        embeddedTemplateDir = templateDir = "aspnet-fastendpoints";
        apiPackage = "Features";
        modelPackage = "Models";

        String packageFolder = sourceFolder + File.separator + packageName;

        modelTemplateFiles.put("model.mustache", ".cs");

        apiTemplateFiles.put("endpoint.mustache", "Endpoint.cs");
        apiTemplateFiles.put("request.mustache", "Request.cs");


        supportingFiles.add(new SupportingFile("readme.mustache", sourceFolder, "README.md"));
        supportingFiles.add(new SupportingFile("gitignore", sourceFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("solution.mustache", sourceFolder, packageName + ".sln"));
        supportingFiles.add(new SupportingFile("project.csproj.mustache", packageFolder, packageName + ".csproj"));
        supportingFiles.add(new SupportingFile("Properties" + File.separator + "launchSettings.json", packageFolder + File.separator + "Properties", "launchSettings.json"));

        supportingFiles.add(new SupportingFile("appsettings.json", packageFolder, "appsettings.json"));
        supportingFiles.add(new SupportingFile("appsettings.Development.json", packageFolder, "appsettings.Development.json"));

        supportingFiles.add(new SupportingFile("program.mustache", packageFolder, "Program.cs"));

        addSwitch(USE_PROBLEM_DETAILS, "Enable RFC compatible error responses.", useProblemDetails);
    }

    @Override
    public void processOpts() {
        additionalProperties.put("projectGuid", "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}");
        additionalProperties.put("projectConfigurationGuid", "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}");
        additionalProperties.put("solutionGuid", "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}");

        setPackageDescription(openAPI.getInfo().getDescription());

        setUseProblemDetails();

        super.processOpts();
    }

    @Override
    protected void processOperation(CodegenOperation operation) {
        super.processOperation(operation);

        // Converts, for example, PUT to Put for endpoint configuration
        operation.httpMethod = operation.httpMethod.charAt(0) + operation.httpMethod.substring(1).toLowerCase(Locale.ROOT);
    }

    private void setUseProblemDetails() {
        if (additionalProperties.containsKey(USE_PROBLEM_DETAILS)) {
            useProblemDetails = convertPropertyToBooleanAndWriteBack(USE_PROBLEM_DETAILS);
        } else {
            additionalProperties.put(USE_PROBLEM_DETAILS, useProblemDetails);
        }
    }
}
