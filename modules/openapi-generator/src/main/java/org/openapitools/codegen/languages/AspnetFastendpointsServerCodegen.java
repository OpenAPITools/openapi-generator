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
    public static final String USE_RECORDS = "useRecords";
    public static final String USE_AUTHENTICATION = "useAuthentication";
    public static final String USE_VALIDATORS = "useValidators";
    public static final String USE_RESPONSE_CACHING = "useResponseCaching";

    private final Logger LOGGER = LoggerFactory.getLogger(AspnetFastendpointsServerCodegen.class);

    private boolean useProblemDetails = false;
    private boolean useRecords = false;
    private boolean useAuthentication = false;
    private boolean useValidators = false;
    private boolean UseResponseCaching = false;


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

        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("endpoint.mustache", "Endpoint.cs");
        apiTemplateFiles.put("request.mustache", "Request.cs");

        addSwitch(USE_PROBLEM_DETAILS, "Enable RFC compatible error responses (https://fast-endpoints.com/docs/configuration-settings#rfc7807-rfc9457-compatible-problem-details).", useProblemDetails);
        addSwitch(USE_RECORDS, "Use record instead of class for the requests and response.", useRecords);
        addSwitch(USE_AUTHENTICATION, "Enable authentication (https://fast-endpoints.com/docs/security).", useAuthentication);
        addSwitch(USE_VALIDATORS, "Enable request validators (https://fast-endpoints.com/docs/validation).", useValidators);
        addSwitch(USE_RESPONSE_CACHING, "Enable response caching (https://fast-endpoints.com/docs/response-caching).", UseResponseCaching);
    }

    @Override
    public void processOpts() {
        additionalProperties.put("projectGuid", "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}");
        additionalProperties.put("projectConfigurationGuid", "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}");
        additionalProperties.put("solutionGuid", "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}");

        setPackageDescription(openAPI.getInfo().getDescription());

        setUseProblemDetails();
        setUseRecordForRequest();
        setUseAuthentication();
        setUseValidators();
        setUseResponseCaching();

        super.processOpts();

        addSupportingFiles();
    }

    private void addSupportingFiles() {
        apiPackage = "Features";
        modelPackage = "Models";
        String packageFolder = sourceFolder + File.separator + packageName;

        if(useAuthentication) {
            supportingFiles.add(new SupportingFile("loginRequest.mustache", packageFolder + File.separator + apiPackage, "LoginRequest.cs"));
            supportingFiles.add(new SupportingFile("userLoginEndpoint.mustache", packageFolder + File.separator + apiPackage, "UserLoginEndpoint.cs"));
        }

        supportingFiles.add(new SupportingFile("readme.mustache", sourceFolder, "README.md"));
        supportingFiles.add(new SupportingFile("gitignore", sourceFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("solution.mustache", sourceFolder, packageName + ".sln"));
        supportingFiles.add(new SupportingFile("project.csproj.mustache", packageFolder, packageName + ".csproj"));
        supportingFiles.add(new SupportingFile("Properties" + File.separator + "launchSettings.json", packageFolder + File.separator + "Properties", "launchSettings.json"));

        supportingFiles.add(new SupportingFile("appsettings.json", packageFolder, "appsettings.json"));
        supportingFiles.add(new SupportingFile("appsettings.Development.json", packageFolder, "appsettings.Development.json"));

        supportingFiles.add(new SupportingFile("program.mustache", packageFolder, "Program.cs"));
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

    private void setUseRecordForRequest() {
        if (additionalProperties.containsKey(USE_RECORDS)) {
            useRecords = convertPropertyToBooleanAndWriteBack(USE_RECORDS);
        } else {
            additionalProperties.put(USE_RECORDS, useRecords);
        }
    }

    private void setUseAuthentication() {
        if (additionalProperties.containsKey(USE_AUTHENTICATION)) {
            useAuthentication = convertPropertyToBooleanAndWriteBack(USE_AUTHENTICATION);
        } else {
            additionalProperties.put(USE_AUTHENTICATION, useAuthentication);
        }
    }

    private void setUseValidators() {
        if (additionalProperties.containsKey(USE_VALIDATORS)) {
            useValidators = convertPropertyToBooleanAndWriteBack(USE_VALIDATORS);
        } else {
            additionalProperties.put(USE_VALIDATORS, useValidators);
        }
    }

    private void setUseResponseCaching() {
        if (additionalProperties.containsKey(USE_RESPONSE_CACHING)) {
            UseResponseCaching = convertPropertyToBooleanAndWriteBack(USE_RESPONSE_CACHING);
        } else {
            additionalProperties.put(USE_RESPONSE_CACHING, UseResponseCaching);
        }
    }
}
