package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Locale;

import static java.util.UUID.randomUUID;

public class AspnetFastendpointsServerCodegen extends AbstractCSharpCodegen implements CodegenConfig {

    public static final String PROJECT_NAME = "projectName";
    public static final String USE_PROBLEM_DETAILS = "useProblemDetails";
    public static final String USE_RECORDS = "useRecords";
    public static final String USE_AUTHENTICATION = "useAuthentication";
    public static final String USE_VALIDATORS = "useValidators";
    public static final String USE_RESPONSE_CACHING = "useResponseCaching";
    public static final String USE_API_VERSIONING = "useApiVersioning";
    public static final String ROUTE_PREFIX = "routePrefix";
    public static final String VERSIONING_PREFIX = "versioningPrefix";
    public static final String API_VERSION = "apiVersion";
    public static final String SOLUTION_GUID = "solutionGuid";
    public static final String PROJECT_CONFIGURATION_GUID = "projectConfigurationGuid";

    private final Logger LOGGER = LoggerFactory.getLogger(AspnetFastendpointsServerCodegen.class);

    private boolean useProblemDetails = false;
    private boolean useRecords = false;
    private boolean useAuthentication = false;
    private boolean useValidators = false;
    private boolean useResponseCaching = false;
    private boolean useApiVersioning = false;
    private String routePrefix = "api";
    private String versioningPrefix = "v";
    private String apiVersion = "1";
    private String solutionGuid = null;
    private String projectConfigurationGuid = null;


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
        addSwitch(USE_RESPONSE_CACHING, "Enable response caching (https://fast-endpoints.com/docs/response-caching).", useResponseCaching);
        addSwitch(USE_API_VERSIONING, "Enable API versioning (https://fast-endpoints.com/docs/api-versioning).", useApiVersioning);
        addOption(ROUTE_PREFIX, "The route prefix for the API. Used only if useApiVersioning is true", routePrefix);
        addOption(VERSIONING_PREFIX, "The versioning prefix for the API. Used only if useApiVersioning is true", versioningPrefix);
        addOption(API_VERSION, "The version of the API. Used only if useApiVersioning is true", apiVersion);
        addOption(SOLUTION_GUID, "The solution GUID to be used in the solution file (auto generated if not provided)", solutionGuid);
        addOption(PROJECT_CONFIGURATION_GUID, "The project configuration GUID to be used in the solution file (auto generated if not provided)", projectConfigurationGuid);
    }

    @Override
    public void processOpts() {

        setPackageDescription(openAPI.getInfo().getDescription());

        setUseProblemDetails();
        setUseRecordForRequest();
        setUseAuthentication();
        setUseValidators();
        setUseResponseCaching();
        setUseApiVersioning();
        setRoutePrefix();
        setVersioningPrefix();
        setApiVersion();
        setSolutionGuid();
        setProjectConfigurationGuid();

        super.processOpts();

        addSupportingFiles();
    }

    private void addSupportingFiles() {
        apiPackage = "Features";
        modelPackage = "Models";
        String packageFolder = sourceFolder + File.separator + packageName;

        if (useAuthentication) {
            supportingFiles.add(new SupportingFile("loginRequest.mustache", packageFolder + File.separator + apiPackage, "LoginRequest.cs"));
            supportingFiles.add(new SupportingFile("userLoginEndpoint.mustache", packageFolder + File.separator + apiPackage, "UserLoginEndpoint.cs"));
        }

        supportingFiles.add(new SupportingFile("readme.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("solution.mustache", "", packageName + ".sln"));
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
            useResponseCaching = convertPropertyToBooleanAndWriteBack(USE_RESPONSE_CACHING);
        } else {
            additionalProperties.put(USE_RESPONSE_CACHING, useResponseCaching);
        }
    }

    private void setUseApiVersioning() {
        if (additionalProperties.containsKey(USE_API_VERSIONING)) {
            useApiVersioning = convertPropertyToBooleanAndWriteBack(USE_API_VERSIONING);
        } else {
            additionalProperties.put(USE_API_VERSIONING, useApiVersioning);
        }
    }

    private void setRoutePrefix() {
        if (additionalProperties.containsKey(ROUTE_PREFIX)) {
            routePrefix = (String) additionalProperties.get(ROUTE_PREFIX);
        } else {
            additionalProperties.put(ROUTE_PREFIX, routePrefix);
        }
    }

    private void setVersioningPrefix() {
        if (additionalProperties.containsKey(VERSIONING_PREFIX)) {
            versioningPrefix = (String) additionalProperties.get(VERSIONING_PREFIX);
        } else {
            additionalProperties.put(VERSIONING_PREFIX, versioningPrefix);
        }
    }

    private void setApiVersion() {
        if (additionalProperties.containsKey(API_VERSION)) {
            apiVersion = (String) additionalProperties.get(API_VERSION);
        } else {
            additionalProperties.put(API_VERSION, apiVersion);
        }
    }

    private void setSolutionGuid() {
        if (additionalProperties.containsKey(SOLUTION_GUID)) {
            solutionGuid = (String) additionalProperties.get(SOLUTION_GUID);
        } else {
            solutionGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";
            additionalProperties.put(SOLUTION_GUID, solutionGuid);
        }
    }

    private void setProjectConfigurationGuid() {
        if (additionalProperties.containsKey(PROJECT_CONFIGURATION_GUID)) {
            projectConfigurationGuid = (String) additionalProperties.get(PROJECT_CONFIGURATION_GUID);
        } else {
            projectConfigurationGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";
            additionalProperties.put(PROJECT_CONFIGURATION_GUID, projectConfigurationGuid);
        }
    }
}
