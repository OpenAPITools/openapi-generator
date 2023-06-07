package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.StringUtils;
import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;


public class JavaMicronautServerCodegen extends JavaMicronautAbstractCodegen {
    public static final String OPT_CONTROLLER_PACKAGE = "controllerPackage";
    public static final String OPT_GENERATE_CONTROLLER_FROM_EXAMPLES = "generateControllerFromExamples";
    public static final String OPT_GENERATE_CONTROLLER_AS_ABSTRACT = "generateControllerAsAbstract";
    public static final String OPT_GENERATE_OPERATIONS_TO_RETURN_NOT_IMPLEMENTED = "generateOperationsToReturnNotImplemented";

    public static final String EXTENSION_ROLES = "x-roles";
    public static final String ANONYMOUS_ROLE_KEY = "isAnonymous()";
    public static final String ANONYMOUS_ROLE = "SecurityRule.IS_ANONYMOUS";
    public static final String AUTHORIZED_ROLE_KEY = "isAuthorized()";
    public static final String AUTHORIZED_ROLE = "SecurityRule.IS_AUTHENTICATED";
    public static final String DENY_ALL_ROLE_KEY = "denyAll()";
    public static final String DENY_ALL_ROLE = "SecurityRule.DENY_ALL";

    public static final String NAME = "java-micronaut-server";

    protected String controllerPackage = "org.openapitools.controller";
    protected boolean generateControllerAsAbstract = false;
    protected boolean generateOperationsToReturnNotImplemented = true;
    protected boolean generateControllerFromExamples = false;
    protected boolean useAuth = true;

    protected String controllerPrefix = "";
    protected String controllerSuffix = "Controller";
    protected String apiPrefix = "Abstract";
    protected String apiSuffix = "Controller";

    public JavaMicronautServerCodegen() {
        super();

        title = "OpenAPI Micronaut Server";
        apiPackage = "org.openapitools.api";
        apiDocPath = "docs/controllers";

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();
        additionalProperties.put("server", "true");

        cliOptions.add(new CliOption(OPT_CONTROLLER_PACKAGE, "The package in which controllers will be generated").defaultValue(apiPackage));
        cliOptions.removeIf(c -> c.getOpt().equals(CodegenConstants.API_PACKAGE));
        cliOptions.add(CliOption.newBoolean(OPT_GENERATE_CONTROLLER_FROM_EXAMPLES,
                "Generate the implementation of controller and tests from parameter and return examples that will verify that the api works as desired (for testing)",
                generateControllerFromExamples));
        cliOptions.add(CliOption.newBoolean(OPT_GENERATE_CONTROLLER_AS_ABSTRACT,
                "Generate an abstract class for controller to be extended. (" + CodegenConstants.API_PACKAGE +
                " is then used for the abstract class, and " + OPT_CONTROLLER_PACKAGE +
                " is used for implementation that extends it.)",
                generateControllerAsAbstract));
        cliOptions.add(CliOption.newBoolean(OPT_GENERATE_OPERATIONS_TO_RETURN_NOT_IMPLEMENTED,
                "Return HTTP 501 Not Implemented instead of an empty response in the generated controller methods.",
                generateOperationsToReturnNotImplemented));

        cliOptions.add(CliOption.newBoolean(OPT_USE_AUTH, "Whether to import authorization and to annotate controller methods accordingly", useAuth));

        // Set the type mappings
        // It could be also StreamingFileUpload
        typeMapping.put("file", "CompletedFileUpload");
        importMapping.put("CompletedFileUpload", "io.micronaut.http.multipart.CompletedFileUpload");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    public String getHelp() {
        return "Generates a Java Micronaut Server.";
    }

    @Override
    public void processOpts() {
        // Get all the properties that require to know if user specified them directly
        if (additionalProperties.containsKey(OPT_GENERATE_CONTROLLER_AS_ABSTRACT)) {
            generateControllerAsAbstract = convertPropertyToBoolean(OPT_GENERATE_CONTROLLER_AS_ABSTRACT);
        }
        writePropertyBack(OPT_GENERATE_CONTROLLER_AS_ABSTRACT, generateControllerAsAbstract);

        if (additionalProperties.containsKey(OPT_GENERATE_OPERATIONS_TO_RETURN_NOT_IMPLEMENTED)) {
            generateOperationsToReturnNotImplemented = convertPropertyToBoolean(OPT_GENERATE_OPERATIONS_TO_RETURN_NOT_IMPLEMENTED);
        }
        writePropertyBack(OPT_GENERATE_OPERATIONS_TO_RETURN_NOT_IMPLEMENTED, generateOperationsToReturnNotImplemented);

        if (additionalProperties.containsKey(OPT_CONTROLLER_PACKAGE)) {
            controllerPackage = (String) additionalProperties.get(OPT_CONTROLLER_PACKAGE);
        } else if (!generateControllerAsAbstract && additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            controllerPackage = (String) additionalProperties.get(CodegenConstants.API_PACKAGE);
        }
        additionalProperties.put(OPT_CONTROLLER_PACKAGE, controllerPackage);

        if (!generateControllerAsAbstract) {
            apiPackage = controllerPackage;
            additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        }
        super.processOpts();

        // Get all the other properties after superclass processed everything
        if (additionalProperties.containsKey(OPT_GENERATE_CONTROLLER_FROM_EXAMPLES)) {
            generateControllerFromExamples = convertPropertyToBoolean(OPT_GENERATE_CONTROLLER_FROM_EXAMPLES);
        }
        writePropertyBack(OPT_GENERATE_CONTROLLER_FROM_EXAMPLES, generateControllerFromExamples);

        if (additionalProperties.containsKey(OPT_USE_AUTH)) {
            useAuth = convertPropertyToBoolean(OPT_USE_AUTH);
        }
        writePropertyBack(OPT_USE_AUTH, useAuth);

        // Api file
        apiTemplateFiles.clear();
        if (generateControllerAsAbstract) {
            setApiNamePrefix(apiPrefix);
            setApiNameSuffix(apiSuffix);
        } else {
            setApiNamePrefix(controllerPrefix);
            setApiNameSuffix(controllerSuffix);
        }
        apiTemplateFiles.put("server/controller.mustache", ".java");

        // Add documentation files
        supportingFiles.add(new SupportingFile("server/doc/README.mustache", "", "README.md").doNotOverwrite());
        apiDocTemplateFiles.clear();
        apiDocTemplateFiles.put("server/doc/controller_doc.mustache", ".md");

        // Add test files
        apiTestTemplateFiles.clear();
        // Controller Implementation is generated as a test file - so that it is not overwritten
        if (generateControllerAsAbstract) {
            apiTestTemplateFiles.put("server/controllerImplementation.mustache", ".java");
        }
        if (testTool.equals(OPT_TEST_JUNIT)) {
            apiTestTemplateFiles.put("server/test/controller_test.mustache", ".java");
        } else if (testTool.equals(OPT_TEST_SPOCK)) {
            apiTestTemplateFiles.put("server/test/controller_test.groovy.mustache", ".groovy");
        }

        // Add Application.java file
        String invokerFolder = (sourceFolder + '/' + invokerPackage).replace('.', '/');
        supportingFiles.add(new SupportingFile("common/configuration/Application.mustache", invokerFolder, "Application.java").doNotOverwrite());
    }

    @Override
    public String apiTestFileFolder() {
        // Set it to the whole output dir, so that validation always passes
        return super.getOutputDir();
    }

    @Override
    public String apiTestFilename(String templateName, String tag) {
        // For controller implementation
        if (generateControllerAsAbstract && templateName.contains("controllerImplementation")) {
            String implementationFolder = outputFolder + File.separator +
                    sourceFolder + File.separator +
                    controllerPackage.replace('.', File.separatorChar);
            return (implementationFolder + File.separator +
                    StringUtils.camelize(controllerPrefix + "_" + tag + "_" + controllerSuffix) + ".java"
            ).replace('/', File.separatorChar);
        }

        // For api tests
        String suffix = apiTestTemplateFiles().get(templateName);
        return super.apiTestFileFolder() + File.separator + toApiTestFilename(tag) + suffix;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        super.setParameterExampleValue(p);

        if (p.isFile) {
            // The CompletedFileUpload cannot be initialized
            p.example = "null";
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        // Add the controller classname to operations
        OperationMap operations = objs.getOperations();
        String controllerClassname = StringUtils.camelize(controllerPrefix + "_" + operations.getPathPrefix() + "_" + controllerSuffix);
        objs.put("controllerClassname", controllerClassname);

        List<CodegenOperation> allOperations = (List<CodegenOperation>) operations.get("operation");
        if (useAuth) {
            for (CodegenOperation operation : allOperations) {
                if (!operation.vendorExtensions.containsKey(EXTENSION_ROLES)) {
                    String role = operation.hasAuthMethods ? AUTHORIZED_ROLE : ANONYMOUS_ROLE;
                    operation.vendorExtensions.put(EXTENSION_ROLES, Collections.singletonList(role));
                } else {
                    List<String> roles = (List<String>) operation.vendorExtensions.get(EXTENSION_ROLES);
                    roles = roles.stream().map(role -> {
                        switch (role) {
                            case ANONYMOUS_ROLE_KEY:
                                return ANONYMOUS_ROLE;
                            case AUTHORIZED_ROLE_KEY:
                                return AUTHORIZED_ROLE;
                            case DENY_ALL_ROLE_KEY:
                                return DENY_ALL_ROLE;
                            default:
                                return "\"" + escapeText(role) + "\"";
                        }
                    }).collect(Collectors.toList());
                    operation.vendorExtensions.put(EXTENSION_ROLES, roles);
                }
            }
        }

        return objs;
    }
}
