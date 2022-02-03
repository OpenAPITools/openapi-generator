package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.io.File;
import java.util.List;
import java.util.Map;


public class JavaMicronautServerCodegen extends JavaMicronautAbstractCodegen {
    public static final String OPT_CONTROLLER_PACKAGE = "controllerPackage";
    public static final String OPT_GENERATE_CONTROLLER_FROM_EXAMPLES = "generateControllerFromExamples";
    public static final String OPT_GENERATE_CONTROLLER_AS_ABSTRACT = "generateControllerAsAbstract";

    private final Logger LOGGER = LoggerFactory.getLogger(JavaClientCodegen.class);

    public static final String NAME = "java-micronaut-server";

    protected String controllerPackage = "org.openapitools.controller";
    protected boolean generateControllerAsAbstract = false;
    protected boolean generateControllerFromExamples = false;
    protected boolean useAuth = true;

    protected String controllerPrefix = "";
    protected String controllerSuffix = "Controller";
    protected String apiPrefix = "Abstract";
    protected String apiSuffix = "Controller";

    public JavaMicronautServerCodegen() {
        super();

        title = "OpenAPI Micronaut Server";;
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
    public String apiTestFilename(String templateName, String tag) {
        if (generateControllerAsAbstract && templateName.contains("controllerImplementation")) {
            return (
                    outputFolder + File.separator +
                    sourceFolder + File.separator +
                    controllerPackage.replace('.', File.separatorChar) + File.separator +
                    StringUtils.camelize(controllerPrefix + "_" + tag + "_" + controllerSuffix) + ".java"
            ).replace('/', File.separatorChar);
        }

        return super.apiTestFilename(templateName, tag);
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
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        // Add the controller classname to operations
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        String controllerClassname = StringUtils.camelize(controllerPrefix + "_" + operations.get("pathPrefix") + "_" + controllerSuffix);
        objs.put("controllerClassname", controllerClassname);

        return objs;
    }
}
