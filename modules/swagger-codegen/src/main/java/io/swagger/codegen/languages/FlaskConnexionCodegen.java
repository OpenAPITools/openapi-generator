package io.swagger.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;

import config.ConfigParser;
import io.swagger.codegen.*;
import io.swagger.models.HttpMethod;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;
import io.swagger.util.Yaml;

import java.io.File;
import java.util.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FlaskConnexionCodegen extends DefaultCodegen implements CodegenConfig {
	
    private static final Logger LOGGER = LoggerFactory.getLogger(FlaskConnexionCodegen.class);

    public static final String CONTROLLER_PACKAGE = "controllerPackage";
    public static final String DEFAULT_CONTROLLER = "defaultController";

    protected String apiVersion = "1.0.0";
    protected int serverPort = 8080;
    protected String projectName = "swagger-server";
    protected String controllerPackage;
    protected String defaultController;

    public FlaskConnexionCodegen() {
        super();

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("float");
        languageSpecificPrimitives.add("list");
        languageSpecificPrimitives.add("bool");
        languageSpecificPrimitives.add("str");
        languageSpecificPrimitives.add("datetime");
        languageSpecificPrimitives.add("date");

        typeMapping.clear();
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("number", "float");
        typeMapping.put("long", "int");
        typeMapping.put("double", "float");
        typeMapping.put("array", "list");
        typeMapping.put("map", "dict");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "str");
        typeMapping.put("date", "date");
        typeMapping.put("DateTime", "datetime");
        typeMapping.put("object", "object");
        typeMapping.put("file", "file");

        // set the output folder here
        outputFolder = "generated-code/connexion";

        modelTemplateFiles.clear();

        apiTemplateFiles.clear();

        /**
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "flaskConnexion";

        // from https://docs.python.org/release/2.5.4/ref/keywords.html
        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "and", "del", "from", "not", "while", "as", "elif", "global", "or", "with",
                        "assert", "else", "if", "pass", "yield", "break", "except", "import",
                        "print", "class", "exec", "in", "raise", "continue", "finally", "is",
                        "return", "def", "for", "lambda", "try"));

        /**
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties.put("serverPort", serverPort);

        /**
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */

        supportingFiles.add(new SupportingFile("swagger.mustache",
                        "swagger",
                        "swagger.yaml")
        );
        supportingFiles.add(new SupportingFile("app.mustache",
                        "",
                        "app.py")
        );
        supportingFiles.add(new SupportingFile("README.mustache",
                        "",
                        "README.md")
        );

        cliOptions.add(new CliOption(CONTROLLER_PACKAGE, "controller package").
                defaultValue("controllers"));
        cliOptions.add(new CliOption(DEFAULT_CONTROLLER, "default controller").
                defaultValue("default_controller"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        apiTemplateFiles.clear();

        if (additionalProperties.containsKey(CONTROLLER_PACKAGE)) {
            this.controllerPackage = additionalProperties.get(CONTROLLER_PACKAGE).toString();
        }
        else {
            this.controllerPackage = "controllers";
            additionalProperties.put(CONTROLLER_PACKAGE, this.controllerPackage);
        }
        if (additionalProperties.containsKey(DEFAULT_CONTROLLER)) {
            this.defaultController = additionalProperties.get(DEFAULT_CONTROLLER).toString();
        }
        else {
            this.defaultController = "default_controller";
            additionalProperties.put(DEFAULT_CONTROLLER, this.defaultController);
        }

        if(!new java.io.File(controllerPackage + File.separator + defaultController + ".py").exists()) {
            supportingFiles.add(new SupportingFile("controller.mustache",
                            controllerPackage,
                            defaultController + ".py")
            );
        }
    }

    @Override
    public String apiPackage() {
        return controllerPackage;
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see io.swagger.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -l flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-flask";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Python server library using the Connexion project. By default, " +
               "it will also generate service classes -- which you can disable with the `-Dnoservice` environment variable.";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultController";
        }
        return initialCaps(name);
    }

    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reseved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;  // add an underscore to the name
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public void preprocessSwagger(Swagger swagger) {
        if(swagger != null && swagger.getPaths() != null) {
            for(String pathname : swagger.getPaths().keySet()) {
                Path path = swagger.getPath(pathname);
                if(path.getOperations() != null) {
                    for(Map.Entry<HttpMethod, Operation> entry : path.getOperationMap().entrySet()) {
                        // Normalize `operationId` and add package/class path in front, e.g.
                        //     controllers.default_controller.add_pet
                        String httpMethod = entry.getKey().name().toLowerCase();
                        Operation operation = entry.getValue();
                        String operationId = getOrGenerateOperationId(operation, pathname, httpMethod);
                        if(!operationId.contains(".")) {
                            operationId = underscore(sanitizeName(operationId));
                            operationId = controllerPackage + "." + defaultController + "." + operationId;
                        }
                        operation.setOperationId(operationId);
                        if(operation.getTags() != null) {
                            List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
                            for(String tag : operation.getTags()) {
                                Map<String, String> value = new HashMap<String, String>();
                                value.put("tag", tag);
                                value.put("hasMore", "true");
                                tags.add(value);
                            }
                            if(tags.size() > 0) {
                                tags.get(tags.size() - 1).remove("hasMore");
                            }
                            if(operation.getTags().size() > 0) {
                                String tag = operation.getTags().get(0);
                                operation.setTags(Arrays.asList(tag));
                            }
                            operation.setVendorExtension("x-tags", tags);
                        }
                        else {
                            String tag = "default_controller";
                            operation.setTags(Arrays.asList(tag));
                        }
                    }
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    private static List<Map<String, Object>> getOperations(Map<String, Object> objs) {
        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();
        Map<String, Object> apiInfo = (Map<String, Object>) objs.get("apiInfo");
        List<Map<String, Object>> apis = (List<Map<String, Object>>) apiInfo.get("apis");
        for (Map<String, Object> api : apis) {
            result.add((Map<String, Object>) api.get("operations"));
        }
        return result;
    }

    private static List<Map<String, Object>> sortOperationsByPath(List<CodegenOperation> ops) {
        Multimap<String, CodegenOperation> opsByPath = ArrayListMultimap.create();

        for (CodegenOperation op : ops) {
            opsByPath.put(op.path, op);
        }

        List<Map<String, Object>> opsByPathList = new ArrayList<Map<String, Object>>();
        for (Map.Entry<String, Collection<CodegenOperation>> entry : opsByPath.asMap().entrySet()) {
            Map<String, Object> opsByPathEntry = new HashMap<String, Object>();
            opsByPathList.add(opsByPathEntry);
            opsByPathEntry.put("path", entry.getKey());
            opsByPathEntry.put("operation", entry.getValue());
            List<CodegenOperation> operationsForThisPath = Lists.newArrayList(entry.getValue());
            operationsForThisPath.get(operationsForThisPath.size() - 1).hasMore = null;
            if (opsByPathList.size() < opsByPath.asMap().size()) {
                opsByPathEntry.put("hasMore", "true");
            }
        }

        return opsByPathList;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Swagger swagger = (Swagger)objs.get("swagger");
        if(swagger != null) {
            try {
                objs.put("swagger-yaml", Yaml.mapper().writeValueAsString(swagger));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }
        for (Map<String, Object> operations : getOperations(objs)) {
            @SuppressWarnings("unchecked")
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");

            List<Map<String, Object>> opsByPathList = sortOperationsByPath(ops);
            operations.put("operationsByPath", opsByPathList);
        }
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String toOperationId(String operationId) {
        operationId = super.toOperationId(operationId); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // Use the part after the last dot, e.g.
        //     controllers.defaultController.addPet => addPet
        operationId = operationId.replaceAll(".*\\.", "");
        // Need to underscore it since it has been processed via removeNonNameElementToCamelCase, e.g.
        //     addPet => add_pet
        return underscore(operationId);
    }
}
