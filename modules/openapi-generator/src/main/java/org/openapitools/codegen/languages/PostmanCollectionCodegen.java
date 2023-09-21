package org.openapitools.codegen.languages;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.ServerVariable;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * OpenAPI generator for Postman Collection format v2.1
 */
public class PostmanCollectionCodegen extends DefaultCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(PostmanCollectionCodegen.class);

    protected String apiVersion = "1.0.0";

    // Select whether to create folders according to the spec’s paths or tags. Values: Paths | Tags
    public static final String FOLDER_STRATEGY = "folderStrategy";
    public static final String FOLDER_STRATEGY_DEFAULT_VALUE = "Tags";
    // Select whether to create Postman variables for path templates
    public static final String PATH_PARAMS_AS_VARIABLES = "pathParamsAsVariables";
    public static final Boolean PATH_PARAMS_AS_VARIABLES_DEFAULT_VALUE = false;

    public static final String POSTMAN_FILE_DEFAULT_VALUE = "postman.json";

    // create Postman variables from placeholders ie {{VAR_1}}
    public static final String POSTMAN_VARIABLES = "postmanVariables";
    protected Boolean postmanVariables = true;
    // replace placeholder `UNIQUE_REFERENCE` with Postman {{$guid}}
    public static final String POSTMAN_GUID = "postmanGuid";
    protected Boolean postmanGuid = true;
    // default guid placeholder name
    public static final String POSTMAN_GUID_PLACEHOLDER_NAME = "postmanGuidPlaceholderName";
    protected static final String POSTMAN_GUID_PLACEHOLDER_NAME_DEFAULT_VALUE = "UNIQUE_REFERENCE";
    protected static String postmanGuidPlaceholderName = POSTMAN_GUID_PLACEHOLDER_NAME_DEFAULT_VALUE;
    // replace placeholder `ISO_TIMESTAMP` with Postman {{isoTimestamp}}
    protected Boolean postmanIsoTimestamp = true;
    public static final String POSTMAN_ISO_TIMESTAMP = "postmanIsoTimestamp";
    public static final String POSTMAN_ISO_TIMESTAMP_PLACEHOLDER_NAME = "postmanIsoTimestampPlaceholderName";
    public static final String POSTMAN_ISO_TIMESTAMP_PLACEHOLDER_NAME_DEFAULT_VALUE = "ISO_TIMESTAMP";
    protected static String postmanIsoTimestampPlaceholderName = POSTMAN_ISO_TIMESTAMP_PLACEHOLDER_NAME_DEFAULT_VALUE;

    public static final String REQUEST_PARAMETER_GENERATION = "requestParameterGeneration";
    public static final String REQUEST_PARAMETER_GENERATION_DEFAULT_VALUE = "Example";

    public String folderStrategy = FOLDER_STRATEGY_DEFAULT_VALUE; // values: Paths | Tags
    protected Boolean pathParamsAsVariables = PATH_PARAMS_AS_VARIABLES_DEFAULT_VALUE; // values: true | false

    // Output file
    public String postmanFile = POSTMAN_FILE_DEFAULT_VALUE;

    // Select whether to generate requests/responses from Example or Schema
    protected String requestParameterGeneration = REQUEST_PARAMETER_GENERATION_DEFAULT_VALUE; // values: Example, Schema

    public Set<PostmanVariable> variables = new HashSet<>();

    public static final String JSON_ESCAPE_DOUBLE_QUOTE = "\\\"";
    public static final String JSON_ESCAPE_NEW_LINE = "\\n";


    // operations grouped by tag
    protected Map<String, List<CodegenOperation>> codegenOperationsByTag = new HashMap<>();
    // list of operations
    protected List<CodegenOperation> codegenOperationsList = new ArrayList<>();

    /**
     * Configures the type of generator.
     *
     * @return  the CodegenType for this generator
     * @see     org.openapitools.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return null;
    }


    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "postman-collection";
    }

    public PostmanCollectionCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        embeddedTemplateDir = templateDir = "postman-collection";
        supportingFiles.add(
                new SupportingFile("postman.mustache", "", postmanFile)
        );

        cliOptions.clear();
        cliOptions.add(CliOption.newString(FOLDER_STRATEGY, "whether to create folders according to the spec’s paths or tags"));
        cliOptions.add(CliOption.newBoolean(PATH_PARAMS_AS_VARIABLES, "whether to create Postman variables for path parameters"));
        cliOptions.add(CliOption.newString(POSTMAN_VARIABLES, "whether to convert placeholders (i.e. {{VAR_1}}) into Postman variables"));
        cliOptions.add(CliOption.newString(POSTMAN_GUID, "whether to convert placeholders (i.e. {{UNIQUE_REFERENCE}}) into Postman formula {{$guid}}"));
        cliOptions.add(CliOption.newString(POSTMAN_GUID_PLACEHOLDER_NAME, "name of the placeholder (i.e. {{UNIQUE_REFERENCE}}) to replace with Postman formula {{$guid}}"));
        cliOptions.add(CliOption.newString(POSTMAN_ISO_TIMESTAMP, "whether to convert placeholders (i.e. {{ISO_TIMESTAMP}}) into Postman formula {{$isoTimestamp}}"));
        cliOptions.add(CliOption.newString(POSTMAN_ISO_TIMESTAMP_PLACEHOLDER_NAME, "name of the placeholder (i.e. {{ISO_TIMESTAMP}}) to replace with Postman formula {{$isoTimestamp}}"));
        cliOptions.add(CliOption.newString(REQUEST_PARAMETER_GENERATION, "whether to generate the request parameters based on the schema or the examples"));

    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        if(pathParamsAsVariables && parameter.isPathParam) {
            variables.add(new PostmanVariable()
                    .addName(parameter.paramName)
                    .addType(mapToPostmanType(parameter.dataType))
                    .addeDefaultValue(parameter.defaultValue));
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        this.additionalProperties().put("formattedDescription", formatDescription(openAPI.getInfo().getDescription()));
    }

    @Override
    public List<CodegenServerVariable> fromServerVariables(Map<String, ServerVariable> variables) {

        if(variables != null){
            variables.entrySet().stream().forEach(serverVariableEntry -> this.variables.add(new PostmanVariable()
                    .addName(serverVariableEntry.getKey())
                    .addType("string")
                    .addeDefaultValue(serverVariableEntry.getValue().getDefault())));
        }

        return super.fromServerVariables(variables);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if(additionalProperties().containsKey(FOLDER_STRATEGY)) {
            folderStrategy = additionalProperties().get(FOLDER_STRATEGY).toString();
        }

        if (additionalProperties.containsKey(PATH_PARAMS_AS_VARIABLES)) {
            pathParamsAsVariables = Boolean.parseBoolean(additionalProperties.get(PATH_PARAMS_AS_VARIABLES).toString());
        }

        if(additionalProperties().containsKey(REQUEST_PARAMETER_GENERATION)) {
            requestParameterGeneration = additionalProperties().get(REQUEST_PARAMETER_GENERATION).toString();
        }

        if(additionalProperties().containsKey(POSTMAN_VARIABLES)) {
            postmanVariables = Boolean.parseBoolean(additionalProperties.get(POSTMAN_VARIABLES).toString());
        }

        if(additionalProperties().containsKey(POSTMAN_GUID)) {
            postmanGuid = Boolean.parseBoolean(additionalProperties.get(POSTMAN_GUID).toString());
        }

        if(additionalProperties().containsKey(POSTMAN_GUID_PLACEHOLDER_NAME)) {
            postmanGuidPlaceholderName = additionalProperties.get(POSTMAN_GUID_PLACEHOLDER_NAME).toString();
        }

        if(additionalProperties().containsKey(POSTMAN_ISO_TIMESTAMP)) {
            postmanIsoTimestamp = Boolean.parseBoolean(additionalProperties.get(POSTMAN_ISO_TIMESTAMP).toString());
        }

        if(additionalProperties().containsKey(POSTMAN_ISO_TIMESTAMP_PLACEHOLDER_NAME)) {
            postmanIsoTimestampPlaceholderName = additionalProperties.get(POSTMAN_ISO_TIMESTAMP_PLACEHOLDER_NAME).toString();
        }

        super.vendorExtensions().put("variables", variables);

        if(folderStrategy.equalsIgnoreCase("tags")) {
            this.additionalProperties().put("codegenOperationsByTag", codegenOperationsByTag);
        } else {
            this.additionalProperties().put("codegenOperationsList", codegenOperationsList);
        }

    }

    /**
     * Process and modify operations before generating code
     */
    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationsMap results = super.postProcessOperationsWithModels(objs, allModels);

        OperationMap ops = results.getOperations();
        List<CodegenOperation> opList = ops.getOperation();

        for(CodegenOperation codegenOperation : opList) {

            if(pathParamsAsVariables) {
                codegenOperation.path = doubleCurlyBraces(codegenOperation.path);
            }

            codegenOperation.summary = getSummary(codegenOperation);

            // request headers
            if(codegenOperation.produces != null && codegenOperation.produces.get(0) != null) {
                // produces mediaType as `Accept` header (use first mediaType only)
                String mediaType = codegenOperation.produces.get(0).get("mediaType");
                CodegenParameter acceptHeader = new CodegenParameter();
                acceptHeader.baseName = "Accept";
                acceptHeader.paramName = "Accept";
                CodegenProperty schema = new CodegenProperty();
                schema.defaultValue = mediaType;
                acceptHeader.setSchema(schema);
                codegenOperation.headerParams.add(0, acceptHeader);
            }

            if(codegenOperation.consumes != null && codegenOperation.consumes.get(0) != null) {
                // consumes mediaType as `Content-Type` header (use first mediaType only)
                String mediaType = codegenOperation.consumes.get(0).get("mediaType");
                CodegenParameter contentTypeHeader = new CodegenParameter();

                contentTypeHeader.baseName = "Content-Type";
                contentTypeHeader.paramName = "Content-Type";
                CodegenProperty schema = new CodegenProperty();
                schema.defaultValue = mediaType;
                contentTypeHeader.setSchema(schema);
                codegenOperation.headerParams.add(0, contentTypeHeader);
            }

            // build pathSegments
            String[] pathSegments = codegenOperation.path.substring(1).split("/");
            codegenOperation.vendorExtensions.put("pathSegments", pathSegments);
            codegenOperation.responses.stream().forEach(r -> r.vendorExtensions.put("pathSegments", pathSegments));

            List<PostmanRequestItem> postmanRequests = getPostmanRequests(codegenOperation);
            if(postmanRequests != null) {
                if(postmanVariables) {
                    postmanRequests = createPostmanVariables(postmanRequests);
                }
                if(postmanGuid) {
                    postmanRequests = setPostmanGuid(postmanRequests);
                }
                if(postmanIsoTimestamp) {
                    postmanRequests = setPostmanIsoTimestamp(postmanRequests);
                }
                codegenOperation.vendorExtensions.put("postmanRequests", postmanRequests);
            }

            // set all available responses
            for(CodegenResponse codegenResponse : codegenOperation.responses) {

                codegenResponse.vendorExtensions.put("status", getStatus(codegenResponse));

//        TODO: set response for each request
//        if(postmanRequests != null) {
//          // re-use request body for each response
//          codegenResponse.vendorExtensions.put("requestBody", postmanRequests);
//        }
//        String responseBody = getResponseBody(codegenResponse);
//        if(responseBody != null) {
//          codegenResponse.vendorExtensions.put("responseBody", responseBody);
//          codegenResponse.vendorExtensions.put("hasResponseBody", true);
//        } else {
//          codegenResponse.vendorExtensions.put("hasResponseBody", false);
//        }

            }

            if(folderStrategy.equalsIgnoreCase("tags")) {
                addToMap(codegenOperation);
            } else {
                addToList(codegenOperation);
            }

        }

        return results;
    }


    /**
     * Add the CodegenOperation to the map that is passed to the Mustache templates
     * The map groups the CodegenOperations by tag as defined in the OpenAPI spec
     * @param codegenOperation Codegen operation instance
     */
    void addToMap(CodegenOperation codegenOperation){

        String key = null;
        if(codegenOperation.tags == null || codegenOperation.tags.isEmpty()) {
            key = "default";
        } else {
            key = codegenOperation.tags.get(0).getName();
        }

        List<CodegenOperation> list = codegenOperationsByTag.get(key);

        if(list == null) {
            list = new ArrayList<>();
        }
        list.add(codegenOperation);

        codegenOperationsByTag.put(key, list);

    }

    void addToList(CodegenOperation codegenOperation) {
        codegenOperationsList.add(codegenOperation);
    }

    String getResponseBody(CodegenResponse codegenResponse) {
        String responseBody = "";

        if(codegenResponse.getContent() != null && codegenResponse.getContent().get("application/json") != null &&
                codegenResponse.getContent().get("application/json").getExamples() != null) {
            // find in components/examples
            String exampleRef = codegenResponse.getContent().get("application/json").getExamples()
                    .values().iterator().next().get$ref();
            if(exampleRef != null) {
                Example example = this.openAPI.getComponents().getExamples().get(extractExampleByName(exampleRef));
                responseBody = getJsonFromExample(example);
            }
        } else if(codegenResponse.getContent() != null) {
            // find in context examples
            Map<String, Example> maxExamples = codegenResponse.getContent().get("application/json").getExamples();
            if(maxExamples != null && maxExamples.values().iterator().hasNext()) {
                responseBody = getJsonFromExample(maxExamples.values().iterator().next());
            }
        }

        return responseBody;
    }

    // from OpenAPI operation to n Postman requests
    List<PostmanRequestItem> getPostmanRequests(CodegenOperation codegenOperation) {
        List<PostmanRequestItem> items = new ArrayList<>();

        if(codegenOperation.getHasBodyParam()) {
            // operation with bodyParam
            if (requestParameterGeneration.equalsIgnoreCase("Schema")) {
                // get from schema
                items.add(new PostmanRequestItem(codegenOperation.summary, getJsonFromSchema(codegenOperation.bodyParam)));
            } else {
                // get from examples
                if (codegenOperation.bodyParam.example != null) {
                    // find in bodyParam example
                    items.add(new PostmanRequestItem(codegenOperation.summary, formatJson(codegenOperation.bodyParam.example)));
                } else if (codegenOperation.bodyParam.getContent().get("application/json") != null &&
                        codegenOperation.bodyParam.getContent().get("application/json").getExamples() != null) {
                    // find in components/examples
                    for (Map.Entry<String, Example> entry : codegenOperation.bodyParam.getContent().get("application/json").getExamples().entrySet()) {
                        String exampleRef = entry.getValue().get$ref();
                        Example example = this.openAPI.getComponents().getExamples().get(extractExampleByName(exampleRef));
                        String exampleAsString = getJsonFromExample(example);

                        items.add(new PostmanRequestItem(example.getSummary(), exampleAsString));
                    }
                } else if (codegenOperation.bodyParam.getSchema() != null) {
                    // find in schema example
                    String exampleAsString = formatJson(codegenOperation.bodyParam.getSchema().getExample());
                    items.add(new PostmanRequestItem(codegenOperation.summary, exampleAsString));
                } else {
                    // example not found
                    // get from schema
                    items.add(new PostmanRequestItem(codegenOperation.summary, getJsonFromSchema(codegenOperation.bodyParam)));

                }
            }
        } else {
            // operation without bodyParam
            items.add(new PostmanRequestItem(codegenOperation.summary, ""));
        }

        return items;
    }

    // from placeholders (ie {{VAR_1}}) create Postman variables
    public List<PostmanRequestItem> createPostmanVariables(List<PostmanRequestItem> postmanRequests) {

        for(PostmanRequestItem requestItem : postmanRequests) {
            for(String var: extractPlaceholders(requestItem.getBody())) {
                variables.add(new PostmanVariable()
                        .addName(var)
                        .addType("string")
                        .addeDefaultValue(""));
            }
        }

        return postmanRequests;
    }

    // replace postmanGuid placeholder (ie {{UNIQUE_REFERENCE}}) with Postman formula {{$guid}}
    List<PostmanRequestItem> setPostmanGuid(List<PostmanRequestItem> postmanRequests) {

        for(PostmanRequestItem requestItem : postmanRequests) {
            requestItem.setBody(requestItem.getBody().replace("{{" + postmanGuidPlaceholderName + "}}", "{{$guid}}"));
        }

        return postmanRequests;
    }

    // replace postman placeholder (ie {{ISO_TIMESTAMP}}) with Postman formula {{$isoTimestamp}}
    List<PostmanRequestItem> setPostmanIsoTimestamp(List<PostmanRequestItem> postmanRequests) {

        for(PostmanRequestItem requestItem : postmanRequests) {
            requestItem.setBody(requestItem.getBody().replace("{{" + postmanIsoTimestampPlaceholderName + "}}", "{{$isoTimestamp}}"));
        }

        return postmanRequests;
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a Postman collection (format v2.1.0) JSON file";
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;  // add an underscore to the name
    }

    /**
     * override with any special text escaping logic to handle unsafe
     * characters so as to avoid code injection
     *
     * @param input String to be cleaned up
     * @return string with unsafe characters removed or escaped
     */
    @Override
    public String escapeUnsafeCharacters(String input) {
        //TODO: check that this logic is safe to escape unsafe characters to avoid code injection
        return input;
    }

    /**
     * Return the default value of the property
     * <p>
     * Return null when the default is not defined
     *
     * @param schema Property schema
     * @return string presentation of the default value of the property
     */
    @SuppressWarnings("static-method")
    @Override
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            return schema.getDefault().toString();
        }

        return null;
    }

    /**
     * Escape single and/or double quote to avoid code injection
     *
     * @param input String to be cleaned up
     * @return string with quotation mark removed or escaped
     */
    public String escapeQuotationMark(String input) {
        //TODO: check that this logic is safe to escape quotation mark to avoid code injection
        return input.replace("\"", "\\\"");
    }

    public String doubleCurlyBraces(String str) {

        // remove doublebraces first
        String s = str.replace("{{", "{").replace("}}", "}");
        // change all singlebraces to doublebraces
        s = s.replace("{", "{{").replace("}", "}}");

        return s;

    }

    public String extractExampleByName(String ref) {
        return ref.substring(ref.lastIndexOf("/") + 1);
    }

    public String mapToPostmanType(String openApiDataType) {
        String ret = "any";  // default value

        if(openApiDataType.equalsIgnoreCase("string")) {
            ret = "string";
        } else if(openApiDataType.equalsIgnoreCase("number") ||
                openApiDataType.equalsIgnoreCase("integer")) {
            ret = "number";
        } else if(openApiDataType.equalsIgnoreCase("boolean")) {
            ret = "boolean";
        }

        return ret;
    }

    /**
     * get HTTP Status Code as text
     * @param codegenResponse
     * @return
     */
    String getStatus(CodegenResponse codegenResponse) {
        String ret = "";

        if (codegenResponse.is2xx) {
            if (codegenResponse.code.equalsIgnoreCase("200")) {
                ret = "OK";
            } else if (codegenResponse.code.equalsIgnoreCase("201")) {
                ret = "Created";
            } else {
                ret = "Success";
            }
        } else if (codegenResponse.is3xx) {
            ret = "Redirection";
        }
        if (codegenResponse.is4xx) {
            if (codegenResponse.code.equalsIgnoreCase("400")) {
                ret = "Bad Request";
            } else if (codegenResponse.code.equalsIgnoreCase("401")) {
                ret = "Unauthorized";
            } else if (codegenResponse.code.equalsIgnoreCase("403")) {
                ret = "Forbidden";
            } else if (codegenResponse.code.equalsIgnoreCase("404")) {
                ret = "Not Found";
            } else if (codegenResponse.code.equalsIgnoreCase("409")) {
                ret = "Conflict";
            } else {
                ret = "Client Error";
            }
        }
        if (codegenResponse.is5xx) {
            if (codegenResponse.code.equalsIgnoreCase("500")) {
                ret = "Internal Server Error";
            } else if (codegenResponse.code.equalsIgnoreCase("501")) {
                ret = "Not Implemented";
            } else {
                ret = "Server Error";
            }
        }

        return ret;
    }

    // make sure operation name is always set
    String getSummary(CodegenOperation codegenOperation) {
        String ret = null;

        if(codegenOperation.summary != null) {
            ret = codegenOperation.summary;
        } else if (codegenOperation.operationId != null) {
            ret = codegenOperation.operationId;
        } else {
            ret = codegenOperation.httpMethod;
        }
        return ret;
    }

    /**
     * Format text to include in JSON file
     * @param description Text to format
     * @return Formatted text
     */
    public String formatDescription(String description) {
        if (description != null) {
            description = description.replace("\n", JSON_ESCAPE_NEW_LINE);
            description = description.replace("\"", JSON_ESCAPE_DOUBLE_QUOTE);
        }

        return description;
    }

    /**
     * Extract all placeholders (string delimited by curly braces ie {{PLACEHOLDER}}) from the input string
     * @param input String containing the placeholders
     * @return Set of placeholders found in the string
     */
    public Set<String> extractPlaceholders(String input) {
        Set<String> variables = new HashSet<>();

        Pattern pattern = Pattern.compile("\\{\\{([^}]*)\\}\\}");
        Matcher matcher = pattern.matcher(input);

        while(matcher.find()) {
            if(postmanGuidPlaceholderName.equalsIgnoreCase(matcher.group(1))) {
                // skip if it is postmanGuid placeholder
                break;
            }
            if(postmanIsoTimestampPlaceholderName.equalsIgnoreCase(matcher.group(1))) {
                // skip if it is postmanIsoTimestamp placeholder
                break;
            }
            if(isPostmanDynamicVariable(matcher.group(1))) {
                // skip if it is reserved words
                break;
            }

            variables.add(matcher.group(1));
        }

        return variables;
    }

    public boolean isPostmanDynamicVariable(String value) {
        boolean ret = false;

        if(value.equals("$guid") || value.equals("$timestamp")) {
            ret = true;
        }

        return ret;
    }

    // Supporting helpers
    public String getJsonFromSchema(CodegenParameter codegenParameter) {

        String ret = "{" + JSON_ESCAPE_NEW_LINE + " ";

        int numVars = codegenParameter.vars.size();
        int counter = 1;

        for (CodegenProperty codegenProperty : codegenParameter.vars) {
            ret = ret + JSON_ESCAPE_DOUBLE_QUOTE + codegenProperty.baseName + JSON_ESCAPE_DOUBLE_QUOTE + ": " +
                    JSON_ESCAPE_DOUBLE_QUOTE + "<" + getPostmanType(codegenProperty) + ">" + JSON_ESCAPE_DOUBLE_QUOTE;

            if(counter < numVars) {
                // add comma unless last attribute
                ret = ret + "," + JSON_ESCAPE_NEW_LINE + " ";
            }
            counter++;

        }

        ret = ret + JSON_ESCAPE_NEW_LINE + "}";

        return ret;
    }

    public String getJsonFromExample(Example example) {
        String ret = "";

        if(example == null) {
            return ret;
        }

        if(example.getValue() instanceof ObjectNode) {
            ret = convertToJson((ObjectNode)example.getValue());
        } else if(example.getValue() instanceof LinkedHashMap) {
            ret = convertToJson((LinkedHashMap)example.getValue());
        }

        return ret;
    }

    // array of attributes from JSON payload (ignore commas within quotes)
    public String[] getAttributes(String json) {
        return json.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1);
    }

    public String convertToJson(ObjectNode objectNode) {
        return formatJson(objectNode.toString());
    }

    // convert to JSON (string) escaping and formatting
    public String convertToJson(LinkedHashMap<String, Object> linkedHashMap) {
        String ret = "";

        return traverseMap(linkedHashMap, ret);
    }

    public String formatJson(String json) {

        ObjectMapper objectMapper = new ObjectMapper();

        try {
            // convert to JSON object and prettify
            JsonNode actualObj = objectMapper.readTree(json);
            json = Json.pretty(actualObj);
            json = json.replace("\"", JSON_ESCAPE_DOUBLE_QUOTE);
            json = json.replace("\n", JSON_ESCAPE_NEW_LINE);

        } catch (JsonProcessingException e) {
            LOGGER.warn("Error formatting JSON", e);
            json = "";
        }

        return json;
    }

    // traverse recursively
    private String traverseMap(LinkedHashMap<String, Object> linkedHashMap, String ret) {

        ret = ret + "{" + JSON_ESCAPE_NEW_LINE + " ";

        int numVars = linkedHashMap.entrySet().size();
        int counter = 1;

        for (Map.Entry<String, Object> mapElement : linkedHashMap.entrySet()) {
            String key = mapElement.getKey();
            Object value = mapElement.getValue();

            if(value instanceof String) {
                // unescape double quotes already escaped
                value = ((String)value).replace("\\\"", "\"");

                ret = ret + JSON_ESCAPE_DOUBLE_QUOTE + key + JSON_ESCAPE_DOUBLE_QUOTE + ": " +
                        JSON_ESCAPE_DOUBLE_QUOTE + value + JSON_ESCAPE_DOUBLE_QUOTE;
            } else if (value instanceof Integer) {
                ret = ret + JSON_ESCAPE_DOUBLE_QUOTE + key + JSON_ESCAPE_DOUBLE_QUOTE + ": " +
                        value;
            } else if (value instanceof LinkedHashMap) {
                String in = ret + JSON_ESCAPE_DOUBLE_QUOTE + key + JSON_ESCAPE_DOUBLE_QUOTE + ": ";
                ret = traverseMap(((LinkedHashMap<String, Object>) value),  in);
            } else {
                LOGGER.warn("Value type unrecognised: " + value.getClass());
            }

            if(counter < numVars) {
                // add comma unless last attribute
                ret = ret + "," + JSON_ESCAPE_NEW_LINE + " ";
            }
            counter++;
        }

        ret = ret + JSON_ESCAPE_NEW_LINE + "}";

        return ret;
    }

    public String getPostmanType(CodegenProperty codegenProperty) {
        if(codegenProperty.isNumeric) {
            return "number";
        } else if(codegenProperty.isDate) {
            return "date";
        } else {
            return "string";
        }
    }

    // Supporting models
    public class PostmanRequestItem {

        private String name;
        private String body;

        public PostmanRequestItem() {
        }

        public PostmanRequestItem(String name, String body) {
            this.name = name;
            this.body = body;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getBody() {
            return body;
        }

        public void setBody(String body) {
            this.body = body;
        }
    }

    class PostmanVariable {

        private String name;
        private String type;
        private String defaultValue;

        public PostmanVariable addName(String name) {
            this.name = name;
            return this;
        }

        public PostmanVariable addType(String type) {
            this.type = type;
            return this;
        }

        public PostmanVariable addeDefaultValue(String defaultValue) {
            this.defaultValue = defaultValue;
            return this;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getDefaultValue() {
            return defaultValue;
        }

        public void setDefaultValue(String defaultValue) {
            this.defaultValue = defaultValue;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            PostmanVariable that = (PostmanVariable) o;
            return name.equals(that.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }

        @Override
        public String toString() {
            return "PostmanVariable{" +
                    "name='" + name + '\'' +
                    ", type='" + type + '\'' +
                    ", defaultValue='" + defaultValue + '\'' +
                    '}';
        }
    }

}

