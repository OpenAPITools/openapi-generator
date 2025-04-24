/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.examples.Example;
import lombok.Getter;
import lombok.Setter;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/*
Note : This code has been MASSIVELY inspired by PostmanCollectionCodegen from @gcatanese.

Hopefully one day we can merge the similar code, as both generators stabilize
 */
public class JetbrainsHttpClientClientCodegen extends DefaultCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(JetbrainsHttpClientClientCodegen.class);

    public static final String JSON_ESCAPE_NEW_LINE = "\n";
    public static final String JSON_ESCAPE_DOUBLE_QUOTE = "\"";

    public static final String REQUEST_PARAMETER_GENERATION_DEFAULT_VALUE = "Example";
    protected String requestParameterGeneration = REQUEST_PARAMETER_GENERATION_DEFAULT_VALUE; // values: Example, Schema


    public static final String PROJECT_NAME = "Jetbrains HTTP Client";

    public static final String BODY_VARIABLES = "bodyVariables";

    public List<String> bodyVariables = new ArrayList<>();

    public static final String CUSTOM_HEADERS = "customHeaders";

    public List<String> customHeaders = new ArrayList<>();

    // A map is nice, because that way I easily override variables across APIs, for pagination for example. This should add nice defaults
    private final Map<String, Object> customVariables = new HashMap<>();


    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "jetbrains-http-client";
    }

    @Override
    public String getHelp() {
        return "Generates a jetbrains-http client. See https://www.jetbrains.com/help/idea/http-client-in-product-code-editor.html";
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.HTTP;
    }

    public JetbrainsHttpClientClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();

        outputFolder = "generated-code" + File.separator + "jetbrains-http-client";
        apiTemplateFiles.put("api.mustache", ".http");
        embeddedTemplateDir = templateDir = "jetbrains-http-client";
        apiPackage = "Apis";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("http-client.template.env.mustache", "Apis", "http-client.template.env.json"));


        cliOptions.clear();
        cliOptions.add(CliOption.newString(BODY_VARIABLES, "whether to convert body placeholders (i.e. VAR_1) into variables (i.e. {{VAR_1}})"));
        cliOptions.add(CliOption.newString(CUSTOM_HEADERS, "custom headers that can be set for each request. Can be used for unsupported features, for example auth methods like oauth."));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        var additionalProperties = additionalProperties();

        if (additionalProperties.containsKey(BODY_VARIABLES)) {
            bodyVariables = Arrays.asList(additionalProperties.get(BODY_VARIABLES).toString().split("-"));
        }

        if (additionalProperties.containsKey(CUSTOM_HEADERS)) {
            customHeaders = Arrays.asList(additionalProperties.get(CUSTOM_HEADERS).toString().split("&"));
        }

        bodyVariables.forEach(variable -> customVariables.put(variable, ""));
        for (String header : customHeaders) {
            List<String> variables = extractDoubleCurlyBraces(header);
            if (!variables.isEmpty()) {
                variables.forEach(v -> customVariables.put(v, ""));
            }
        }
    }

    @Override
    protected ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas() {

        return super.addMustacheLambdas()
                .put("doubleMustache", new DoubleMustacheLambda());
    }

    public static class DoubleMustacheLambda implements Mustache.Lambda {
        @Override
        public void execute(Template.Fragment fragment, Writer writer) throws IOException {
            String text = fragment.execute();
            writer.write(text
                    .replaceAll("\\{", "{{")
                    .replaceAll("}", "}}")
            );
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationsMap results = super.postProcessOperationsWithModels(objs, allModels);

        OperationMap ops = results.getOperations();
        List<CodegenOperation> opList = ops.getOperation();

        for (CodegenOperation codegenOperation : opList) {
            List<RequestItem> requests = getRequests(codegenOperation);

            if (requests != null) {
                codegenOperation.vendorExtensions.put("requests", requests);
                //Adding to each operation for now, we may be smarter later on
                codegenOperation.vendorExtensions.put("customHeaders", customHeaders);
            }
        }

        return results;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        var variables = new ArrayList<>(customVariables.keySet());
        objs.put("vendorExtensionsVariables", variables);
        return objs;
    }

    List<RequestItem> getRequests(CodegenOperation codegenOperation) {
        List<RequestItem> items = new ArrayList<>();

        if (codegenOperation.getHasBodyParam()) {
            // operation with bodyParam
            if (requestParameterGeneration.equalsIgnoreCase("Schema")) {
                // get from schema
                items.add(new RequestItem(codegenOperation.summary, getJsonFromSchema(codegenOperation.bodyParam)));
            } else {
                // get from examples
                if (codegenOperation.bodyParam.example != null) {
                    // find in bodyParam example
                    items.add(new RequestItem(codegenOperation.summary, formatJson(codegenOperation.bodyParam.example)));
                } else if (codegenOperation.bodyParam.getContent().get("application/json") != null &&
                        codegenOperation.bodyParam.getContent().get("application/json").getExamples() != null) {
                    // find in components/examples
                    for (Map.Entry<String, Example> entry : codegenOperation.bodyParam.getContent().get("application/json").getExamples().entrySet()) {
                        String exampleRef = entry.getValue().get$ref();
                        if (exampleRef != null) {
                            Example example = this.openAPI.getComponents().getExamples().get(extractExampleByName(exampleRef));
                            String exampleAsString = getJsonFromExample(example);
                            items.add(new RequestItem(example.getSummary(), exampleAsString));
                        }
                    }
                } else if (codegenOperation.bodyParam.getSchema() != null) {
                    // find in schema example
                    String exampleAsString = (codegenOperation.bodyParam.getSchema().getExample());
                    items.add(new RequestItem(codegenOperation.summary, exampleAsString));
                } else {
                    // example not found
                    // get from schema
                    items.add(new RequestItem(codegenOperation.summary, getJsonFromSchema(codegenOperation.bodyParam)));

                }
            }
        } else {
            // operation without bodyParam
            items.add(new RequestItem(codegenOperation.summary, null));
        }

        codegenOperation.headerParams.forEach(param -> customVariables.put(param.baseName, ""));
        codegenOperation.queryParams.forEach(param -> customVariables.put(param.paramName, ""));

        // I also need to grab the parameters from the path
        List<String> pathVariables = extractSingleCurlyBraces(codegenOperation.path);
        pathVariables.forEach(pv -> customVariables.put(pv, ""));

        // Handling custom variables now
        return handleCustomVariablesInRequests(items);
    }

    public static List<String> extractDoubleCurlyBraces(String input) {
        List<String> result = new ArrayList<>();
        Pattern pattern = Pattern.compile("\\{\\{([^}]+)\\}\\}");
        Matcher matcher = pattern.matcher(input);

        while (matcher.find()) {
            result.add(matcher.group(1));
        }

        return result;
    }

    public static List<String> extractSingleCurlyBraces(String input) {
        List<String> result = new ArrayList<>();
        Pattern pattern = Pattern.compile("\\{([^}]+)\\}");
        Matcher matcher = pattern.matcher(input);

        while (matcher.find()) {
            result.add(matcher.group(1));
        }

        return result;
    }


    private List<RequestItem> handleCustomVariablesInRequests(List<RequestItem> items) {
        if (!bodyVariables.isEmpty()) {
            for (var item : items) {
                for (var customVariable : bodyVariables) {
                    var body = item.getBody();
                    if (body != null) {
                        body = body.replace(customVariable, "{{" + customVariable + "}}");
                        item.setBody(body);
                    }
                }
            }
        }

        return items;
    }


    @Override
    public void postProcess() {
        System.out.println("##########################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                                    #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                                    #");
        System.out.println("#                                                                                        #");
        System.out.println("# This generator was written by Julien Lengrand-Lambert (https://github.com/jlengrand)   #");
        System.out.println("##########################################################################################");
    }

    @Getter
    @Setter
    public class RequestItem {

        private String name;
        private String body;

        public RequestItem(String name, String body) {
            this.name = name;
            this.body = body;
        }
    }

    /*
    Helpers
     */

    public String getJsonFromSchema(CodegenParameter codegenParameter) {

        String ret = "{" + JSON_ESCAPE_NEW_LINE + " ";

        int numVars = codegenParameter.vars.size();
        int counter = 1;

        for (CodegenProperty codegenProperty : codegenParameter.vars) {
            ret = ret + JSON_ESCAPE_DOUBLE_QUOTE + codegenProperty.baseName + JSON_ESCAPE_DOUBLE_QUOTE + ": " +
                    JSON_ESCAPE_DOUBLE_QUOTE + "<" + getType(codegenProperty) + ">" + JSON_ESCAPE_DOUBLE_QUOTE;

            if (counter < numVars) {
                // add comma unless last attribute
                ret = ret + "," + JSON_ESCAPE_NEW_LINE + " ";
            }
            counter++;

        }

        ret = ret + JSON_ESCAPE_NEW_LINE + "}";

        return ret;
    }

    public String getType(CodegenProperty codegenProperty) {
        if (codegenProperty.isNumeric) {
            return "number";
        } else if (codegenProperty.isDate) {
            return "date";
        } else {
            return "string";
        }
    }

    public String formatJson(String json) {

        ObjectMapper objectMapper = new ObjectMapper();

        try {
            // convert to JSON object and prettify
            JsonNode actualObj = objectMapper.readTree(json);
            json = Json.pretty(actualObj);

        } catch (JsonProcessingException e) {
            LOGGER.warn("Error formatting JSON", e);
            json = "";
        }

        return json;
    }

    public String extractExampleByName(String ref) {
        return ref.substring(ref.lastIndexOf("/") + 1);
    }

    public String getJsonFromExample(Example example) {
        String ret = "";

        if (example == null) {
            return ret;
        }

        if (example.getValue() instanceof ObjectNode) {
            ret = convertToJson((ObjectNode) example.getValue());
        } else if (example.getValue() instanceof LinkedHashMap) {
            ret = convertToJson((LinkedHashMap) example.getValue());
        }

        return ret;
    }

    public String convertToJson(ObjectNode objectNode) {
        return formatJson(objectNode.toString());
    }

    public String convertToJson(LinkedHashMap<String, Object> linkedHashMap) {
        String ret = "";

        return traverseMap(linkedHashMap, ret);
    }

    private String traverseMap(LinkedHashMap<String, Object> linkedHashMap, String ret) {

        ret = ret + "{" + JSON_ESCAPE_NEW_LINE + " ";

        int numVars = linkedHashMap.entrySet().size();
        int counter = 1;

        for (Map.Entry<String, Object> mapElement : linkedHashMap.entrySet()) {
            String key = mapElement.getKey();
            Object value = mapElement.getValue();

            if (value instanceof String) {
                // unescape double quotes already escaped
                value = ((String) value).replace("\\\"", "\"");

                ret = ret + JSON_ESCAPE_DOUBLE_QUOTE + key + JSON_ESCAPE_DOUBLE_QUOTE + ": " +
                        JSON_ESCAPE_DOUBLE_QUOTE + value + JSON_ESCAPE_DOUBLE_QUOTE;
            } else if (value instanceof Integer || value instanceof Boolean) {
                ret = ret + JSON_ESCAPE_DOUBLE_QUOTE + key + JSON_ESCAPE_DOUBLE_QUOTE + ": " +
                        value;
            } else if (value instanceof HashMap) {
                String in = ret + JSON_ESCAPE_DOUBLE_QUOTE + key + JSON_ESCAPE_DOUBLE_QUOTE + ": ";
                ret = traverseMap(((LinkedHashMap<String, Object>) value), in);
            } else if (value instanceof List) {
                // TODO : What if it's a list of objects? _urgh_
                var items = ((List<?>) value);
                StringBuilder jsonBuilder = new StringBuilder("[");

                for (int i = 0; i < items.size(); i++) {
                    jsonBuilder.append(JSON_ESCAPE_DOUBLE_QUOTE).append(items.get(i)).append(JSON_ESCAPE_DOUBLE_QUOTE);
                    if (i < items.size() - 1) {
                        jsonBuilder.append(",");
                    }
                }
                jsonBuilder.append("]");

                ret = ret + JSON_ESCAPE_DOUBLE_QUOTE + key + JSON_ESCAPE_DOUBLE_QUOTE + ": " + jsonBuilder;
            } else {
                LOGGER.warn("Value type unrecognised: " + value.getClass());
                //WARNING: here we are undoing what is done in "add comma unless last attribute"
                // This is meant to avoid dangling commas if we encounter an unknown type
                ret = ret.substring(0, ret.length() - 3);
            }

            if (counter < numVars) {
                // add comma unless last attribute
                ret = ret + "," + JSON_ESCAPE_NEW_LINE + " ";
            }
            counter++;
        }

        ret = ret + JSON_ESCAPE_NEW_LINE + "}";

        return ret;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input;
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input;
    }
}