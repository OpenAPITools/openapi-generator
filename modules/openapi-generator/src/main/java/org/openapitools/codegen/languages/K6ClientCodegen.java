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

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.dashize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import javax.annotation.Nullable;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.text.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Mustache.Lambda;
import com.samskivert.mustache.Template;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;

public class K6ClientCodegen extends DefaultCodegen implements CodegenConfig {

    // K6 vendor extension - operation grouping - group operations and define their
    // ordering, to allow for e.g. scenario testing
    private static final String X_OPERATION_GROUPING = "x-k6-openapi-operation-grouping";

    // K6 vendor extension - operation response - for now, allows to hide given
    // operation response, so that in case of multiple 2xx responses, generated
    // script checks only against e.g. code 200 responses
    private static final String X_OPERATION_RESPONSE = "x-k6-openapi-operation-response";
    private static final String X_OPERATION_RESPONSE_HIDE = "hide";

    // K6 vendor extension - extract data from operation - allows to specify path to
    // value in body of response which should be extracted and assigned to variable
    // for later use by other operations
    private static final String X_OPERATION_DATAEXTRACT = "x-k6-openapi-operation-dataextract";
    private static final String X_OPERATION_DATAEXTRACT_OPERATION_ID = "operationId"; // denotes ID of operation whose response body contains value to be extracted
    private static final String X_OPERATION_DATAEXTRACT_VALUE_PATH = "valuePath"; // denotes path to value in body of response which should be extracted
    private static final String X_OPERATION_DATAEXTRACT_PARAMETER_NAME = "parameterName"; // denotes name of parameter to which extracted value should be assigned

    public K6ClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

    }

    static class Parameter {
        String key;
        Object value;
        boolean hasExample;
        boolean initialize;

        public Parameter(String key, Object value) {
            this.key = key;
            this.value = value;
        }

        public Parameter(String key, Object exampleValue, boolean hasExample) {
            this.key = key;
            this.value = exampleValue;
            this.hasExample = hasExample;
        }

        public Parameter(String key, boolean initialize) {
            this.key = key;
            this.initialize = initialize;
        }

        @Override
        public int hashCode() {
            return key.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null || getClass() != obj.getClass())
                return false;
            Parameter p = (Parameter) obj;
            return key.equals(p.key) && value.equals(p.value) && hasExample == p.hasExample
                    && initialize == p.initialize;
        }
    }

    // Stores information specified in `X_OPERATION_GROUPING` K6 vendor extension
    static class OperationGrouping {
        String groupName;
        int order;

        public OperationGrouping(String groupName, int order) {
            this.groupName = groupName;
            this.order = order;
        }
    }

    // Stores information specified in `X_OPERATION_DATAEXTRACT` K6 vendor extension
    static class DataExtractSubstituteParameter {
        String operationId;
        String valuePath;
        String paramName;

        public DataExtractSubstituteParameter(String operationId, String valuePath, String paramName) {
            this.operationId = operationId;
            this.valuePath = valuePath;
            this.paramName = paramName;
        }
    }

    static class ParameterValueLambda implements Mustache.Lambda {
        private static final String NO_EXAMPLE_PARAM_VALUE_PREFIX = "TODO_EDIT_THE_";

        @Override
        public void execute(Template.Fragment fragment, Writer writer) throws IOException {

            // default used if no example is provided
            String noExampleParamValue = String.join("",
                    quoteExample(
                            String.join("", NO_EXAMPLE_PARAM_VALUE_PREFIX, fragment.execute())),
                    ";",
                    " // specify value as there is no example value for this parameter in OpenAPI spec");

            // param has example(s)
            if (fragment.context() instanceof K6ClientCodegen.Parameter
                    && ((K6ClientCodegen.Parameter) fragment.context()).hasExample) {

                Object rawValue = ((K6ClientCodegen.Parameter) fragment.context()).value;

                // handle as 'examples'
                if (rawValue instanceof Map) {

                    @SuppressWarnings("unchecked")
                    Set<String> exampleValues = ((Map<String, Example>) rawValue).values().stream()
                    .map(x -> quoteExample(
                            StringEscapeUtils.escapeEcmaScript(
                                    String.valueOf(x.getValue()))))
                    .collect(Collectors.toCollection(TreeSet::new));

                    if (!exampleValues.isEmpty()) {

                        writer.write(String.join("",
                                Arrays.toString(exampleValues.toArray()),
                                ".shift();",
                                " // first element from list extracted from 'examples' field defined at the parameter level of OpenAPI spec"));

                    } else {
                        writer.write(noExampleParamValue);
                    }

                // handle as (single) 'example'
                } else {
                    writer.write(String.join("",
                            quoteExample(
                                    StringEscapeUtils.escapeEcmaScript(
                                            String.valueOf(
                                                    ((K6ClientCodegen.Parameter) fragment.context()).value))),
                            ";",
                            " // extracted from 'example' field defined at the parameter level of OpenAPI spec"));
                }

            // param needs to be initialized for subsequent data extraction - see `X_OPERATION_DATAEXTRACT` K6 vendor extension
            } else if (fragment.context() instanceof K6ClientCodegen.Parameter
                    && ((K6ClientCodegen.Parameter) fragment.context()).initialize) {

                writer.write(String.join("",
                        "null",
                        ";",
                        " // parameter initialized for subsequent data extraction"));

            } else {
                writer.write(noExampleParamValue);
            }
        }

        private static String quoteExample(String exampleValue) {
            return StringUtils.wrap(exampleValue, "'");
        }
    }

    static class HTTPBody {
        List<Parameter> parameters;

        public HTTPBody(List<Parameter> parameters) {
            this.parameters = parameters;
        }
    }

    static class HTTPParameters {
        @Nullable
        String auth;
        @Nullable
        List<Parameter> cookies;
        @Nullable
        List<Parameter> headers;
        @Nullable
        List<Parameter> jar;
        @Nullable
        Integer redirects;
        @Nullable
        List<Parameter> tags;
        @Nullable
        Integer timeout;
        @Nullable
        String compression;
        @Nullable
        String responseType;

        public HTTPParameters(@Nullable String auth, @Nullable List<Parameter> cookies,
                              @Nullable List<Parameter> headers, @Nullable List<Parameter> jar, @Nullable Integer redirects,
                              @Nullable List<Parameter> tags, @Nullable Integer timeout, @Nullable String compression,
                              @Nullable String responseType) {
            this.auth = auth;
            this.cookies = cookies;
            this.headers = headers;
            this.jar = jar;
            this.redirects = redirects;
            this.tags = tags;
            this.timeout = timeout;
            this.compression = compression;
            this.responseType = responseType;
        }
    }

    static class k6Check {
        Integer status;
        String description;

        public k6Check(Integer status, String description) {
            this.status = status;
            this.description = description;
        }
    }

    static class HTTPRequest {
        String method;
        boolean isDelete;
        String path;
        @Nullable
        List<Parameter> query;
        @Nullable
        HTTPBody body;
        boolean hasBodyExample;
        @Nullable
        HTTPParameters params;
        @Nullable
        List<k6Check> k6Checks;
        @Nullable
        DataExtractSubstituteParameter dataExtract;

        public HTTPRequest(String method, String path, @Nullable List<Parameter> query, @Nullable HTTPBody body,
                boolean hasBodyExample, @Nullable HTTPParameters params, @Nullable List<k6Check> k6Checks,
                DataExtractSubstituteParameter dataExtract) {
            // NOTE: https://k6.io/docs/javascript-api/k6-http/del-url-body-params
            this.method = method.equals("delete") ? "del" : method;
            this.isDelete = method.equals("delete");
            this.path = path;
            this.query = query;
            this.body = body;
            this.hasBodyExample = hasBodyExample;
            this.params = params;
            this.k6Checks = k6Checks;
            this.dataExtract = dataExtract;
        }
    }

    static public class HTTPRequestGroup {
        String groupName;
        Set<Parameter> variables; // query and path parameters
        List<HTTPRequest> requests;
        private final Map<Integer, HTTPRequest> requestsMap;

        public HTTPRequestGroup(String groupName, Set<Parameter> variables, Map<Integer, HTTPRequest> requestsMap) {
            this.groupName = groupName;
            this.variables = variables;
            this.requestsMap = requestsMap;
            this.requests = sortRequests(requestsMap);
        }

        private void addRequests(Map<Integer, HTTPRequest> moreRequests) {
            this.requestsMap.putAll(moreRequests);
            this.requests = sortRequests(this.requestsMap);
        }

        private void addVariables(Set<Parameter> moreVariables) {
            this.variables.addAll(moreVariables);
        }

        private List<HTTPRequest> sortRequests(Map<Integer, HTTPRequest> requestsMap) {
            return new ArrayList<>(new TreeMap<>(requestsMap).values());
        }
    }

    private final Logger LOGGER = LoggerFactory.getLogger(K6ClientCodegen.class);

    public static final String PROJECT_NAME = "projectName";
    public static final String MODULE_NAME = "moduleName";
    public static final String PROJECT_DESCRIPTION = "projectDescription";
    public static final String PROJECT_VERSION = "projectVersion";
    public static final String BASE_URL = "baseURL";
    public static final String PRESERVE_LEADING_PARAM_CHAR = "preserveLeadingParamChar";
    static final Collection<String> INVOKER_PKG_SUPPORTING_FILES = Arrays.asList("script.mustache", "README.mustache");
    static final String[][] JAVASCRIPT_SUPPORTING_FILES = {
            new String[]{"script.mustache", "script.js"}, new String[]{"README.mustache", "README.md"}};

    protected String projectName;
    protected String moduleName;
    protected String projectDescription;
    protected String projectVersion;
    protected String licenseName;

    protected String invokerPackage;
    protected String sourceFolder = "";
    private String modelPropertyNaming = "camelCase";
    protected boolean preserveLeadingParamChar = false;

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "k6";
    }

    @Override
    public String getHelp() {
        return "Generates a k6 script (beta).";
    }

    @Override
    public void processOpts() {
        embeddedTemplateDir = templateDir = "k6";

        super.processOpts();

        if (additionalProperties.containsKey(PROJECT_NAME)) {
            setProjectName(((String) additionalProperties.get(PROJECT_NAME)));
        }
        if (additionalProperties.containsKey(MODULE_NAME)) {
            setModuleName(((String) additionalProperties.get(MODULE_NAME)));
        }
        if (additionalProperties.containsKey(PROJECT_DESCRIPTION)) {
            setProjectDescription(((String) additionalProperties.get(PROJECT_DESCRIPTION)));
        }
        if (additionalProperties.containsKey(PROJECT_VERSION)) {
            setProjectVersion(((String) additionalProperties.get(PROJECT_VERSION)));
        }
        if (additionalProperties.containsKey(CodegenConstants.LICENSE_NAME)) {
            setLicenseName(((String) additionalProperties.get(CodegenConstants.LICENSE_NAME)));
        }
        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }
        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
        }
        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming((String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }
        boolean preserveLeadingParamChar = convertPropertyToBooleanAndWriteBack(PRESERVE_LEADING_PARAM_CHAR);
        this.setPreserveLeadingParamChar(preserveLeadingParamChar);
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        if (openAPI.getInfo() != null) {
            Info info = openAPI.getInfo();
            if (StringUtils.isBlank(projectName) && info.getTitle() != null) {
                // when projectName is not specified, generate it from info.title
                projectName = sanitizeName(dashize(info.getTitle()));
            }
            if (StringUtils.isBlank(projectVersion)) {
                // when projectVersion is not specified, use info.version
                projectVersion = escapeUnsafeCharacters(escapeQuotationMark(info.getVersion()));
            }
            if (projectDescription == null) {
                // when projectDescription is not specified, use info.description
                projectDescription = sanitizeName(info.getDescription());
            }

            // when licenceName is not specified, use info.license
            if (additionalProperties.get(CodegenConstants.LICENSE_NAME) == null && info.getLicense() != null) {
                License license = info.getLicense();
                licenseName = license.getName();
            }
        }

        // default values
        if (StringUtils.isBlank(projectName)) {
            projectName = "swagger-k6-client";
        }
        if (StringUtils.isBlank(moduleName)) {
            moduleName = camelize(underscore(projectName));
        }
        if (StringUtils.isBlank(projectVersion)) {
            projectVersion = "1.0.0";
        }
        if (projectDescription == null) {
            projectDescription = "Client library of " + projectName;
        }
        if (StringUtils.isBlank(licenseName)) {
            licenseName = "Unlicense";
        }

        additionalProperties.put(PROJECT_NAME, projectName);
        additionalProperties.put(MODULE_NAME, moduleName);
        additionalProperties.put(PROJECT_DESCRIPTION, escapeText(projectDescription));
        additionalProperties.put(PROJECT_VERSION, projectVersion);
        additionalProperties.put(CodegenConstants.LICENSE_NAME, licenseName);
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        additionalProperties.put(CodegenConstants.SOURCE_FOLDER, sourceFolder);

        String baseURL = openAPI.getServers().get(0).getUrl();
        for (Server server : openAPI.getServers()) {
            if (server.getUrl().contains("https://")) {
                baseURL = server.getUrl();
            }
        }
        additionalProperties.put(BASE_URL, baseURL);

        // if data is to be extracted from any of the operations' responses, this has to
        // be known prior to executing processing of OpenAPI spec further down
        Map<String, DataExtractSubstituteParameter> dataExtractSubstituteParams = getDataExtractSubstituteParameters(
                openAPI);

        Map<String, HTTPRequestGroup> requestGroups = new HashMap<>();
        Set<Parameter> extraParameters = new HashSet<>();
        Map<String, Set<Parameter>> pathVariables = new HashMap<>();

        for (String path : openAPI.getPaths().keySet()) {
            Map<Integer, HTTPRequest> requests = new HashMap<>();
            Set<Parameter> variables = new HashSet<>();

            String groupName = path;

            for (Map.Entry<PathItem.HttpMethod, Operation> methodOperation : openAPI.getPaths().get(path).
                    readOperationsMap().entrySet()) {
                List<Parameter> httpParams = new ArrayList<>();
                List<Parameter> queryParams = new ArrayList<>();
                List<Parameter> bodyOrFormParams = new ArrayList<>();
                List<k6Check> k6Checks = new ArrayList<>();
                Set<String> imports = new HashSet<>();

                final Operation operation = methodOperation.getValue();
                final PathItem.HttpMethod method = methodOperation.getKey();
                OptionalInt operationGroupingOrder = OptionalInt.empty();

                String operationId = operation.getOperationId();

                boolean hasRequestBodyExample = false;

                // optionally group and order operations - see `X_OPERATION_GROUPING` K6 vendor
                // extension
                final CodegenOperation cgOperation = super.fromOperation(path, method.name(), operation, null);
                Optional<OperationGrouping> operationGrouping = extractOperationGrouping(cgOperation);
                if (operationGrouping.isPresent()) {
                    groupName = operationGrouping.get().groupName;
                    operationGroupingOrder = OptionalInt.of(operationGrouping.get().order);
                }

                for (Map.Entry<String, ApiResponse> resp : operation.getResponses().entrySet()) {
                    String statusData = resp.getKey().equals("default") ? "200" : resp.getKey();

                    // optionally hide given response - see `X_OPERATION_RESPONSE` K6 vendor
                    // extension
                    // i.e. in case of multiple 2xx responses, generated script checks only against
                    // e.g. code 200 responses
                    boolean hideOperationResponse = shouldHideOperationResponse(resp.getValue());

                    int status = Integer.parseInt(statusData);
                    if (!hideOperationResponse && (status >= 200 && status < 300)) {
                        k6Checks.add(new k6Check(status, resp.getValue().getDescription()));
                    }
                }

                if (hasBodyParameter(openAPI, operation) || hasFormParameter(openAPI, operation)) {
                    String defaultContentType = hasFormParameter(openAPI, operation) ? "application/x-www-form-urlencoded" : "application/json";
                    List<String> consumes = new ArrayList<>(getConsumesInfo(openAPI, operation));
                    String contentTypeValue = consumes.isEmpty() ? defaultContentType : consumes.get(0);
                    if (contentTypeValue.equals("*/*"))
                        contentTypeValue = "application/json";
                    Parameter contentType = new Parameter("Content-Type", getDoubleQuotedString(contentTypeValue));
                    httpParams.add(contentType);

                    RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());

                    // extract request body example, if present
                    hasRequestBodyExample = hasRequestBodyExample(requestBody, contentTypeValue);
                    if (hasRequestBodyExample) {
                        extractRequestBodyExample(requestBody, contentTypeValue, bodyOrFormParams);
                    }

                    for (Map.Entry<String, ApiResponse> responseEntry : operation.getResponses().entrySet()) {
                        CodegenResponse r = fromResponse(responseEntry.getKey(), responseEntry.getValue());
                        if (r.baseType != null &&
                                !defaultIncludes.contains(r.baseType) &&
                                !languageSpecificPrimitives.contains(r.baseType)) {
                            imports.add(r.baseType);
                        }
                    }

                    // if we have at least one request body example, we do not need to construct these dummies
                    if (!hasRequestBodyExample) {
                        List<CodegenParameter> formParameters = fromRequestBodyToFormParameters(requestBody, imports);
                        for (CodegenParameter parameter : formParameters) {
                            String reference = "";
                            if (parameter.isModel) {
                                Schema nestedSchema = ModelUtils.getSchema(openAPI, parameter.baseType);
                                CodegenModel model = fromModel(parameter.paramName, nestedSchema);
                                reference = generateNestedModelTemplate(model);
                                if (parameter.dataType.equals("List")) {
                                    reference = "[" + reference + "]";
                                }
                            }

                            Parameter k6Parameter;
                            if (parameter.dataType.equals("File")) {
                                k6Parameter = new Parameter(parameter.paramName,
                                        "http.file(open(\"/path/to/file.bin\", \"b\"), \"test.bin\")");
                            } else {
                                k6Parameter = new Parameter(parameter.paramName, !reference.isEmpty() ? reference
                                        : getDoubleQuotedString(parameter.dataType.toLowerCase(Locale.ROOT)));
                            }

                            bodyOrFormParams.add(k6Parameter);
                        }
                    }
                }

                String accepts = getAccept(openAPI, operation);
                String responseType = getDoubleQuotedString(accepts);

                try {

                    for (io.swagger.v3.oas.models.parameters.Parameter parameter : operation.getParameters()) {
                        switch (parameter.getIn()) {
                            case "header":
                                httpParams.add(new Parameter(parameter.getName(), getTemplateString(toVarName(parameter.getName()))));
                                extraParameters.add(new Parameter(toVarName(parameter.getName()), parameter.getName().toUpperCase(Locale.ROOT)));
                                break;
                            case "path":
                            case "query":
                                if (parameter.getIn().equals("query"))
                                    queryParams.add(new Parameter(parameter.getName(), getTemplateVariable(parameter.getName())));
                                if (!pathVariables.containsKey(path)) {
                                    // use 'example' field defined at the parameter level of OpenAPI spec
                                    if (Objects.nonNull(parameter.getExample())) {
                                        variables.add(new Parameter(toVarName(parameter.getName()),
                                                parameter.getExample(), true));

                                    // use 'examples' field defined at the parameter level of OpenAPI spec
                                    } else if (Objects.nonNull(parameter.getExamples())) {
                                        variables.add(new Parameter(toVarName(parameter.getName()),
                                                parameter.getExamples(), true));

                                    // no example provided, generated script will contain placeholder value
                                    } else {
                                        variables.add(new Parameter(toVarName(parameter.getName()),
                                                parameter.getName().toUpperCase(Locale.ROOT)));
                                    }
                                }
                                break;
                            default:
                                break;
                        }
                    }
                } catch (NullPointerException e) {
                    LOGGER.error(e.getMessage(), e);
                }

                pathVariables.put(groupName, variables);

                final HTTPParameters params = new HTTPParameters(null, null, httpParams, null, null, null, null, null,
                        responseType.length() > 0 ? responseType : null);

                assert params.headers != null;

                // check if data needs to be extracted from response of this operation
                Optional<DataExtractSubstituteParameter> dataExtract = getDataExtractSubstituteParameter(
                        dataExtractSubstituteParams, operationId);

                // calculate order for this current request
                Integer requestOrder = calculateRequestOrder(operationGroupingOrder, requests.size());

                requests.put(requestOrder, new HTTPRequest(method.toString().toLowerCase(Locale.ROOT), path,
                        queryParams.size() > 0 ? queryParams : null,
                        bodyOrFormParams.size() > 0 ? new HTTPBody(bodyOrFormParams) : null, hasRequestBodyExample,
                        params.headers.size() > 0 ? params : null, k6Checks.size() > 0 ? k6Checks : null,
                        dataExtract.orElse(null)));
            }

            addOrUpdateRequestGroup(requestGroups, groupName, pathVariables.get(groupName), requests);
        }

        for (HTTPRequestGroup requestGroup : requestGroups.values()) {
            for (HTTPRequest request : requestGroup.requests) {
                if (request.path.contains("/{")) {
                    request.path = request.path.replace("/{", "/${");
                }
            }

            // any variables not defined yet but used for subsequent data extraction must be
            // initialized
            initializeDataExtractSubstituteParameters(dataExtractSubstituteParams, requestGroup);
        }

        additionalProperties.put("requestGroups", requestGroups.values());
        additionalProperties.put("extra", extraParameters);

        for (String[] supportingTemplateFile : JAVASCRIPT_SUPPORTING_FILES) {
            String templateFile = supportingTemplateFile[0];
            String folder;
            if (INVOKER_PKG_SUPPORTING_FILES.contains(templateFile))
                // #1150: script.js must be generated to invokerPackage, otherwise
                // nothing works!
                folder = createPath(sourceFolder, invokerPackage);
            else
                folder = "";
            supportingFiles.add(new SupportingFile(templateFile, folder, supportingTemplateFile[1]));
        }
    }

    private String generateNestedModelTemplate(CodegenModel model) {
        StringBuilder reference = new StringBuilder();
        int modelEntrySetSize = model.getAllVars().size();
        for (CodegenProperty property : model.getAllVars()) {
            reference.append(getDoubleQuotedString(property.name)).append(": ").append(getDoubleQuotedString(property.dataType.toLowerCase(Locale.ROOT)));
            if (modelEntrySetSize > 1)
                reference.append(", ");
        }
        reference = new StringBuilder("{" + reference + "}");
        reference = new StringBuilder(reference.toString().replace(", }", "}"));
        return reference.toString();
    }

    private String getTemplateVariable(String input) {
        return "${" + input + "}";
    }

    public String getModelPropertyNaming() {
        return this.modelPropertyNaming;
    }

    private String getNameUsingModelPropertyNaming(String name) {
        switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
            case original:
                return name;
            case camelCase:
                return camelize(name, true);
            case PascalCase:
                return camelize(name);
            case snake_case:
                return underscore(name);
            default:
                throw new IllegalArgumentException("Invalid model property naming '" +
                        name + "'. Must be 'original', 'camelCase', " +
                        "'PascalCase' or 'snake_case'");
        }
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);  // FIXME parameter should not be assigned. Also declare it as "final"

        if ("_".equals(name)) {
            name = "_u";
        }

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = getNameUsingModelPropertyNaming(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    private String getTemplateString(String input) {
        return "`" + getTemplateVariable(input) + "`";
    }

    private String getDoubleQuotedString(String input) {
        return "\"" + input + "\"";
    }

    /**
     * Concatenates an array of path segments into a path string.
     *
     * @param segments The path segments to concatenate. A segment may contain
     *                 either of the file separator characters '\' or '/'. A segment
     *                 is ignored if it is <code>null</code>, empty or
     *                 &quot;.&quot;.
     * @return A path string using the correct platform-specific file separator
     * character.
     */
    private String createPath(String... segments) {
        StringBuilder buf = new StringBuilder();
        for (String segment : segments) {
            if (!StringUtils.isEmpty(segment) && !segment.equals(".")) {
                if (buf.length() != 0)
                    buf.append(File.separatorChar);
                buf.append(segment);
            }
        }
        for (int i = 0; i < buf.length(); i++) {
            char c = buf.charAt(i);
            if ((c == '/' || c == '\\') && c != File.separatorChar)
                buf.setCharAt(i, File.separatorChar);
        }
        return buf.toString();
    }

    @Override
    public String apiFileFolder() {
        return createPath(outputFolder, sourceFolder, invokerPackage, apiPackage());
    }

    public String getInvokerPackage() {
        return invokerPackage;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public void setProjectDescription(String projectDescription) {
        this.projectDescription = projectDescription;
    }

    public void setProjectVersion(String projectVersion) {
        this.projectVersion = projectVersion;
    }

    public void setLicenseName(String licenseName) {
        this.licenseName = licenseName;
    }

    public void setModelPropertyNaming(String naming) {
        if ("original".equals(naming) || "camelCase".equals(naming) || "PascalCase".equals(naming)
                || "snake_case".equals(naming)) {
            this.modelPropertyNaming = naming;
        } else {
            throw new IllegalArgumentException("Invalid model property naming '" + naming
                    + "'. Must be 'original', 'camelCase', " + "'PascalCase' or 'snake_case'");
        }
    }

    public void setPreserveLeadingParamChar(boolean preserveLeadingParamChar) {
        this.preserveLeadingParamChar = preserveLeadingParamChar;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ', " to avoid code injection
        return input.replace("\"", "").replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    private static String getAccept(OpenAPI openAPI, Operation operation) {
        String accepts = null;
        String defaultContentType = "application/json";
        Set<String> producesInfo = getProducesInfo(openAPI, operation);
        if (producesInfo != null && !producesInfo.isEmpty()) {
            ArrayList<String> produces = new ArrayList<>(producesInfo);
            StringBuilder sb = new StringBuilder();
            for (String produce : produces) {
                if (defaultContentType.equalsIgnoreCase(produce)) {
                    accepts = defaultContentType;
                    break;
                } else {
                    if (sb.length() > 0) {
                        sb.append(",");
                    }
                    sb.append(produce);
                }
            }
            if (accepts == null) {
                accepts = sb.toString();
            }
        } else {
            accepts = defaultContentType;
        }

        return accepts;
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas().put("handleParamValue", new ParameterValueLambda());
    }

    /**
     * We're iterating over paths but grouping requests across paths, therefore
     * these need to be aggregated.
     *
     * @param requestGroups
     * @param groupName
     * @param variables
     * @param requests
     */
    private void addOrUpdateRequestGroup(Map<String, HTTPRequestGroup> requestGroups, String groupName,
            Set<Parameter> variables, Map<Integer, HTTPRequest> requests) {
        if (requestGroups.containsKey(groupName)) {
            HTTPRequestGroup existingHTTPRequestGroup = requestGroups.get(groupName);
            existingHTTPRequestGroup.addRequests(requests);
            existingHTTPRequestGroup.addVariables(variables);
        } else {
            requestGroups.put(groupName, new HTTPRequestGroup(groupName, variables, requests));
        }
    }

    /**
     * If `X_OPERATION_DATAEXTRACT` K6 vendor extension is present, extract info
     * from it.
     *
     * @param openAPI
     * @return
     */
    private Map<String, DataExtractSubstituteParameter> getDataExtractSubstituteParameters(OpenAPI openAPI) {
        Map<String, DataExtractSubstituteParameter> dataExtractSubstituteParams = new HashMap<>();

        for (String path : openAPI.getPaths().keySet()) {
            for (Map.Entry<PathItem.HttpMethod, Operation> methodOperation : openAPI.getPaths().get(path)
                    .readOperationsMap().entrySet()) {

                final PathItem.HttpMethod method = methodOperation.getKey();
                final Operation operation = methodOperation.getValue();
                final CodegenOperation cgOperation = super.fromOperation(path, method.name(), operation, null);

                if (cgOperation.getHasVendorExtensions()
                        && cgOperation.vendorExtensions.containsKey(X_OPERATION_DATAEXTRACT)
                        && cgOperation.vendorExtensions.get(X_OPERATION_DATAEXTRACT) instanceof java.util.Map) {

                    Optional<DataExtractSubstituteParameter> dataExtractSubstituteParameter = getDataExtractSubstituteParameter(
                            (Map<?, ?>) cgOperation.vendorExtensions.get(X_OPERATION_DATAEXTRACT));

                    // TODO: add support for extracting data for multiple params
                    dataExtractSubstituteParameter.ifPresent(extractSubstituteParameter -> dataExtractSubstituteParams.put(extractSubstituteParameter.operationId,
                            extractSubstituteParameter));
                }

            }
        }

        return dataExtractSubstituteParams;
    }

    /**
     * Optionally, retrieve information specified in the `X_OPERATION_DATAEXTRACT`
     * K6 vendor extension
     *
     * @param xOperationDataExtractProperties
     * @return optional as only returned if all required info is present
     */
    private Optional<DataExtractSubstituteParameter> getDataExtractSubstituteParameter(
            Map<?, ?> xOperationDataExtractProperties) {

        Optional<String> operationId = Optional.empty();
        Optional<String> valuePath = Optional.empty();
        Optional<String> parameterName = Optional.empty();

        for (Map.Entry<?, ?> xOperationDataExtractPropertiesEntry : xOperationDataExtractProperties.entrySet()) {

            switch (String.valueOf(xOperationDataExtractPropertiesEntry.getKey())) {
            case X_OPERATION_DATAEXTRACT_OPERATION_ID:
                operationId = Optional.of(String.valueOf(xOperationDataExtractPropertiesEntry.getValue()));
                continue;

            case X_OPERATION_DATAEXTRACT_VALUE_PATH:
                valuePath = Optional.of(String.valueOf(xOperationDataExtractPropertiesEntry.getValue()));
                continue;

            case X_OPERATION_DATAEXTRACT_PARAMETER_NAME:
                parameterName = Optional.of(String.valueOf(xOperationDataExtractPropertiesEntry.getValue()));
            }
        }

        if (operationId.isPresent() && valuePath.isPresent() && parameterName.isPresent()) {
            return Optional
                    .of(new DataExtractSubstituteParameter(operationId.get(), valuePath.get(), parameterName.get()));

        } else {
            return Optional.empty();
        }
    }

    /**
     * Optionally, retrieve data extraction properties for given operation
     *
     * @param dataExtractSubstituteParams
     * @param operationId
     * @return optional as only returned if present for given operation
     */
    private Optional<DataExtractSubstituteParameter> getDataExtractSubstituteParameter(
            Map<String, DataExtractSubstituteParameter> dataExtractSubstituteParams, String operationId) {

        return (!dataExtractSubstituteParams.isEmpty() && dataExtractSubstituteParams.containsKey(operationId))
                ? Optional.of(dataExtractSubstituteParams.get(operationId))
                : Optional.empty();
    }

    /**
     * Optionally, retrieve information specified in the `X_OPERATION_GROUPING` K6
     * vendor extension
     *
     * @param cgOperation
     * @return optional as only returned if required info is present
     */
    private Optional<OperationGrouping> extractOperationGrouping(CodegenOperation cgOperation) {
        Optional<OperationGrouping> operationGrouping = Optional.empty();

        if (cgOperation.getHasVendorExtensions() && cgOperation.vendorExtensions.containsKey(X_OPERATION_GROUPING)
                && cgOperation.vendorExtensions.get(X_OPERATION_GROUPING) instanceof java.util.Map) {

            Map.Entry<?, ?> operationGroupingEntry = ((Map<?, ?>) cgOperation.vendorExtensions
                    .get(X_OPERATION_GROUPING)).entrySet().stream().findFirst().get();

            return Optional.of(new OperationGrouping(String.valueOf(operationGroupingEntry.getKey()),
                    Integer.parseInt(String.valueOf(operationGroupingEntry.getValue()))));
        }

        return operationGrouping;
    }

    /**
     * If `X_OPERATION_RESPONSE` K6 vendor extension is present, check if given
     * operation response should be hidden.
     *
     * @param resp
     * @return true if should be hidden, false otherwise
     */
    private boolean shouldHideOperationResponse(ApiResponse resp) {
        boolean hideOperationResponse = false;

        if (Objects.nonNull(resp.getExtensions()) && !resp.getExtensions().isEmpty()
                && resp.getExtensions().containsKey(X_OPERATION_RESPONSE)) {

            Map<?, ?> respExtensions = (Map<?, ?>) resp.getExtensions().get(X_OPERATION_RESPONSE);
            Entry<?, ?> entry = respExtensions.entrySet().stream().findFirst().orElse(null);

            if (entry != null && entry.getKey().equals(X_OPERATION_RESPONSE_HIDE)) {
                return Boolean.parseBoolean(String.valueOf(entry.getValue()));
            }
        }

        return false;
    }

    /**
     * Check if example is present for given request body and content type.
     *
     * @param requestBody
     * @param contentTypeValue
     * @return true if present, false otherwise
     */
    private boolean hasRequestBodyExample(RequestBody requestBody, String contentTypeValue) {
        return (Objects.nonNull(requestBody.getContent()) && requestBody.getContent().containsKey(contentTypeValue)
                && Objects.nonNull(requestBody.getContent().get(contentTypeValue).getExamples())
                && !requestBody.getContent().get(contentTypeValue).getExamples().isEmpty());
    }

    /**
     * Extract example for given request body.
     *
     * @param requestBody
     * @param contentTypeValue
     * @param bodyOrFormParams
     */
    private void extractRequestBodyExample(RequestBody requestBody, String contentTypeValue,
            List<Parameter> bodyOrFormParams) {

        Optional<Map.Entry<String, Example>> requestBodyExampleEntry = requestBody.getContent().get(contentTypeValue)
                .getExamples().entrySet().stream().findFirst();

        if (requestBodyExampleEntry.isPresent()) {

            Example requestBodyExample = requestBodyExampleEntry.get().getValue();

            try {
                JsonNode requestBodyExampleValueJsonNode = Json.mapper()
                        .readTree(String.valueOf(requestBodyExample.getValue()));

                Iterator<Map.Entry<String, JsonNode>> fields = requestBodyExampleValueJsonNode.fields();
                while (fields.hasNext()) {
                    Map.Entry<String, JsonNode> fieldsEntry = fields.next();

                    JsonNode exampleValueAsJsonNode = fieldsEntry.getValue();

                    Parameter k6Parameter = new Parameter(fieldsEntry.getKey(),
                            exampleValueAsJsonNode.isNumber() ? exampleValueAsJsonNode.asText()
                                    : exampleValueAsJsonNode.toString());

                    bodyOrFormParams.add(k6Parameter);
                }

            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }
    }

    /**
     * Calculate order for this current request
     *
     * @param operationGroupingOrder
     * @param requestsSize
     * @return request order
     */
    private Integer calculateRequestOrder(OptionalInt operationGroupingOrder, int requestsSize) {
        int requestOrder;

        if (operationGroupingOrder.isPresent()) {
            requestOrder = operationGroupingOrder.getAsInt() - 1;

        } else {
            switch (requestsSize) {
            case 0:
            case 1:
                requestOrder = requestsSize;
                break;

            default:
                requestOrder = (requestsSize - 1);
                break;
            }
        }

        return requestOrder;
    }

    //
    /**
     * Any variables not defined yet but used for subsequent data extraction must be
     * initialized
     *
     * @param dataExtractSubstituteParams
     * @param requestGroup
     */
    private void initializeDataExtractSubstituteParameters(
            Map<String, DataExtractSubstituteParameter> dataExtractSubstituteParams, HTTPRequestGroup requestGroup) {

        if (!dataExtractSubstituteParams.isEmpty()) {
            Set<String> existingVariablesNames = requestGroup.variables.stream().map(v -> v.key)
                    .collect(Collectors.toSet());

            Set<DataExtractSubstituteParameter> initializeVariables = dataExtractSubstituteParams.values().stream()
                    .filter(p -> !existingVariablesNames.contains(toVarName(p.paramName))).collect(Collectors.toSet());

            for (DataExtractSubstituteParameter initializeVariable : initializeVariables) {
                requestGroup.variables.add(new Parameter(toVarName(initializeVariable.paramName), true));
            }
        }
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.K_SIX; }
}
