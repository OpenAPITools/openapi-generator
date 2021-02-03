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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nullable;
import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.*;

public class K6ClientCodegen extends DefaultCodegen implements CodegenConfig {

    public K6ClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

    }

    static class Parameter {
        String key;
        Object value;

        public Parameter(String key, Object value) {
            this.key = key;
            this.value = value;
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
            return key.equals(p.key) && value.equals((String) p.value);
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
        String path;
        @Nullable
        List<Parameter> query;
        @Nullable
        HTTPBody body;
        @Nullable
        HTTPParameters params;
        @Nullable
        List<k6Check> k6Checks;

        public HTTPRequest(String method, String path, @Nullable List<Parameter> query, @Nullable HTTPBody body,
                           @Nullable HTTPParameters params, @Nullable List<k6Check> k6Checks) {
            // NOTE: https://k6.io/docs/javascript-api/k6-http/del-url-body-params
            this.method = method.equals("delete") ? "del" : method;
            this.path = path;
            this.query = query;
            this.body = body;
            this.params = params;
            this.k6Checks = k6Checks;
        }
    }

    static public class HTTPRequestGroup {
        String groupName;
        Set<Parameter> variables; // query and path parameters
        List<HTTPRequest> requests;

        public HTTPRequestGroup(String groupName, Set<Parameter> variables, List<HTTPRequest> requests) {
            this.groupName = groupName;
            this.variables = variables;
            this.requests = requests;
        }
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(JavascriptClientCodegen.class);

    public static final String PROJECT_NAME = "projectName";
    public static final String MODULE_NAME = "moduleName";
    public static final String PROJECT_DESCRIPTION = "projectDescription";
    public static final String PROJECT_VERSION = "projectVersion";
    public static final String BASE_URL = "baseURL";
    public static final String PRESERVE_LEADING_PARAM_CHAR = "preserveLeadingParamChar";
    static final Collection<String> INVOKER_PKG_SUPPORTING_FILES = Arrays.asList("script.mustache", "README.mustache");
    static final String[][] JAVASCRIPT_SUPPORTING_FILES = new String[][]{
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

        List<HTTPRequestGroup> requestGroups = new ArrayList<>();
        Set<Parameter> extraParameters = new HashSet<>();
        Map<String, Set<Parameter>> pathVariables = new HashMap<>();

        for (String path : openAPI.getPaths().keySet()) {
            List<HTTPRequest> requests = new ArrayList<>();
            Set<Parameter> variables = new HashSet<>();

            for (Map.Entry<PathItem.HttpMethod, Operation> methodOperation : openAPI.getPaths().get(path).
                    readOperationsMap().entrySet()) {
                List<Parameter> httpParams = new ArrayList<>();
                List<Parameter> queryParams = new ArrayList<>();
                List<Parameter> bodyOrFormParams = new ArrayList<>();
                List<k6Check> k6Checks = new ArrayList<>();
                Set<String> imports = new HashSet<String>();

                final Operation operation = methodOperation.getValue();
                final PathItem.HttpMethod method = methodOperation.getKey();

                for (Map.Entry<String, ApiResponse> resp : operation.getResponses().entrySet()) {
                    String statusData = resp.getKey().equals("default") ? "200" : resp.getKey();
                    int status = Integer.parseInt(statusData);
                    if (status >= 200 && status < 300) {
                        k6Checks.add(new k6Check(status, resp.getValue().getDescription()));
                    }
                }

                if (hasBodyParameter(openAPI, operation) || hasFormParameter(openAPI, operation)) {
                    String defaultContentType = hasFormParameter(openAPI, operation) ? "application/x-www-form-urlencoded" : "application/json";
                    List<String> consumes = new ArrayList<>(getConsumesInfo(openAPI, operation));
                    String contentTypeValue = consumes == null || consumes.isEmpty() ? defaultContentType : consumes.get(0);
                    if (contentTypeValue.equals("*/*"))
                        contentTypeValue = "application/json";
                    Parameter contentType = new Parameter("Content-Type", getDoubleQuotedString(contentTypeValue));
                    httpParams.add(contentType);

                    RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());

                    for (Map.Entry<String, ApiResponse> responseEntry : operation.getResponses().entrySet()) {
                        CodegenResponse r = fromResponse(responseEntry.getKey(), responseEntry.getValue());
                        if (r.baseType != null &&
                                !defaultIncludes.contains(r.baseType) &&
                                !languageSpecificPrimitives.contains(r.baseType)) {
                            imports.add(r.baseType);
                        }
                    }

                    List<CodegenParameter> formParameteres = fromRequestBodyToFormParameters(requestBody, imports);
                    for (CodegenParameter parameter : formParameteres) {
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
                                variables.add(new Parameter(toVarName(parameter.getName()), parameter.getName().toUpperCase(Locale.ROOT)));
                                break;
                            default:
                                break;
                        }
                    }
                } catch (NullPointerException e) {

                }

                pathVariables.put(path, variables);

                final HTTPParameters params = new HTTPParameters(null, null, httpParams, null, null, null, null, null,
                        responseType.length() > 0 ? responseType : null);

                assert params.headers != null;
                requests.add(new HTTPRequest(method.toString().toLowerCase(Locale.ROOT), path,
                        queryParams.size() > 0 ? queryParams : null,
                        bodyOrFormParams.size() > 0 ? new HTTPBody(bodyOrFormParams) : null,
                        params.headers.size() > 0 ? params : null, k6Checks.size() > 0 ? k6Checks : null));
            }
            requestGroups.add(new HTTPRequestGroup(path, pathVariables.get(path), requests));
        }

        for (HTTPRequestGroup requestGroup : requestGroups) {
            for (HTTPRequest request : requestGroup.requests) {
                if (request.path.contains("/{")) {
                    request.path = request.path.replace("/{", "/${");
                }
            }
        }

        additionalProperties.put("requestGroups", requestGroups);
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

    //
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

        // if it's all uppper case, do nothing
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
}
