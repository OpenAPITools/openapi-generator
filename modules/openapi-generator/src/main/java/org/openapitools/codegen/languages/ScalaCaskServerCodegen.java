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
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.serializer.SerializerUtils;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ScalaCaskServerCodegen extends AbstractScalaCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    private final Logger LOGGER = LoggerFactory.getLogger(ScalaCaskServerCodegen.class);

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "scala-cask";
    }

    public String getHelp() {
        return "Generates a scala-cask server.";
    }

    protected String artifactVersion = "0.0.1";

    static String ApiServiceTemplate = "apiService.mustache";

    public ScalaCaskServerCodegen() {
        super();

        outputFolder = "generated-code/scala-cask";

        embeddedTemplateDir = templateDir = "scala-cask";
        apiPackage = "Apis";
        modelPackage = "Models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        outputFolder = "generated-code/cask";

        modelTestTemplateFiles.put("modelTest.mustache", ".scala");
        modelTemplateFiles.put("model.mustache", ".scala");
        modelTemplateFiles.put("modelData.mustache", "Data.scala");

        apiTemplateFiles.put("api.mustache", ".scala");
        apiTemplateFiles.put("apiRoutes.mustache", ".scala");
        apiTemplateFiles.put(ApiServiceTemplate, "Service.scala");

        embeddedTemplateDir = templateDir = "scala-cask";

        setReservedWordsLowerCase(
                Arrays.asList(
                        "abstract", "continue", "for", "new", "switch", "assert",
                        "default", "if", "package", "synchronized", "boolean", "do", "goto", "private",
                        "this", "break", "double", "implements", "protected", "throw", "byte", "else",
                        "import", "public", "throws", "case", "enum", "instanceof", "return", "transient",
                        "catch", "extends", "int", "short", "try", "char", "final", "interface", "static",
                        "void", "class", "finally", "long", "strictfp", "volatile", "const", "float",
                        "native", "super", "while", "type")
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList("double",
                        "Int",
                        "Long",
                        "Float",
                        "Double",
                        "char",
                        "float",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float",
                        "List",
                        "Set",
                        "Map")
        );

        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Long");
        //TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "String");

        typeMapping.put("object", "Value");

        cliOptions.add(new CliOption(CodegenConstants.GROUP_ID, CodegenConstants.GROUP_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_ID, CodegenConstants.ARTIFACT_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, CodegenConstants.ARTIFACT_VERSION_DESC));
        cliOptions.add(new CliOption(CodegenConstants.GIT_REPO_ID, CodegenConstants.GIT_REPO_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.GIT_USER_ID, CodegenConstants.GIT_USER_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, CodegenConstants.PACKAGE_DESCRIPTION));
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(ModelUtils.getAdditionalProperties(p));
            return "Map[String, " + inner + "]() ";
        } else if (ModelUtils.isFreeFormObject(p)) {
            // We're opinionated in this template to use ujson
            return "ujson.Null";
        }
        return super.toDefaultValue(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        if (ModelUtils.isFreeFormObject(p)) {
            // We're opinionated in this template to use ujson
            return "Value";
        }
        return super.getSchemaType(p);
    }

    @Override
    public String testPackage() {
        return "jvm/src/test/scala";
    }

    public String toModelTestFilename(String name) {
        String n = super.toModelTestFilename(name);
        return (modelPackage + "." + n).replace('.', '/');
    }

    private String ensureProp(String key, String defaultValue) {
        if (additionalProperties.containsKey(key) && !additionalProperties.get(key).toString().trim().isEmpty()) {
            return (String) additionalProperties.get(key);
        } else {
            additionalProperties.put(key, defaultValue);
            return defaultValue;
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();

        final String groupId = ensureProp(CodegenConstants.GROUP_ID, "org.openapitools");
        ensureProp(CodegenConstants.ARTIFACT_ID, "caskgen");
        artifactVersion = ensureProp(CodegenConstants.ARTIFACT_VERSION, "0.0.1");

        gitRepoId = ensureProp(CodegenConstants.GIT_REPO_ID, "<your git repo -- set 'gitRepoId'>");
        gitUserId = ensureProp(CodegenConstants.GIT_USER_ID, "<your git user -- set 'gitUserId'>");

        String basePackage = ensureProp(CodegenConstants.PACKAGE_NAME, groupId + ".server");
        apiPackage = ensureProp(CodegenConstants.API_PACKAGE, basePackage + ".api");
        modelPackage = ensureProp(CodegenConstants.MODEL_PACKAGE, basePackage + ".model");


        final String apiPath = "jvm/src/main/scala/" + apiPackage.replace('.', '/');
        final String modelPath = "shared/src/main/scala/" + modelPackage.replace('.', '/');

        final List<String> appFullPath = Arrays.stream(apiPath.split("/")).collect(Collectors.toList());
        final String appFolder = String.join("/", appFullPath.subList(0, appFullPath.size() - 1));

        additionalProperties.put("appName", "Cask App");
        additionalProperties.put("appDescription", "A cask service");
        additionalProperties.put("infoUrl", "https://openapi-generator.tech");
        additionalProperties.put("infoEmail", infoEmail);
        additionalProperties.put("licenseInfo", "All rights reserved");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put("openbrackets", "{{");
        additionalProperties.put("closebrackets", "}}");

        supportingFiles.add(new SupportingFile("example.mustache", "example", "Server.scala"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "example", "Dockerfile"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        supportingFiles.add(new SupportingFile("buildAndPublish.yml.mustache", "", ".github/workflows/buildAndPublish.yml"));
        supportingFiles.add(new SupportingFile("build.sc.mustache", "", "build.sc"));
        supportingFiles.add(new SupportingFile(".scalafmt.conf.mustache", "", ".scalafmt.conf"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("appPackage.mustache", appFolder, "package.scala"));
        supportingFiles.add(new SupportingFile("apiPackage.mustache", apiPath, "package.scala"));
        supportingFiles.add(new SupportingFile("modelPackage.mustache", modelPath, "package.scala"));
        supportingFiles.add(new SupportingFile("exampleApp.mustache", appFolder, "ExampleApp.scala"));
        supportingFiles.add(new SupportingFile("baseApp.mustache", appFolder, "BaseApp.scala"));
        supportingFiles.add(new SupportingFile("openapiRoute.mustache", apiPath, "OpenApiRoutes.scala"));
        supportingFiles.add(new SupportingFile("appRoutes.mustache", appFolder, "AppRoutes.scala"));
        supportingFiles.add(new SupportingFile("project/build.properties", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("project/plugins.sbt", "project", "plugins.sbt"));


        instantiationTypes.put("array", "Seq");
        instantiationTypes.put("map", "Map");

        importMapping = new HashMap<String, String>();
        importMapping.put("BigDecimal", "scala.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.time.LocalDate as Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "Map");
        importMapping.put("HashMap", "Map");
        importMapping.put("Array", "Seq");
        importMapping.put("ArrayList", "Seq");
        importMapping.put("List", "Seq");
        importMapping.put("DateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
        importMapping.put("LocalTime", "java.time.LocalTime");
        importMapping.put("Value", "ujson.Value");
    }

    static boolean consumesMimetype(CodegenOperation op, String mimetype) {
        // people don't always/often specify the 'consumes' property, so we assume true when
        // the optional 'consumes' is null or empty
        boolean defaultRetValue = true;

        final List<Map<String, String>> consumes = op.consumes;
        if (consumes != null) {
            for (Map<String, String> c : consumes) {
                final String mt = c.get("mediaType");
                if (mt.equalsIgnoreCase(mimetype)) {
                    return true;
                }
            }
            return false;
        } else {
            return defaultRetValue;
        }
    }


    static String formatMap(Map<?, ?> map) {
        StringBuilder mapAsString = new StringBuilder("{");
        for (Object key : map.keySet().stream().sorted().collect(Collectors.toList())) {
            mapAsString.append(key + " -- " + map.get(key) + ",\n");
        }
        if (mapAsString.length() > 1) {
            mapAsString.delete(mapAsString.length() - 2, mapAsString.length());
        }
        mapAsString.append("}");
        return mapAsString.toString();
    }

    @Override
    public String toApiName(String name) {
        if (name.isEmpty()) {
            return "DefaultApi";
        }
        name = sanitizeName(name);
        return camelize(name);
    }

    @Override
    public String apiFilename(String templateName, String tag) {

        final String suffix = apiTemplateFiles().get(templateName);
        final String fn = toApiFilename(tag);
        if (templateName.equals(ApiServiceTemplate)) {
            return apiInterfaceFileFolder() + '/' + fn + suffix;
        }
        return apiFileFolder() + '/' + fn + "Routes" + suffix;
    }
    @Override
    public String modelFilename(String templateName, String modelName) {
        final String defaultFilename = super.modelFilename(templateName, modelName);
        if (templateName.equals(ApiServiceTemplate)) {
            final String suffix = apiTemplateFiles().get(templateName);
            final String fn = toApiFilename(modelName);
            final String path = modelFileFolder() + '/' + fn + suffix;
            return path;
        } else {
            return defaultFilename;
        }
    }

    @Override
    public String apiFileFolder() {
        final String folder = outputFolder + "/jvm/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);;
        return folder;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/shared/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
    }

    public String apiInterfaceFileFolder() {
        return outputFolder + "/shared/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    static String capitalise(String p) {
        if (p.length() < 2) {
            return p.toUpperCase(Locale.ROOT);
        } else {
            String first = "" + p.charAt(0);
            return first.toUpperCase(Locale.ROOT) + p.substring(1);
        }
    }


    // thanks FlaskConnectionCodeGen
    private static List<Map<String, Object>> getOperations(Map<String, Object> objs) {
        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();
        Map<String, Object> apiInfo = (Map<String, Object>) objs.get("apiInfo");
        List<Map<String, Object>> apis = (List<Map<String, Object>>) apiInfo.get("apis");
        for (Map<String, Object> api : apis) {
            Map<String, Object> operations = (Map<String, Object>) api.get("operations");
            result.add(operations);
        }
        return result;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        List<Map<String, Object>> operations = getOperations(objs);
        for (int i = 0; i < operations.size(); i++) {
            operations.get(i).put("hasMore", i < operations.size() - 1);
        }
        objs.put("operations", operations);
        return super.postProcessSupportingFileData(objs);
    }

    protected String getResourceFolder() {
        String src = getSourceFolder();

        List<String> parts = Arrays.stream(src.split("/", -1)).collect(Collectors.toList());
        if (parts.isEmpty()) {
            return "resources";
        } else {
            String srcMain = String.join("/", parts.subList(0, parts.size() - 1));
            return srcMain + "/resources";
        }
    }

    @Override
    public void processOpenAPI(OpenAPI openAPI) {
        String jsonOpenAPI = SerializerUtils.toJsonString(openAPI);

        try {
            String outputFile = getOutputDir() + "/jvm/" + getResourceFolder() + "/openapi.json";
            FileUtils.writeStringToFile(new File(outputFile), jsonOpenAPI, StandardCharsets.UTF_8);
            LOGGER.info("wrote file to {}", outputFile);
        } catch (Exception e) {
            LOGGER.error(e.getMessage(), e);
        }
    }


    public static class ParamPart {
        final CodegenParameter param;
        final String name;
        final boolean isParam;

        // flag for if there are more path parts
        boolean hasMore;
        // flag for if there are more path parts which are parameters
        boolean hasMoreParams;

        final String conversion;

        public ParamPart(String name, CodegenParameter param) {
            this.name = name;
            this.param = param;
            this.isParam = param != null;
            this.hasMore = true;
            this.conversion = !isParam || param.isString ? "" : ".to" + param.dataType;
        }
    }

    /**
     * Cask will compile but 'initialize' can throw a route overlap exception:
     * <p>
     * {{{
     * Routes overlap with wildcards: get /user/logout, get /user/:username, get /user/login
     * }}}
     * <p>
     * Note: The same error persists even if the suffixes are unique:
     * {{{
     * Routes overlap with wildcards: get /user/logout/3, get /user/:username/1, get /user/login/2
     * }}}
     * <p>
     * To fix this, we need to identify and resolve conflicts in our generated code.
     * <p>
     * # How do we identify conflicts?
     * </p>
     * <p>
     * 1. group routes by their non-param prefixes.
     * <p>
     * 2. add an "x-annotation" vendor extension for operations
     * <p>
     * 3. add a list of "RouteGroups" which can manually delegate as per below
     * <p>
     * <p>
     * # How do we resolve conflicts?
     * <p>
     * We leave out the cask route annotation on the conflicting operations, e.g. :
     * {{{
     * //conflict: @cask.get("/user/:username")
     * def getUserByName(username: String, request: cask.Request) = ...
     * }}}
     * <p>
     * and we introduce a new discriminator function to "manually" call those conflicts:
     * {{{
     *
     * @cask.get("/user", subpath = true)
     * def userRouteDescriminator(request: cask.Request) = {
     * request.remainingPathSegments match {
     * case Seq("logout") => logoutUser(request)
     * case Seq("login") => loginUser(request)
     * case Seq(param) => getUserByName(param, request)
     * }
     * }
     * }}}
     */
    public static class OperationGroup {
        List<CodegenOperation> operations = new ArrayList<>();
        final String pathPrefix;
        final String httpMethod;
        final String caskAnnotation;
        final String methodName;

        // TODO - multiple operations may have the same query params, so we'll need to somehow merge them (and take the right type)
        public boolean hasGroupQueryParams() {
            return operations.stream().flatMap(op -> op.queryParams.stream()).count() > 0;
        }

        /**
         * This is invoked from `scala-cask/apiRoutesQueryParamsTyped.mustache`
         *
         * @return the CodegenParameters
         */
        public List<CodegenParameter> getGroupQueryParams() {
            List<CodegenParameter> list = operations.stream().flatMap(op -> op.queryParams.stream()).map(p -> {
                        final CodegenParameter copy = p.copy();
                        copy.vendorExtensions.put("x-default-value", defaultValue(p));
                        copy.required = false; // all our query params are optional for our work-around as it's a super-set of a few different routes
                        copy.dataType = asScalaDataType(copy, false, true, true);
                        copy.defaultValue = defaultValue(copy);
                        return copy;
                    }
            ).collect(Collectors.toList());

            return list;
        }

        @Override
        public String toString() {
            List<String> ops = operations.stream().map(o -> o.path + "\n").collect(Collectors.toList());
            return httpMethod + " " + pathPrefix + " w/ " + operations.size() + " operations:\n" + String.join("", ops);
        }

        public OperationGroup(String httpMethod, String pathPrefix) {
            this.httpMethod = httpMethod;
            this.pathPrefix = pathPrefix;
            caskAnnotation = "@cask." + httpMethod.toLowerCase(Locale.ROOT);

            List<String> stripped = Arrays.stream(pathPrefix.split("/", -1))
                    .map(ScalaCaskServerCodegen::capitalise).collect(Collectors.toList());

            methodName = "routeWorkAroundFor" + capitalise(httpMethod) + String.join("", stripped);
        }

        public void add(CodegenOperation op) {
            if (!op.path.startsWith(pathPrefix)) {
                throw new IllegalArgumentException("inconsistent path: " + pathPrefix);
            }
            if (!op.httpMethod.equals(httpMethod)) {
                throw new IllegalArgumentException("inconsistent method: " + httpMethod);
            }

            final List<ScalaCaskServerCodegen.ParamPart> pathParts = new ArrayList<>();
            final List<String> parts = Arrays.stream(op.path.substring(pathPrefix.length()).split("/", -1)).filter(p -> !p.isEmpty()).collect(Collectors.toList());
            for (int i = 0; i < parts.size(); i++) {
                String p = parts.get(i);
                ScalaCaskServerCodegen.ParamPart pp = hasBrackets(p) ? new ScalaCaskServerCodegen.ParamPart(chompBrackets(p), pathParamForName(op, chompBrackets(p))) : new ScalaCaskServerCodegen.ParamPart(p, null);
                pathParts.add(pp);
            }

            List<ScalaCaskServerCodegen.ParamPart> paramPathParts = pathParts.stream().filter(p -> p.isParam).collect(Collectors.toList());
            if (!paramPathParts.isEmpty()) {
                final String lastParamName = paramPathParts.get(paramPathParts.size() - 1).name;
                paramPathParts.forEach(p -> p.hasMoreParams = !p.name.equals(lastParamName));
            }
            if (!pathParts.isEmpty()) {
                pathParts.get(pathParts.size() - 1).hasMore = false;
            }

            op.vendorExtensions.put("x-path-remaining", pathParts);
            op.vendorExtensions.put("x-has-path-remaining", !paramPathParts.isEmpty());
            operations.add(op);
        }

        public boolean contains(CodegenOperation op) {
            return operations.contains(op);
        }

        public void updateAnnotations() {
            operations.forEach(op -> {
                String annotation = op.vendorExtensions.get("x-annotation").toString();
                String conflicts = String.join(", ", operations.stream().map(o -> o.path).collect(Collectors.toList()));
                op.vendorExtensions.put("x-annotation", "// conflicts with [" + conflicts + "] after" + pathPrefix + ", ignoring " + annotation);
            });
            operations = operations.stream().sorted((a, b) -> a.pathParams.size() - b.pathParams.size()).collect(Collectors.toList());
        }
    }


    static List<ScalaCaskServerCodegen.OperationGroup> group(List<CodegenOperation> operationList) {
        Map<String, ScalaCaskServerCodegen.OperationGroup> groupedByPrefix = new HashMap<>();
        operationList.forEach(op -> {
            String prefix = nonParamPathPrefix(op);
            String key = op.httpMethod + " " + prefix;
            if (!op.pathParams.isEmpty()) {
                final ScalaCaskServerCodegen.OperationGroup group = groupedByPrefix.getOrDefault(key, new ScalaCaskServerCodegen.OperationGroup(op.httpMethod, prefix));
                group.add(op);
                groupedByPrefix.put(key, group);
            }
        });
        return groupedByPrefix.values().stream().collect(Collectors.toList());
    }

    static String nonParamPathPrefix(CodegenOperation op) {
        if (op.pathParams.isEmpty()) {
            return op.path;
        }

        final String firstParam = op.pathParams.stream().findFirst().get().paramName;
        final int i = op.path.indexOf(firstParam);
        final String path = chompSuffix(op.path.substring(0, i - 1), "/");
        return path;
    }

    static List<ScalaCaskServerCodegen.OperationGroup> createRouteGroups(List<CodegenOperation> operationList) {

        List<ScalaCaskServerCodegen.OperationGroup> groups = group(operationList);
        operationList.forEach((op) -> {
            // for the usage/call site
            final String scalaPath = pathWithBracketPlaceholdersRemovedAndXPathIndexAdded(op);
            op.vendorExtensions.put("x-cask-path", scalaPath);

            final String annotation = "@cask." + op.httpMethod.toLowerCase(Locale.ROOT);
            op.vendorExtensions.put("x-annotation", annotation);

            for (final ScalaCaskServerCodegen.OperationGroup group : groups) {
                if (!group.contains(op)) {
                    if (op.path.startsWith(group.pathPrefix) && op.httpMethod.equalsIgnoreCase(group.httpMethod)) {
                        group.add(op);
                    }
                }
            }
        });

        List<ScalaCaskServerCodegen.OperationGroup> trimmed = groups.stream().filter(g -> g.operations.size() > 1).map(g -> {
            g.updateAnnotations();
            return g;
        }).collect(Collectors.toList());
        return trimmed;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        final Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        final List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        objs.put("route-groups", createRouteGroups(operationList));

        operationList.forEach(ScalaCaskServerCodegen::postProcessOperation);
        return objs;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs.getModels().stream().map(ModelMap::getModel).forEach(this::postProcessModel);
        return objs;
    }

    private void setDefaultValueForCodegenProperty(CodegenProperty p) {

        if (p.defaultValue == null || p.defaultValue.trim().isEmpty()) {
            if (p.getIsEnumOrRef()) {
                p.defaultValue = "null";
            } else {
                p.defaultValue = defaultValueNonOption(p);
            }
        } else if (p.defaultValue.contains("Seq.empty")) {
            p.defaultValue = "Nil";
        }
    }

    private void postProcessModel(CodegenModel model) {
        model.getAllVars().forEach(this::setDefaultValueForCodegenProperty);
        model.getVars().forEach(this::setDefaultValueForCodegenProperty);

        model.getVars().forEach(ScalaCaskServerCodegen::postProcessProperty);
        model.getAllVars().forEach(ScalaCaskServerCodegen::postProcessProperty);
    }

    private static void postProcessOperation(CodegenOperation op) {
        // force http method to lower case
        op.httpMethod = op.httpMethod.toLowerCase(Locale.ROOT);

        /* Put in 'x-consumes-json' and 'x-consumes-xml' */
        op.vendorExtensions.put("x-consumes-json", consumesMimetype(op, "application/json"));
        op.vendorExtensions.put("x-consumes-xml", consumesMimetype(op, "application/xml"));

        op.bodyParams.stream().filter((b) -> b.isBodyParam).forEach((p) -> {
            p.vendorExtensions.put("x-consumes-json", consumesMimetype(op, "application/json"));
            p.vendorExtensions.put("x-consumes-xml", consumesMimetype(op, "application/xml"));
        });

        /* put in 'x-container-type' to help with unmarshalling from json */
        op.allParams.forEach((p) -> p.vendorExtensions.put("x-container-type", containerType(p.dataType)));
        op.bodyParams.forEach((p) -> p.vendorExtensions.put("x-container-type", containerType(p.dataType)));

        final String paramList = op.allParams.stream().map((p) -> p.paramName).collect(Collectors.joining(", "));
        op.vendorExtensions.put("x-param-list", paramList);

        final Stream<String> typed = op.allParams.stream().map((p) -> p.paramName + " : " + asScalaDataType(p, p.required, false));
        op.vendorExtensions.put("x-param-list-typed", String.join(", ", typed.collect(Collectors.toList())));

        final Stream<String> typedJson = op.allParams.stream().map((p) -> p.paramName + " : " + asScalaDataType(p, p.required, true));
        op.vendorExtensions.put("x-param-list-typed-json", String.join(", ", typedJson.collect(Collectors.toList())));

        // for the declaration site
        op.vendorExtensions.put("x-cask-path-typed", routeArgs(op));
        op.vendorExtensions.put("x-query-args", queryArgs(op));

        List<String> responses = op.responses.stream().map(r -> r.dataType).filter(Objects::nonNull).collect(Collectors.toList());
        op.vendorExtensions.put("x-response-type", responses.isEmpty() ? "Unit" : String.join(" | ", responses));
    }

    private static void postProcessProperty(CodegenProperty p) {
        p.vendorExtensions.put("x-datatype-model", asScalaDataType(p, p.required, false));
        p.vendorExtensions.put("x-defaultValue-model", defaultValue(p, p.required, p.defaultValue));
        String dataTypeData = asScalaDataType(p, p.required, true);
        p.vendorExtensions.put("x-datatype-data", dataTypeData);


        p.vendorExtensions.put("x-containertype-data", containerType(dataTypeData));

        p.vendorExtensions.put("x-defaultValue-data", defaultValueNonOption(p, p.defaultValue));

        // the 'asModel' logic for modelData.mustache
        //
        // if it's optional (not required), then wrap the value in Option()
        // ... unless it's a map or array, in which case it can just be empty
        //
        p.vendorExtensions.put("x-wrap-in-optional", !p.required && !p.isArray && !p.isMap);

        // if it's an array or optional, we need to map it as a model -- unless it's a map,
        // in which case we have to map the values
        boolean hasItemModel = p.items != null && p.items.isModel;
        boolean isObjectArray = p.isArray && hasItemModel;
        boolean isOptionalObj = !p.required && p.isModel;
        p.vendorExtensions.put("x-map-asModel", (isOptionalObj || isObjectArray) && !p.isMap);

        // when deserialising map objects, the logic is tricky.
        p.vendorExtensions.put("x-deserialize-asModelMap", p.isMap && hasItemModel);

        // for some reason, an openapi spec with pattern field like this:
        // pattern: '^[A-Za-z]+$'
        // will result in the pattern property text of
        // pattern: '/^[A-Za-z]+$/'
        if (p.pattern != null && p.pattern.startsWith("/") && p.pattern.endsWith("/")) {
            p.pattern = p.pattern.substring(1, p.pattern.length() - 1);
        }

    }


    /**
     * Cask path params use the :pathParam syntax rather than the {pathParam} syntax
     *
     * @param op
     * @return
     */
    private static String pathWithBracketPlaceholdersRemovedAndXPathIndexAdded(CodegenOperation op) {
        String[] items = op.path.split("/", -1);
        String scalaPath = "";
        for (int i = 0; i < items.length; ++i) {
            final String nextPart = hasBrackets(items[i]) ? ":" + chompBrackets(items[i]) : items[i];
            if (i != items.length - 1) {
                scalaPath = scalaPath + nextPart + "/";
            } else {
                scalaPath = scalaPath + nextPart;
            }
        }
        return scalaPath;
    }

    private static CodegenParameter pathParamForName(CodegenOperation op, String pathParam) {
        final CodegenParameter param = op.pathParams.stream().filter(p -> p.paramName.equals(pathParam)).findFirst().get();
        if (param == null) {
            throw new RuntimeException("Bug: path param " + pathParam + " not found");
        }
        return param;
    }

    /**
     * The path placeholders as well as query parameters
     *
     * @param op the codegen operations
     * @return a list of both the path and query parameters as typed arguments (e.g. "aPathArg : Int, request: cask.Request, aQueryArg : Option[Long]")
     */
    private static String routeArgs(CodegenOperation op) {
        final Stream<String> pathParamNames = Arrays.stream(op.path.split("/", -1)).filter(ScalaCaskServerCodegen::hasBrackets).map(p -> {
            final CodegenParameter param = pathParamForName(op, chompBrackets(p));
            return param.paramName + " : " + asScalaDataType(param, param.required, true);
        });


        final List<String> pathList = pathParamNames.collect(Collectors.toList());

        // we always include the cask request
        pathList.add("request: cask.Request");

        final Stream<String> queryParams = op.queryParams.stream().map(p -> {
            p.vendorExtensions.put("x-default-value", defaultValue(p));
            return p.paramName + " : " + asScalaDataType(p, p.required, true, true);
        });
        pathList.addAll(queryParams.collect(Collectors.toList()));
        return pathList.isEmpty() ? "" : (String.join(", ", pathList));
    }

    private static String defaultValue(CodegenParameter p) {
        return defaultValue(p, p.required, p.defaultValue);
    }

    private static String defaultValue(IJsonSchemaValidationProperties p, boolean required, String fallbackDefaultValue) {
        if (!required && !(p.getIsArray() || p.getIsMap())) {
            return "None";
        }
        return defaultValueNonOption(p, fallbackDefaultValue);
    }

    private static String defaultValueNonOption(IJsonSchemaValidationProperties p, String fallbackDefaultValue) {
        if (p.getIsArray()) {
            if (p.getUniqueItems()) {
                return "Set.empty";
            }
            return "Nil";
        }
        if (p.getIsMap()) {
            return "Map.empty";
        }
        if (p.getIsNumber()) {
            return "0";
        }
        if (p.getIsEnum()) {
            return fallbackDefaultValue;
        }
        if (p.getIsBoolean()) {
            return "false";
        }
        if (p.getIsUuid()) {
            return "java.util.UUID.randomUUID()";
        }
        if (p.getIsString()) {
            return "\"\"";
        }
        return fallbackDefaultValue;
    }

    private static String defaultValueNonOption(CodegenProperty p) {
        if (p.getIsArray()) {
            return "Nil";
        }
        if (p.getIsMap()) {
            return "Map.empty";
        }
        if (p.isNumber || p.isNumeric) {
            return "0";
        }
        if (p.isBoolean) {
            return "false";
        }
        if (p.isUuid) {
            return "java.util.UUID.randomUUID()";
        }
        if (p.isModel) {
            return "null";
        }
        if (p.isDate || p.isDateTime) {
            return "null";
        }
        if (p.isString) {
            return "\"\"";
        }
        return p.defaultValue;
    }


    @Override
    public CodegenProperty fromProperty(String name, Schema schema) {
        CodegenProperty property = super.fromProperty(name, schema);

        // Customize type for freeform objects
        if (ModelUtils.isFreeFormObject(schema)) {
            property.dataType = "Value";
            property.baseType = "Value";
        }

        return property;
    }

    @Override
    public String getTypeDeclaration(Schema schema) {
        if (ModelUtils.isFreeFormObject(schema)) {
            return "Value";
        }
        return super.getTypeDeclaration(schema);
    }

    @Override
    public String toModelImport(String name) {
        final String result = super.toModelImport(name);
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        }
        return result;
    }

    private static String queryArgs(final CodegenOperation op) {
        final List<String> list = op.queryParams.stream().map(p -> p.paramName).collect(Collectors.toList());
        final String prefix = list.isEmpty() ? "" : ", ";
        return prefix + String.join(", ", list);
    }

    /**
     * For our model classes, we have two variants:
     * <p>
     * 1) a {model}.scala one which is a validated, model class
     * 2) a {model}Data.scala one which is just our data-transfer-object (DTO) which is written primarily for e.g. json serialisation
     * <p>
     * The data variant can have nulls and other non-scala things, but they know how to create validated model objects.
     * <p>
     * This 'asScalaDataType' is used to ensure the type hierarchy is correct for both the model and data varients.
     * <p>
     * e.g. consider this example:
     * ```
     * case class Foo(bar : Bar, bazz :List[Bazz])
     * case class Bar(x : Option[String] = None)
     * case class Bazz(y : Int)
     * <p>
     * // vs
     * <p>
     * case class FooData(bar : BarData, bazz :List[BazzData])
     * case class BarData(x : String = "")
     * case class BazzData(y : Int)
     * ```
     */
    private static String asScalaDataType(final IJsonSchemaValidationProperties param, boolean required, boolean useJason) {
        return asScalaDataType(param, required, useJason, !useJason);
    }

    private static String asScalaDataType(final IJsonSchemaValidationProperties param, boolean required, boolean useJason, boolean allowOptional) {
        String dataType = (param.getIsModel() && useJason) ? param.getDataType() + "Data" : param.getDataType();

        final String dataSuffix = useJason && param.getItems() != null && param.getItems().getIsModel() ? "Data" : "";
        if (dataType.startsWith("List[")) {
            dataType = dataType.replace("List[", "Seq[");
            dataType = dataType.replace("]", dataSuffix + "]");
        } else if (dataType.startsWith("Set[")) {
            dataType = dataType.replace("]", dataSuffix + "]");
        } else if (!required && allowOptional) {
            dataType = "Option[" + dataType + "]";
        }
        return dataType;
    }

    private static String chompBrackets(String str) {
        return str.replace("{", "").replace("}", "");
    }

    private static String chompSuffix(String str, String suffix) {
        return str.endsWith(suffix) ? chompSuffix(str.substring(0, str.length() - suffix.length()), suffix) : str;
    }

    private static boolean hasBrackets(String str) {
        return str.matches("^\\{(.*)\\}$");
    }

    static String containerType(String dataType) {
        String fixedForList = dataType.replaceAll(".*\\[(.*)\\]", "$1");

        // if it is a map, we want the value type
        final String[] parts = fixedForList.split(",");
        return parts[parts.length - 1];
    }
}
