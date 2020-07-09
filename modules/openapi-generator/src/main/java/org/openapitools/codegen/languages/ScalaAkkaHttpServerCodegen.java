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

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ScalaAkkaHttpServerCodegen extends AbstractScalaCodegen implements CodegenConfig {
    protected String groupId;
    protected String artifactId;
    protected String artifactVersion;
    protected String invokerPackage;

    protected String akkaHttpVersion;

    public static final String AKKA_HTTP_VERSION = "akkaHttpVersion";
    public static final String AKKA_HTTP_VERSION_DESC = "The version of akka-http";
    public static final String DEFAULT_AKKA_HTTP_VERSION = "10.1.10";

    static Logger LOGGER = LoggerFactory.getLogger(ScalaAkkaHttpServerCodegen.class);

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "scala-akka-http-server";
    }

    public String getHelp() {
        return "Generates a scala-akka-http server (beta).";
    }

    public ScalaAkkaHttpServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.BearerToken
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "scala-akka-http";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "scala-akka-http-server";

        groupId = "org.openapitools";
        artifactId = "openapi-scala-akka-http-server";
        artifactVersion = "1.0.0";
        apiPackage = "org.openapitools.server.api";
        modelPackage = "org.openapitools.server.model";
        invokerPackage = "org.openapitools.server";
        akkaHttpVersion = DEFAULT_AKKA_HTTP_VERSION;

        setReservedWordsLowerCase(
                Arrays.asList(
                        "abstract", "case", "catch", "class", "def", "do", "else", "extends",
                        "false", "final", "finally", "for", "forSome", "if", "implicit",
                        "import", "lazy", "match", "new", "null", "object", "override", "package",
                        "private", "protected", "return", "sealed", "super", "this", "throw",
                        "trait", "try", "true", "type", "val", "var", "while", "with", "yield")
        );

        cliOptions.add(CliOption.newString(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC).defaultValue(invokerPackage));
        cliOptions.add(CliOption.newString(CodegenConstants.GROUP_ID, CodegenConstants.GROUP_ID_DESC).defaultValue(groupId));
        cliOptions.add(CliOption.newString(CodegenConstants.ARTIFACT_ID, CodegenConstants.ARTIFACT_ID).defaultValue(artifactId));
        cliOptions.add(CliOption.newString(CodegenConstants.ARTIFACT_VERSION, CodegenConstants.ARTIFACT_VERSION_DESC).defaultValue(artifactVersion));
        cliOptions.add(CliOption.newString(AKKA_HTTP_VERSION, AKKA_HTTP_VERSION_DESC).defaultValue(akkaHttpVersion));

        importMapping.remove("Seq");
        importMapping.remove("List");
        importMapping.remove("Set");
        importMapping.remove("Map");

        typeMapping = new HashMap<>();
        typeMapping.put("array", "Seq");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("int", "Int");
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("byte", "Byte");
        typeMapping.put("short", "Short");
        typeMapping.put("char", "Char");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Any");
        typeMapping.put("file", "File");
        typeMapping.put("binary", "File");
        typeMapping.put("number", "Double");

        instantiationTypes.put("array", "ListBuffer");
        instantiationTypes.put("map", "Map");

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            invokerPackage = (String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE);
        } else {
            additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }

        if (additionalProperties.containsKey(CodegenConstants.GROUP_ID)) {
            groupId = (String) additionalProperties.get(CodegenConstants.GROUP_ID);
        } else {
            additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_ID)) {
            artifactId = (String) additionalProperties.get(CodegenConstants.ARTIFACT_ID);
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_VERSION)) {
            artifactVersion = (String) additionalProperties.get(CodegenConstants.ARTIFACT_VERSION);
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }

        if (additionalProperties.containsKey(AKKA_HTTP_VERSION)) {
            akkaHttpVersion = (String) additionalProperties.get(AKKA_HTTP_VERSION);
        } else {
            additionalProperties.put(AKKA_HTTP_VERSION, akkaHttpVersion);
        }

        parseAkkaHttpVersion();

        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        supportingFiles.add(new SupportingFile("controller.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator), "Controller.scala"));
        supportingFiles.add(new SupportingFile("helper.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator), "AkkaHttpHelper.scala"));
        supportingFiles.add(new SupportingFile("stringDirectives.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator), "StringDirectives.scala"));
        supportingFiles.add(new SupportingFile("multipartDirectives.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator), "MultipartDirectives.scala"));
    }

    private static final String IS_10_1_10_PLUS = "akkaHttp10_1_10_plus";
    private boolean is10_1_10AndAbove = false;

    private static final Pattern akkaVersionPattern = Pattern.compile("([0-9]+)(\\.([0-9]+))?(\\.([0-9]+))?(.\\+)?");

    private void parseAkkaHttpVersion() {
        Matcher matcher = akkaVersionPattern.matcher(akkaHttpVersion);
        if (matcher.matches()) {
            String majorS = matcher.group(1);
            String minorS = matcher.group(3);
            String patchS = matcher.group(5);
            boolean andAbove = matcher.group(6) != null;
            int major = -1, minor = -1, patch = -1;
            try {
                if (majorS != null) {
                    major = Integer.parseInt(majorS);
                    if (minorS != null) {
                        minor = Integer.parseInt(minorS);
                        if (patchS != null) {
                            patch = Integer.parseInt(patchS);
                        }
                    }
                }


                if (major > 10 || major == -1 && andAbove) {
                    is10_1_10AndAbove = true;
                } else if (major == 10) {
                    if (minor > 1 || minor == -1 && andAbove) {
                        is10_1_10AndAbove = true;
                    } else if (minor == 1) {
                        if (patch >= 10 || patch == -1 && andAbove) {
                            is10_1_10AndAbove = true;
                        }
                    }
                }

            } catch (NumberFormatException e) {
                LOGGER.warn("Unable to parse " + AKKA_HTTP_VERSION + ": " + akkaHttpVersion + ", fallback to " + DEFAULT_AKKA_HTTP_VERSION);
                akkaHttpVersion = DEFAULT_AKKA_HTTP_VERSION;
                is10_1_10AndAbove = true;
            }
        }

        additionalProperties.put(IS_10_1_10_PLUS, is10_1_10AndAbove);
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation codegenOperation = super.fromOperation(path, httpMethod, operation, servers);
        addPathMatcher(codegenOperation);
        return codegenOperation;
    }

    @Override
    public CodegenParameter fromParameter(Parameter parameter, Set<String> imports) {
        CodegenParameter param = super.fromParameter(parameter, imports);
        // Removing unhandled types
        if (!primitiveParamTypes.contains(param.dataType)) {
            param.dataType = "String";
        }
        if (!param.required) {
            param.vendorExtensions.put("x-has-default-value", param.defaultValue != null);
            // Escaping default string values
            if (param.defaultValue != null && param.dataType.equals("String")) {
                param.defaultValue = String.format(Locale.ROOT, "\"%s\"", param.defaultValue);
            }
        }
        return param;
    }


    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> baseObjs = super.postProcessOperationsWithModels(objs, allModels);
        pathMatcherPatternsPostProcessor(baseObjs);
        marshallingPostProcessor(baseObjs);
        return baseObjs;
    }

    private static Set<String> primitiveParamTypes = new HashSet<String>() {{
        addAll(Arrays.asList(
                "Int",
                "Long",
                "Float",
                "Double",
                "Boolean",
                "String"
        ));
    }};

    private static Map<String, String> pathTypeToMatcher = new HashMap<String, String>() {{
        put("Int", "IntNumber");
        put("Long", "LongNumber");
        put("Float", "FloatNumber");
        put("Double", "DoubleNumber");
        put("Boolean", "Boolean");
        put("String", "Segment");
    }};

    protected static void addPathMatcher(CodegenOperation codegenOperation) {
        LinkedList<String> allPaths = new LinkedList<>(Arrays.asList(codegenOperation.path.split("/")));
        allPaths.removeIf(""::equals);

        LinkedList<TextOrMatcher> pathMatchers = new LinkedList<>();
        for (String path : allPaths) {
            TextOrMatcher textOrMatcher = new TextOrMatcher("", true, true);
            if (path.startsWith("{") && path.endsWith("}")) {
                String parameterName = path.substring(1, path.length() - 1);
                for (CodegenParameter pathParam : codegenOperation.pathParams) {
                    if (pathParam.baseName.equals(parameterName)) {
                        String matcher = pathTypeToMatcher.get(pathParam.dataType);
                        if (matcher == null) {
                            LOGGER.warn("The path parameter " + pathParam.baseName +
                                    " with the datatype " + pathParam.dataType +
                                    " could not be translated to a corresponding path matcher of akka http" +
                                    " and therefore has been translated to string.");
                            matcher = pathTypeToMatcher.get("String");
                        }
                        if (pathParam.pattern != null && !pathParam.pattern.isEmpty()) {
                            matcher = pathMatcherPatternName(pathParam);
                        }
                        textOrMatcher.value = matcher;
                        textOrMatcher.isText = false;
                        pathMatchers.add(textOrMatcher);
                    }
                }
            } else {
                textOrMatcher.value = path;
                textOrMatcher.isText = true;
                pathMatchers.add(textOrMatcher);
            }
        }
        pathMatchers.getLast().hasMore = false;

        codegenOperation.vendorExtensions.put("x-paths", pathMatchers);
    }

    public static String PATH_MATCHER_PATTERNS_KEY = "pathMatcherPatterns";

    @SuppressWarnings("unchecked")
    private static void pathMatcherPatternsPostProcessor(Map<String, Object> objs) {
        if (objs != null) {
            HashMap<String, PathMatcherPattern> patternMap = new HashMap<>();
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    for (CodegenParameter parameter : operation.pathParams) {
                        if (parameter.pattern != null && !parameter.pattern.isEmpty()) {
                            String name = pathMatcherPatternName(parameter);
                            if (!patternMap.containsKey(name)) {
                                patternMap.put(name, new PathMatcherPattern(name, parameter.pattern.substring(1, parameter.pattern.length() - 1)));
                            }
                        }
                    }
                }
            }
            objs.put(PATH_MATCHER_PATTERNS_KEY, new ArrayList<>(patternMap.values()));
        }
    }

    private static String pathMatcherPatternName(CodegenParameter parameter) {
        return parameter.paramName + "Pattern";
    }

    // Responsible for setting up Marshallers/Unmarshallers
    @SuppressWarnings("unchecked")
    public static void marshallingPostProcessor(Map<String, Object> objs) {

        if (objs == null) {
            return;
        }

        Set<Marshaller> entityUnmarshallerTypes = new HashSet<>();
        Set<Marshaller> entityMarshallerTypes = new HashSet<>();
        Set<Marshaller> stringUnmarshallerTypes = new HashSet<>();
        boolean hasCookieParams = false;
        boolean hasMultipart = false;

        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

            for (CodegenOperation op : operationList) {
                boolean isMultipart = op.isMultipart;
                hasMultipart |= isMultipart;
                hasCookieParams |= op.getHasCookieParams();
                ArrayList<CodegenParameter> fileParams = new ArrayList<>();
                ArrayList<CodegenParameter> nonFileParams = new ArrayList<>();
                for (CodegenParameter parameter : op.allParams) {
                    if (parameter.isBodyParam || parameter.isFormParam) {
                        if (parameter.isFile) {
                            fileParams.add(parameter.copy());
                        } else {
                            nonFileParams.add(parameter.copy());
                        }
                        if (!parameter.isPrimitiveType) {
                            if (isMultipart) {
                                stringUnmarshallerTypes.add(new Marshaller(parameter));
                            } else {
                                entityUnmarshallerTypes.add(new Marshaller(parameter));
                            }
                        }
                    }
                }
                for (int i = 0, size = fileParams.size(); i < size; ++i) {
                    fileParams.get(i).hasMore = i < size - 1;
                }
                for (int i = 0, size = nonFileParams.size(); i < size; ++i) {
                    nonFileParams.get(i).hasMore = i < size - 1;
                }

                HashSet<Marshaller> operationSpecificMarshallers = new HashSet<>();
                for (CodegenResponse response : op.responses) {
                    if (!response.primitiveType) {
                        Marshaller marshaller = new Marshaller(response);
                        entityMarshallerTypes.add(marshaller);
                        operationSpecificMarshallers.add(marshaller);
                    }
                    response.vendorExtensions.put("x-empty-response", response.baseType == null && response.message == null);
                    response.vendorExtensions.put("x-is-default", response.code.equals("0"));
                }
                op.vendorExtensions.put("x-specific-marshallers", operationSpecificMarshallers);
                op.vendorExtensions.put("x-file-params", fileParams);
                op.vendorExtensions.put("x-non-file-params", nonFileParams);
            }
        }

        objs.put("hasCookieParams", hasCookieParams);
        objs.put("entityMarshallers", entityMarshallerTypes);
        objs.put("entityUnmarshallers", entityUnmarshallerTypes);
        objs.put("stringUnmarshallers", stringUnmarshallerTypes);
        objs.put("hasMarshalling", !entityMarshallerTypes.isEmpty() || !entityUnmarshallerTypes.isEmpty() || !stringUnmarshallerTypes.isEmpty());
        objs.put("hasMultipart", hasMultipart);
    }

}

class Marshaller {
    String varName;
    String dataType;

    public Marshaller(CodegenResponse response) {
        if (response.containerType != null) {
            this.varName = response.baseType + response.containerType;
        } else {
            this.varName = response.baseType;
        }
        this.dataType = response.dataType;
    }

    public Marshaller(CodegenParameter parameter) {
        if (parameter.isListContainer) {
            this.varName = parameter.baseType + "List";
        } else if (parameter.isMapContainer) {
            this.varName = parameter.baseType + "Map";
        } else if (parameter.isContainer) {
            this.varName = parameter.baseType + "Container";
        } else {
            this.varName = parameter.baseType;
        }
        this.dataType = parameter.dataType;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Marshaller that = (Marshaller) o;
        return varName.equals(that.varName) &&
                dataType.equals(that.dataType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(varName, dataType);
    }
}

class PathMatcherPattern {
    String pathMatcherVarName;
    String pattern;

    public PathMatcherPattern(String pathMatcherVarName, String pattern) {
        this.pathMatcherVarName = pathMatcherVarName;
        this.pattern = pattern;
    }
}

class TextOrMatcher {
    String value;
    boolean isText;
    boolean hasMore;

    public TextOrMatcher(String value, boolean isText, boolean hasMore) {
        this.value = value;
        this.isText = isText;
        this.hasMore = hasMore;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TextOrMatcher that = (TextOrMatcher) o;
        return isText == that.isText &&
                hasMore == that.hasMore &&
                value.equals(that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value, isText, hasMore);
    }
}
