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

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Escapers;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.*;
import org.openapitools.codegen.templating.mustache.EscapeKeywordLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

public class ScalaHttp4sServerCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(ScalaHttp4sServerCodegen.class);
    protected String artifactId = "http4s-server";
    protected String artifactVersion = "1.0.0";
    protected String sourceFolder = "scala";
    protected String sourceSubFolder = "main";
    private String packageName = "org.openapitools";

    public static final String EXCLUDE_SBT = "excludeSbt"; // generate as whole project
    public static final String SOURCE_SUBFOLDER = "sourceSubfolder"; // generate as whole project

    public ScalaHttp4sServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
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

        embeddedTemplateDir = templateDir = "scala-http4s-server";

        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";
        useOneOfInterfaces = true;
        supportsMultipleInheritance = true;
        supportsInheritance = true;
        supportsMixins = true;
        addOneOfInterfaceImports = true;


        setReservedWordsLowerCase(
                Arrays.asList(
                        // Scala
                        "abstract", "case", "catch", "class", "def",
                        "do", "else", "extends", "false", "final",
                        "finally", "for", "forSome", "if", "implicit",
                        "import", "lazy", "match", "new", "null",
                        "object", "override", "package", "private", "protected",
                        "return", "sealed", "super", "this", "throw",
                        "trait", "try", "true", "type", "val",
                        "var", "while", "with", "yield",
                        // Scala-interop languages keywords
                        "abstract", "continue", "switch", "assert",
                        "default", "synchronized", "goto",
                        "break", "double", "implements", "byte",
                        "public", "throws", "enum", "instanceof", "transient",
                        "int", "short", "char", "interface", "static",
                        "void", "finally", "long", "strictfp", "volatile", "const", "float",
                        "native")
        );

        defaultIncludes = new HashSet<>(
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

        typeMapping = new HashMap<>();
        typeMapping.put("string", "String");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Double");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("decimal", "BigDecimal");
        typeMapping.put("date-time", "ZonedDateTime");
        typeMapping.put("offset-date-time", "OffsetDateTime");
        typeMapping.put("date", "LocalDate");
        typeMapping.put("file", "File");
        typeMapping.put("array", "List");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("object", "Object");
        typeMapping.put("binary", "Array[Byte]");
        typeMapping.put("Date", "LocalDate");
        typeMapping.put("DateTime", "ZonedDateTime");
        typeMapping.put("OffsetDateTime", "OffsetDateTime");
        typeMapping.put("uuid", "UUID");

        additionalProperties.put("modelPackage", modelPackage());
        additionalProperties.put("apiPackage", apiPackage());
        additionalProperties.put("infoUrl", "http://org.openapitools");
        additionalProperties.put("infoEmail", "team@openapitools.org");
        additionalProperties.put("licenseInfo", "Apache 2.0");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put("fnEscapeBacktick", new EscapeBacktickLambda());

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "String",
                        "Boolean",
                        "Double",
                        "Int",
                        "Integer",
                        "Long",
                        "Float",
                        "Any",
                        "AnyVal",
                        "AnyRef",
                        "Object",
                        "BigDecimal"
                )
        );
        instantiationTypes.put("array", "ArrayList");
        instantiationTypes.put("map", "HashMap");

        importMapping = new HashMap<>();
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("URI", "java.net.URI");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "scala.collection.immutable.Map");
        importMapping.put("HashMap", "scala.collection.immutable.HashMap");
        importMapping.put("Seq", "scala.collection.immutable.Seq");
        importMapping.put("ArrayBuffer", "scala.collection.mutable.ArrayBuffer");
        importMapping.put("DateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");
        importMapping.put("ZonedDateTime", "java.time.ZonedDateTime");
        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
        //refined
        importMapping.put("Refined", "eu.timepit.refined.api.Refined");
        importMapping.put("And", "eu.timepit.refined.boolean.And");
        importMapping.put("MinSize", "eu.timepit.refined.collection.MinSize");
        importMapping.put("MaxSize", "eu.timepit.refined.collection.MaxSize");
        importMapping.put("MatchesRegex", "eu.timepit.refined.string.MatchesRegex");
        importMapping.put("Greater", "eu.timepit.refined.numeric.Greater");
        importMapping.put("GreaterEqual", "eu.timepit.refined.numeric.GreaterEqual");
        importMapping.put("Less", "eu.timepit.refined.numeric.Less");
        importMapping.put("LessEqual", "eu.timepit.refined.numeric.LessEqual");


        cliOptions.add(new CliOption(EXCLUDE_SBT, "exclude sbt from generation"));
        cliOptions.add(new CliOption(SOURCE_SUBFOLDER, "name of subfolder, for example to generate code in src/scala/generated"));

        inlineSchemaOption.put("SKIP_SCHEMA_REUSE", "true");
        inlineSchemaOption.put("REFACTOR_ALLOF_INLINE_SCHEMAS", "true");
    }

    private final static Map<String, String> locationStatusToResponse = new HashMap<>();

    static {
        locationStatusToResponse.put("300", "MultipleChoices");
        locationStatusToResponse.put("301", "MovedPermanently");
        locationStatusToResponse.put("302", "Found");
        locationStatusToResponse.put("303", "SeeOther");
        locationStatusToResponse.put("307", "TemporaryRedirect");
        locationStatusToResponse.put("308", "PermanentRedirect");
    }

    private final static Map<String, String> wwwAuthStatusToResponse = new HashMap<>();

    static {
        wwwAuthStatusToResponse.put("401", "Unauthorized");
    }

    private final static Map<String, String> allowStatusToResponse = new HashMap<>();

    static {
        allowStatusToResponse.put("405", "MethodNotAllowed");
    }

    private final static Map<String, String> proxyAuthStatusToResponse = new HashMap<>();

    static {
        proxyAuthStatusToResponse.put("407", "ProxyAuthenticationRequired");
    }

    private final static Map<String, String> statusToResponse = new HashMap<>();

    static {
        statusToResponse.put("100", "Continue");

        statusToResponse.put("101", "SwitchingProtocols");

        statusToResponse.put("102", "Processing");
        statusToResponse.put("103", "EarlyHints");

        statusToResponse.put("200", "Ok");
        statusToResponse.put("201", "Created");
        statusToResponse.put("202", "Accepted");
        statusToResponse.put("203", "NonAuthoritativeInformation");
        statusToResponse.put("204", "NoContent");
        statusToResponse.put("205", "ResetContent");
        statusToResponse.put("206", "PartialContent");
        statusToResponse.put("207", "MultiStatus");
        statusToResponse.put("208", "AlreadyReported");
        statusToResponse.put("226", "IMUsed");

        statusToResponse.put("304", "NotModified");
        statusToResponse.put("305", "UseProxy");

        statusToResponse.put("400", "BadRequest");
        statusToResponse.put("402", "PaymentRequired");
        statusToResponse.put("403", "Forbidden");
        statusToResponse.put("404", "NotFound");
        statusToResponse.put("406", "NotAcceptable");
        statusToResponse.put("408", "RequestTimeout");
        statusToResponse.put("409", "Conflict");
        statusToResponse.put("410", "Gone");
        statusToResponse.put("411", "LengthRequired");
        statusToResponse.put("412", "PreconditionFailed");
        statusToResponse.put("413", "PayloadTooLarge");
        statusToResponse.put("414", "UriTooLong");
        statusToResponse.put("415", "UnsupportedMediaType");
        statusToResponse.put("416", "RangeNotSatisfiable");
        statusToResponse.put("417", "ExpectationFailed");
        statusToResponse.put("418", "ImATeapot");
        statusToResponse.put("421", "MisdirectedRequest");
        statusToResponse.put("422", "UnprocessableEntity");
        statusToResponse.put("423", "Locked");
        statusToResponse.put("424", "FailedDependency");
        statusToResponse.put("425", "TooEarly");
        statusToResponse.put("426", "UpgradeRequired");
        statusToResponse.put("428", "PreconditionRequired");
        statusToResponse.put("429", "TooManyRequests");
        statusToResponse.put("431", "RequestHeaderFieldsTooLarge");
        statusToResponse.put("451", "UnavailableForLegalReasons");

        statusToResponse.put("500", "InternalServerError");
        statusToResponse.put("501", "NotImplemented");
        statusToResponse.put("502", "BadGateway");
        statusToResponse.put("503", "ServiceUnavailable");
        statusToResponse.put("504", "GatewayTimeout");
        statusToResponse.put("505", "HttpVersionNotSupported");
        statusToResponse.put("506", "VariantAlsoNegotiates");
        statusToResponse.put("507", "InsufficientStorage");
        statusToResponse.put("508", "LoopDetected");
        statusToResponse.put("510", "NotExtended");
        statusToResponse.put("511", "NetworkAuthenticationRequired");
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            packageName = (String) additionalProperties.get(CodegenConstants.PACKAGE_NAME);

            setApiPackage(packageName + ".apis");
            additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());

            setModelPackage(packageName + ".models");
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, modelPackage());
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(SOURCE_SUBFOLDER)) {
            sourceSubFolder = (String) additionalProperties.get(SOURCE_SUBFOLDER);
        }

        sourceFolder = "src" + File.separator + sourceSubFolder + File.separator + sourceFolder;

        supportingFiles.add(new SupportingFile("types.mustache", modelFileFolderRelative(), "types.scala"));
        supportingFiles.add(new SupportingFile("path.mustache", apiFileFolderRelative(), "path.scala"));
        supportingFiles.add(new SupportingFile("query.mustache", apiFileFolderRelative(), "query.scala"));

        supportingFiles.add(new SupportingFile("apis.mustache", packageFileFolderRelative(), "api.scala"));

        apiTemplateFiles.put("api.mustache", ".scala");

        if (!additionalProperties.containsKey(EXCLUDE_SBT) && !Boolean.parseBoolean((String) additionalProperties.get(EXCLUDE_SBT))) {
            supportingFiles.add(new SupportingFile("build.sbt", "", "build.sbt"));
            supportingFiles.add(new SupportingFile("build.properties", "project", "build.properties"));
        }
    }

    @Override
    public Map<String, String> inlineSchemaOption() {
        return super.inlineSchemaOption();
    }


    @Override
    public boolean isEnablePostProcessFile() {
        return true;
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        LOGGER.debug("postprocess " + file.toString());
        super.postProcessFile(file, fileType);
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> modelsMap = super.postProcessAllModels(objs);

        for (ModelsMap mm : modelsMap.values()) {
            for (ModelMap model : mm.getModels()) {
                // model oneOf as sealed trait

                CodegenModel cModel = model.getModel();

                if (!cModel.oneOf.isEmpty()) {
                    cModel.getVendorExtensions().put("x-isSealedTrait", true);
                }
                else if (cModel.isEnum) {
                    cModel.getVendorExtensions().put("x-isEnum", true);

                } else {
                    cModel.getVendorExtensions().put("x-another", true);
                }

                if (cModel.discriminator != null) {
                    cModel.getVendorExtensions().put("x-use-discr", true);

                    if (cModel.discriminator.getMapping() != null) {
                        cModel.getVendorExtensions().put("x-use-discr-mapping", true);
                    }
                }
                //
                try {
                    List<String> exts = (List<String>) cModel.getVendorExtensions().get("x-implements");
                    if (exts != null) {
                        cModel.getVendorExtensions().put("x-extends", exts.subList(0, 1));
                        cModel.getVendorExtensions().put("x-extendsWith", exts.subList(1, exts.size()));
                    }
                } catch (IndexOutOfBoundsException ignored) {
                }

                // add refined constraints

                for (CodegenProperty prop : cModel.vars) {
                    Set<String> imports = new TreeSet<>();

                    prop.getVendorExtensions().putAll(refineProp(prop, imports));

                    cModel.imports.addAll(imports);
                }
            }
        }
        return modelsMap;
    }

    private Map<String, Object> makeRefined(Set<String> imports, String dataType, ArrayList<String> refined) {
        Map<String, Object> vendorExtensions = new HashMap<>();
        if (!refined.isEmpty()) {
            imports.add("And");
            imports.add("Refined");

            String refinedRgt = String.join(" And ", refined);

            vendorExtensions.put("x-type", "Refined[" + dataType + ", " + refinedRgt + "]");
            vendorExtensions.put("x-refined-lft", dataType);
            vendorExtensions.put("x-refined-rgt", refinedRgt);
            vendorExtensions.put("x-refined", true);
        } else {
            vendorExtensions.put("x-type", dataType);
        }

        return vendorExtensions;
    }

    private Map<String, Object> refineProp(IJsonSchemaValidationProperties prop, Set<String> imports) {
        Map<String, Object> vendorExtensions = new HashMap<>();

        vendorExtensions.put("x-type", prop.getDataType());

        if (prop.getIsString()) {
            ArrayList<String> refined = new ArrayList<>();

            if (prop.getMinLength() != null) {
                refined.add("MinSize[" + prop.getMinLength() + "]");
                imports.add("MinSize");
            }
            if (prop.getMaxLength() != null) {
                refined.add("MaxSize[" + prop.getMaxLength() + "]");
                imports.add("MaxSize");
            }
            if (prop.getPattern() != null) {
                try {
                    String fixedPattern = prop.getPattern().substring(1, prop.getPattern().length() - 1);
                    refined.add("MatchesRegex[\"" + fixedPattern + "\"]");
                    imports.add("MatchesRegex");
                } catch (IndexOutOfBoundsException ignored) {
                }
            }
            vendorExtensions.putAll(makeRefined(imports, prop.getDataType(), refined));
        }

        if ("Int".equals(prop.getDataType())
                || "Long".equals(prop.getDataType())
                || "Float".equals(prop.getDataType())
                || "Double".equals(prop.getDataType())
                || "BigDecimal".equals(prop.getDataType())
        ) {
            ArrayList<String> refined = new ArrayList<>();

            if (prop.getMinimum() != null) {
                if (prop.getExclusiveMinimum()) {
                    refined.add("Greater[" + prop.getMinimum() + "]");
                    imports.add("Greater");
                } else {
                    refined.add("GreaterEqual[" + prop.getMinimum() + "]");
                    imports.add("GreaterEqual");
                }
            }
            if (prop.getMaximum() != null) {
                if (prop.getExclusiveMaximum()) {
                    refined.add("Less[" + prop.getMaximum() + "]");
                    imports.add("Less");
                } else {
                    refined.add("LessEqual[" + prop.getMaximum() + "]");
                    imports.add("LessEqual");
                }
            }
            vendorExtensions.putAll(makeRefined(imports, prop.getDataType(), refined));
        }

        if (prop.getIsUuid() || "Uuid".equals(prop.getDataType())) {
            prop.setDataType("UUID");
        }

        if (prop.getIsArray() && prop.getItems() != null) {
            Map<String, Object> subVendorExtensions = refineProp(prop.getItems(), imports);
            prop.getItems().getVendorExtensions().putAll(subVendorExtensions);

            ArrayList<String> refined = new ArrayList<>();
            if (prop.getMinItems() != null) {
                refined.add("MinSize[" + prop.getMinItems() + "]");
                imports.add("MinSize");
            }
            if (prop.getMaxItems() != null) {
                refined.add("MaxSize[" + prop.getMaxItems() + "]");
                imports.add("MaxSize");
            }

            vendorExtensions.putAll(makeRefined(imports, prop.getDataType(), refined));
        }

        return vendorExtensions;
    }


    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Map<String, Object> bundle = super.postProcessSupportingFileData(objs);

        List<ModelMap> models = (List<ModelMap>) bundle.get("models");
        TreeSet<String> allImports = new TreeSet<>();
        for (ModelMap mm : models) {
            for (String nextImport : mm.getModel().imports) {
                String mapping = importMapping().get(nextImport);
                if (mapping != null && !defaultIncludes().contains(mapping)) {
                    allImports.add(mapping);
                }
                // add instantiation types
                mapping = instantiationTypes().get(nextImport);
                if (mapping != null && !defaultIncludes().contains(mapping)) {
                    allImports.add(mapping);
                }
            }
        }
        bundle.put("imports", allImports);
        bundle.put("packageName", packageName);


        ApiInfoMap apiInfoMap = (ApiInfoMap) bundle.get("apiInfo");
        Map<String, List<String>> authToOperationMap = new TreeMap<>();
        for (OperationsMap op : apiInfoMap.getApis()) {
            List<HashMap<String, Object>> opsByAuth = (List<HashMap<String, Object>>) op.get("operationsByAuth");
            for (HashMap<String, Object> auth : opsByAuth) {
                String autName = (String) auth.get("auth");
                String classname = (String) op.get("classname");
                List<String> classnames = authToOperationMap.computeIfAbsent(autName, k -> new ArrayList<>());
                classnames.add(classname);
            }
        }

        bundle.put("authToOperationMap",
                authToOperationMap.entrySet().stream().map(ent -> {
                    Map<String, Object> tuple = new HashMap<>();
                    String auth = ent.getKey();
                    tuple.put("auth", auth);
                    tuple.put("ops", ent.getValue());
                    tuple.put("addMiddleware", !"".equals(auth));
                    return tuple;
                }).collect(Collectors.toList())
        );
        return bundle;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "scala-http4s-server";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala http4s server bindings.";
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        // Reserved words will be further escaped at the mustache compiler level.
        // Scala escaping done here (via `, without compiler escaping) would otherwise be HTML encoded.
        return "`" + name + "`";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiFileFolderRelative();
    }

    private String apiFileFolderRelative() {
        return sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + modelFileFolderRelative();
    }

    public String modelFileFolderRelative() {
        return sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    public String packageFileFolderRelative() {
        return sourceFolder + File.separator + packageName.replace('.', File.separatorChar);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objsI, List<ModelMap> allModels) {
        OperationsMap objs = super.postProcessOperationsWithModels(objsI, allModels);
        OperationMap operations = objs.getOperations();

        List<CodegenOperation> operationList = operations.getOperation();
        Set<String> allAuth = new HashSet<>();
        Map<String, List<String>> opsByAuth = new HashMap<>();

        for (CodegenOperation op : operationList) {

            // Converts GET /foo/bar => GET / foo / bar =>
            generateScalaPath(op);

            // :? fooQueryParam
            generateQueryParameters(op);

            // decide wat methods do we need in delegate:
            if (op.consumes == null || op.consumes.size() == 0) {
                op.vendorExtensions.put("x-generic-body", true);
            } else {
                if (op.consumes.stream().anyMatch(x -> x.containsKey("isJson"))) {
                    op.vendorExtensions.put("x-json-body", true);
                }
                if (op.consumes.stream().anyMatch(x -> !x.containsKey("isJson"))) {
                    op.vendorExtensions.put("x-generic-body", true);
                }
            }

            // decide wat methods do we need in responses:
            for (CodegenResponse resp : op.responses) {
                if (resp.code.equals("0"))
                    resp.code = "200"; // 200 by default

                String responseName;

                responseName = locationStatusToResponse.get(resp.code);
                if (responseName != null) {
                    resp.vendorExtensions.put("x-response-location", true);
                } else {
                    responseName = wwwAuthStatusToResponse.get(resp.code);
                    if (responseName != null) {
                        resp.vendorExtensions.put("x-response-www-auth", true);
                    } else {
                        responseName = allowStatusToResponse.get(resp.code);
                        if (responseName != null) {
                            resp.vendorExtensions.put("x-response-allow", true);
                        } else {
                            responseName = proxyAuthStatusToResponse.get(resp.code);
                            if (responseName != null) {
                                resp.vendorExtensions.put("x-response-proxy-auth", true);
                            } else {
                                responseName = statusToResponse.get(resp.code);
                                if (responseName != null) {
                                    resp.vendorExtensions.put("x-response-standard", true);
                                } else {
                                    throw new IllegalArgumentException("unsupported status " + resp.code);
                                }
                            }
                        }
                    }
                }

                resp.vendorExtensions.put("x-response", responseName);

                if (resp.getContent() == null) {
                    resp.vendorExtensions.put("x-generic-response", true); // non json resp
                } else {
                    if (resp.getContent().containsKey("application/json")) {
                        resp.vendorExtensions.put("x-json-response", true); // json resp
                    } else {
                        resp.vendorExtensions.put("x-generic-response", true); // non json resp
                    }
                    if (resp.getContent().size() > 1) {
                        resp.vendorExtensions.put("x-generic-response", true); // non json resp
                    }
                }
            }

            if (op.authMethods != null) {
                for (CodegenSecurity cs : op.authMethods) {
                    allAuth.add(cs.name);
                }
                List<Map<String, Object>> authDup = new ArrayList<>();
                for (CodegenSecurity authMeth : op.authMethods) {
                    Map<String, Object> vals = new HashMap<>();
                    vals.put("authName", authMeth.name);
                    vals.put("operation", op);
                    authDup.add(vals);

                    opsByAuth.computeIfAbsent(authMeth.name, k -> new ArrayList<>()).add(op.operationId);
                }
                op.vendorExtensions.put("x-authed", authDup);
            } else {
                opsByAuth.computeIfAbsent("", k -> new ArrayList<>()).add(op.operationId);
            }
        }

        TreeSet<String> allImports = new TreeSet<>();
        List<String> currentImports = objs.getImports().stream().flatMap(m -> m.values().stream()).collect(Collectors.toList());
        for (CodegenOperation op : operationList) {
            for (String nextImport : op.imports) {
                String mapping = importMapping().get(nextImport);
                if (mapping != null && !defaultIncludes().contains(mapping)) {
                    if (!currentImports.contains(mapping)) {
                        allImports.add(mapping);
                    }
                }
                // add instantiation types
                mapping = instantiationTypes().get(nextImport);
                if (mapping != null && !currentImports.contains(mapping)) {
                    if (!currentImports.contains(mapping)) {
                        allImports.add(mapping);
                    }
                }
            }
        }


        objs.put("operationsByAuth", opsByAuth.entrySet().stream().map(ent -> {
                    HashMap<String, Object> tuple = new HashMap<>();
                    tuple.put("auth", ent.getKey());
                    tuple.put("ops", ent.getValue());
                    return tuple;
                }
        ).collect(Collectors.toList()));
        objs.put("extraImports", allImports);
        objs.put("allAuth", allAuth);

        return objs;
    }


    @SuppressWarnings("Duplicates")
    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            Schema inner = ModelUtils.getSchemaItems(p);
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);

            return getSchemaType(p) + "[String, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = schemaType;
        }
        return toModelName(type);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    private void generateScalaPath(CodegenOperation op) {
        Set<String> imports = new HashSet<>();

        String path = op.path;

        // remove first /
        if (path.startsWith("/")) {
            path = path.substring(1);
        }

        // remove last /
        if (path.endsWith("/")) {
            path = path.substring(0, path.length() - 1);
        }

        String[] items = path.split("/", -1);
        String scalaPath = "";
        int pathParamIndex = 0;

        for (String item : items) {

            if (item.matches("^\\{(.*)}$")) { // wrap in {}
                // find the datatype of the parameter
                final CodegenParameter cp = op.pathParams.get(pathParamIndex);

                // TODO: Handle non-primitives…
                scalaPath = scalaPath + " / " + cpToPathParameter(cp, imports, cp.vendorExtensions);

                pathParamIndex++;
            } else {
                scalaPath = scalaPath + " / " + "\"" + item + "\"";
            }
        }

        op.vendorExtensions.put("x-codegen-path", scalaPath);
        op.imports.addAll(imports);
    }

    private String cpToPathParameter(CodegenParameter cp, Set<String> imports, Map<String, Object> vendorExtensions) {
        // don't support containers and arrays yet, reset to string
        if (cp.isContainer || cp.isArray) {
            cp.setDataType("String");
            cp.setIsArray(false);
            cp.setIsString(true);
            cp.isContainer = false;
        }

        Map<String, Object> _vendorExtensions = refineProp(cp, imports);
        vendorExtensions.putAll(_vendorExtensions);

        if (_vendorExtensions.size() == 1) { // only `x-type`
            if ("String".equals(cp.getDataType())) {
                return escapeReservedWordUnapply(cp.baseName);
            } else {
                return cp.dataType + "Varr(" + escapeReservedWordUnapply(cp.baseName) + ")";
            }
        } else {
            return cp.baseName + "Varr(" + escapeReservedWordUnapply(cp.baseName) + ")";
        }
    }

    private void generateQueryParameters(CodegenOperation op) {
        Set<String> imports = new HashSet<>();
        String queryString = "";

        for (CodegenParameter cp : op.queryParams) {
            if (queryString.isEmpty()) {
                queryString = queryString + " :? ";
            } else {
                queryString = queryString + " +& ";
            }

            queryString = queryString + cpToQueryParameter(cp, imports, cp.vendorExtensions);
        }

        op.vendorExtensions.put("x-codegen-query", queryString);
        op.imports.addAll(imports);
    }

    private String cpToQueryParameter(CodegenParameter cp, Set<String> imports, Map<String, Object> vendorExtensions) {
        // don't support containers and arrays yet, reset to string
        if (cp.isContainer && !cp.isArray) {
            cp.setDataType("String");
            cp.setIsArray(false);
            cp.setIsString(true);
            cp.isContainer = false;
        }

        vendorExtensions.putAll(refineProp(cp, imports));
        return cp.baseName + "QueryParam(" + escapeReservedWordUnapply(cp.baseName) + ")";
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.SCALA;
    }

    @Override
    protected ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("escapeReservedWordUnapply", new EscapeKeywordLambda(this::escapeReservedWordUnapply));
    }

    private String escapeReservedWordUnapply(String value) {
        // The unapply method doesn’t allow you to work with reserved variables via backticks;
        // in such cases you should use the variable via a placeholder instead.
        return isReservedWord(value) ? "_" + value : value;
    }

    private static class EscapeBacktickLambda extends AbstractScalaCodegen.CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            if (fragment.startsWith("`") && fragment.endsWith("`")) {
                String unescaped = fragment.substring(1, fragment.length() - 1);
                return "`" + Escapers.HTML.escape(unescaped) + "`";
            }
            return Escapers.HTML.escape(fragment);
        }
    }
}
