//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by FernFlower decompiler)
//

package org.openapitools.codegen.languages;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Pattern;


public class KotlinPerfanaGatlingCodegen extends AbstractScalaCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ScalaPerfanaGatlingCodegen.class);
    private static final String PREFIX_INTEGER_VAR = "I@";
    private static final String NEW_ROW = "\n";
    protected String resourceFolder;
    protected String dataFolder;
    protected String bodiesFolder;
    protected String apiVersion;
    private static final PackageProperty PACKAGE_PROPERTY = new PackageProperty();
    private static final List<Property<?>> properties;
    private final ObjectMapper objectMapper;
    private static final Pattern VAR_PATTERN;

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "kotlin-perfana-gatling";
    }

    public String getHelp() {
        return "Generates a Gatling script including to be used with Perfana, continuous performance testing dashboard";
    }

    public KotlinPerfanaGatlingCodegen() {
        this.resourceFolder = "src" + File.separator + "test" + File.separator + "resources";
        this.dataFolder = this.resourceFolder + File.separator + "data";
        this.bodiesFolder = this.resourceFolder + File.separator + "bodies";
        this.apiVersion = "1.0.0";
        this.objectMapper = new ObjectMapper();
        this.objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, true);
        this.objectMapper.configure(SerializationFeature.WRITE_DURATIONS_AS_TIMESTAMPS, true);
        this.objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        this.modifyFeatureSet((features) -> features.includeDocumentationFeatures(DocumentationFeature.Readme).wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom)).securityFeatures(EnumSet.noneOf(SecurityFeature.class)).excludeGlobalFeatures(GlobalFeature.XMLStructureDefinitions, GlobalFeature.Callbacks, GlobalFeature.LinkObjects, GlobalFeature.ParameterStyling).excludeSchemaSupportFeatures(SchemaSupportFeature.Polymorphism).excludeParameterFeatures(ParameterFeature.Cookie).includeClientModificationFeatures(new ClientModificationFeature[]{ClientModificationFeature.BasePath}));
        this.sourceFolder = "src" + File.separator + "test" + File.separator + "kotlin";
        this.outputFolder = "generated-code/gatling";
        this.apiTemplateFiles.put("api.mustache", ".kt");
        this.templateDir = "kotlin-perfana-gatling";
        properties.stream().map(Property::toCliOptions).flatMap(Collection::stream).forEach((option) -> this.cliOptions.add(option));
        this.importMapping.remove("List");
        this.importMapping.remove("Set");
        this.importMapping.remove("Map");
        this.importMapping.put("Date", "java.util.Date");
        this.typeMapping = new HashMap<>();
        this.typeMapping.put("enum", "NSString");
        this.typeMapping.put("array", "List");
        this.typeMapping.put("set", "Set");
        this.typeMapping.put("boolean", "Boolean");
        this.typeMapping.put("string", "String");
        this.typeMapping.put("int", "Int");
        this.typeMapping.put("long", "Long");
        this.typeMapping.put("float", "Float");
        this.typeMapping.put("byte", "Byte");
        this.typeMapping.put("short", "Short");
        this.typeMapping.put("char", "Char");
        this.typeMapping.put("double", "Double");
        this.typeMapping.put("object", "Any");
        this.typeMapping.put("file", "File");
        this.typeMapping.put("binary", "String");
        this.typeMapping.put("ByteArray", "String");
        this.typeMapping.put("date-time", "Date");
        this.typeMapping.put("DateTime", "Date");
        this.instantiationTypes.put("array", "ListBuffer");
        this.instantiationTypes.put("map", "HashMap");
        this.setReservedWordsLowerCase(Arrays.asList("path", "contentTypes", "contentType", "queryParams", "headerParams", "formParams", "postBody", "mp", "basePath", "apiInvoker", "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "this", "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield"));
    }

    public void processOpts() {
        super.processOpts();
        properties.forEach((p) -> p.updateAdditionalProperties(this.additionalProperties));
        this.invokerPackage = PACKAGE_PROPERTY.getInvokerPackage(this.additionalProperties);
        this.apiPackage = PACKAGE_PROPERTY.getApiPackage(this.additionalProperties);
        this.modelPackage = PACKAGE_PROPERTY.getModelPackage(this.additionalProperties);
        String systemUnderTest;
        if (this.additionalProperties.containsKey("systemUnderTest")) {
            systemUnderTest = this.additionalProperties.get("systemUnderTest").toString();
        } else {
            systemUnderTest = "AddSystemUnderTest";
        }

        String artifactId;
        if (this.additionalProperties.containsKey("artifactId")) {
            artifactId = this.additionalProperties.get("artifactId").toString();
        } else {
            artifactId = "gatling-add-system-under-test";
        }

        String simulationClassName;
        if (this.additionalProperties.containsKey("simulationClassName")) {
            simulationClassName = this.additionalProperties.get("simulationClassName").toString();
        } else {
            simulationClassName = "AddSimulationClassName";
        }

        String gatlingVersion;
        if (this.additionalProperties.containsKey("gatlingVersion")) {
            gatlingVersion = this.additionalProperties.get("gatlingVersion").toString();
        } else {
            gatlingVersion = "3.7.4";
        }

        String eventsGatlingMavenPluginVersion;
        if (this.additionalProperties.containsKey("eventsGatlingMavenPluginVersion")) {
            eventsGatlingMavenPluginVersion = this.additionalProperties.get("eventsGatlingMavenPluginVersion").toString();
        } else {
            eventsGatlingMavenPluginVersion = "4.1.0-events-2";
        }

        String perfanaJavaClientVersion;
        if (this.additionalProperties.containsKey("perfanaJavaClientVersion")) {
            perfanaJavaClientVersion = this.additionalProperties.get("perfanaJavaClientVersion").toString();
        } else {
            perfanaJavaClientVersion = "2.0.1";
        }

        String testEventsWiremockVersion;
        if (this.additionalProperties.containsKey("testEventsWiremockVersion")) {
            testEventsWiremockVersion = this.additionalProperties.get("testEventsWiremockVersion").toString();
        } else {
            testEventsWiremockVersion = "1.2.0-SNAPSHOT";
        }

        String perfanaUrl;
        if (this.additionalProperties.containsKey("perfanaUrl")) {
            perfanaUrl = this.additionalProperties.get("perfanaUrl").toString();
        } else {
            perfanaUrl = "https://perfana.io";
        }

        String targetBaseUrl;
        if (this.additionalProperties.containsKey("targetBaseUrl")) {
            targetBaseUrl = this.additionalProperties.get("targetBaseUrl").toString();
        } else {
            targetBaseUrl = "http://your-app.com";
        }

        boolean perfanaEnabled;
        if (this.additionalProperties.containsKey("perfanaEnabled")) {
            perfanaEnabled = Boolean.parseBoolean(this.additionalProperties.get("perfanaEnabled").toString());
        } else {
            perfanaEnabled = false;
        }

        String influxHost;
        if (this.additionalProperties.containsKey("influxHost")) {
            influxHost = this.additionalProperties.get("influxHost").toString();
        } else {
            influxHost = "http://influxdb";
        }

        String influxPort;
        if (this.additionalProperties.containsKey("influxPort")) {
            influxPort = this.additionalProperties.get("influxPort").toString();
        } else {
            influxPort = "2003";
        }

        String influxProtocol;
        if (this.additionalProperties.containsKey("influxProtocol")) {
            influxProtocol = this.additionalProperties.get("influxProtocol").toString();
        } else {
            influxProtocol = "tcp";
        }

        String graphitePrefix;
        if (this.additionalProperties.containsKey("graphitePrefix")) {
            graphitePrefix = this.additionalProperties.get("graphitePrefix").toString();
        } else {
            graphitePrefix = "gatling2.debug";
        }

        String dbUrl;
        if (this.additionalProperties.containsKey("dbUrl")) {
            dbUrl = this.additionalProperties.get("dbUrl").toString();
        } else {
            dbUrl = "jdbc://localhost:3306";
        }

        String dbUsername;
        if (this.additionalProperties.containsKey("dbUsername")) {
            dbUsername = this.additionalProperties.get("dbUsername").toString();
        } else {
            dbUsername = "root";
        }

        String dbPassword;
        if (this.additionalProperties.containsKey("dbPassword")) {
            dbPassword = this.additionalProperties.get("dbPassword").toString();
        } else {
            dbPassword = "perfana";
        }

        String feederPackage = this.apiPackage.replace(".api", ".feeders");
        String configurationPackage = this.apiPackage.replace(".api", ".configuration");
        String helperPackage = this.apiPackage.replace(".api", ".helpers");
        String setUpPackage = this.apiPackage.replace(".api", ".setup");
        this.additionalProperties.put("systemUnderTest", systemUnderTest);
        this.additionalProperties.put("simulationClassName", simulationClassName);
        this.additionalProperties.put("gatlingVersion", gatlingVersion);
        this.additionalProperties.put("perfanaJavaClientVersion", perfanaJavaClientVersion);
        this.additionalProperties.put("eventsGatlingMavenPluginVersion", eventsGatlingMavenPluginVersion);
        this.additionalProperties.put("testEventsWiremockVersion", testEventsWiremockVersion);
        this.additionalProperties.put("perfanaUrl", perfanaUrl);
        this.additionalProperties.put("targetBaseUrl", targetBaseUrl);
        this.additionalProperties.put("perfanaEnabled", perfanaEnabled);
        this.additionalProperties.put("artifactId", artifactId);
        this.additionalProperties.put("apiVersion", this.apiVersion);
        this.additionalProperties.put("feederPackage", feederPackage);
        this.additionalProperties.put("configurationPackage", configurationPackage);
        this.additionalProperties.put("setUpPackage", setUpPackage);
        this.additionalProperties.put("helperPackage", helperPackage);
        this.additionalProperties.put("influxHost", influxHost);
        this.additionalProperties.put("influxPort", influxPort);
        this.additionalProperties.put("influxProtocol", influxProtocol);
        this.additionalProperties.put("graphitePrefix", graphitePrefix);
        this.additionalProperties.put("dbUrl", dbUrl);
        this.additionalProperties.put("dbUsername", dbUsername);
        this.additionalProperties.put("dbPassword", dbPassword);

        String feederFolder = this.sourceFolder.replace("main", "test") + File.separator + feederPackage.replace('.', File.separatorChar);
        String configurationFolder = this.sourceFolder.replace("main", "test") + File.separator + configurationPackage.replace('.', File.separatorChar);
        String setUpFolder = this.sourceFolder.replace("main", "test") + File.separator + setUpPackage.replace('.', File.separatorChar);
        String helperFolder = this.sourceFolder.replace("main", "test") + File.separator + helperPackage.replace('.', File.separatorChar);
        this.supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        this.supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        this.supportingFiles.add(new SupportingFile("logback.xml", this.resourceFolder, "logback.xml"));
        this.supportingFiles.add(new SupportingFile("gatling.mustache", this.resourceFolder, "gatling.conf"));
        this.supportingFiles.add(new SupportingFile("members-acc-small.csv", this.dataFolder, "members-acc-small.csv"));
        this.supportingFiles.add(new SupportingFile("members-acc.csv", this.dataFolder, "members-acc.csv"));
        this.supportingFiles.add(new SupportingFile("csvFeeders.mustache", feederFolder, "CsvFeeders.kt"));
        this.supportingFiles.add(new SupportingFile("jdbcFeeders.mustache", feederFolder, "JdbcFeeders.kt"));
        this.supportingFiles.add(new SupportingFile("jsonFeeders.mustache", feederFolder, "JsonFeeders.kt"));
        this.supportingFiles.add(new SupportingFile("MemberIdFeeders.mustache", feederFolder, "MemberIdFeeders.kt"));
        this.supportingFiles.add(new SupportingFile("httpProtocolConfiguration.mustache", configurationFolder, "HttpProtocolConfiguration.kt"));
        this.supportingFiles.add(new SupportingFile("scenarioConfiguration.mustache", configurationFolder, "ScenarioConfiguration.kt"));
        this.supportingFiles.add(new SupportingFile("testConfiguration.mustache", configurationFolder, "TestConfiguration.kt"));
        this.supportingFiles.add(new SupportingFile("configurationDumper.mustache", helperFolder, "ConfigurationDumper.kt"));
        this.supportingFiles.add(new SupportingFile("idePathHelper.mustache", helperFolder, "IDEPathHelper.kt"));
        this.supportingFiles.add(new SupportingFile("JWTGenerator.mustache", helperFolder, "JWTGenerator.kt"));
        this.supportingFiles.add(new SupportingFile("scenarios.mustache", setUpFolder, "Scenarios.kt"));
        this.supportingFiles.add(new SupportingFile("setup.mustache", setUpFolder, simulationClassName + ".kt"));
    }

    public String escapeReservedWord(String name) {
        return name;
    }

    public String modelFileFolder() {
        return this.outputFolder + File.separator + this.sourceFolder.replace("main", "test") +
                File.separator + this.modelPackage().replace('.', File.separatorChar);
    }

    public String apiFileFolder() {
        return this.outputFolder + File.separator + this.sourceFolder.replace("main", "test") +
                File.separator + this.apiPackage().replace('.', File.separatorChar);
    }

    public void preprocessOpenAPI(OpenAPI openAPI) {
        Iterator<String> var2 = openAPI.getPaths().keySet().iterator();

        label96:
        while (true) {
            String pathname;
            PathItem path;
            do {
                if (!var2.hasNext()) {
                    return;
                }

                pathname = var2.next();
                path = openAPI.getPaths().get(pathname);
            } while (path.readOperations() == null);

            Iterator<Operation> var5 = path.readOperations().iterator();

            while (true) {
                Operation operation;
                Schema<?> schema;
                do {
                    do {
                        RequestBody requestBody;
                        do {
                            if (!var5.hasNext()) {
                                continue label96;
                            }

                            operation = var5.next();
                            if (operation.getExtensions() == null) {
                                operation.setExtensions(new HashMap<>());
                            }

                            if (!operation.getExtensions().containsKey("x-gatling-path")) {
                                if (pathname.contains("{")) {
                                    String gatlingPath = pathname.replaceAll("\\{", "\\#\\{");
                                    operation.addExtension("x-gatling-path", gatlingPath);
                                } else {
                                    operation.addExtension("x-gatling-path", pathname);
                                }
                            }

                            Map<String, Set<Parameter>> parametersByType = new HashMap<>();

                            if (operation.getParameters() != null) {
                                operation.getParameters().forEach(parameter -> {
                                    String parameterType = parameter.getIn().toLowerCase();
                                    Set<Parameter> parameterSet = parametersByType.computeIfAbsent(parameterType,
                                            k -> new HashSet<>());
                                    parameterSet.add(parameter);
                                });
                            }

                            Set<Parameter> headerParameters = parametersByType.getOrDefault("header", new HashSet<>());
                            Set<Parameter> formParameters = parametersByType.getOrDefault("form", new HashSet<>());
                            Set<Parameter> queryParameters = parametersByType.getOrDefault("query", new HashSet<>());
                            Set<Parameter> pathParameters = parametersByType.getOrDefault("path", new HashSet<>());

                            this.prepareGatlingData(operation, headerParameters, "header");
                            this.prepareGatlingData(operation, formParameters, "form");
                            this.prepareGatlingData(operation, queryParameters, "query");
                            this.prepareGatlingData(operation, pathParameters, "path");

                            requestBody = operation.getRequestBody();
                            ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());
                        } while (requestBody == null);

                        schema = ModelUtils.getSchemaFromRequestBody(requestBody);
                    } while (schema == null);
                } while (schema.get$ref() == null);

                String[] refArray = schema.get$ref().split("/");
                String bodySchemaName = refArray[refArray.length - 1];
                operation.addExtension("x-gatling-body-object", bodySchemaName + ".toStringBody");
                Set<String> sessionBodyVars = new HashSet<>();

                Schema model = openAPI.getComponents().getSchemas().get(bodySchemaName);
                Map<String, Schema<?>> propertiesMap = model.getProperties();

                if (propertiesMap == null) {
                    continue;
                }

                propertiesMap.forEach((key, value) -> {
                    sessionBodyVars.add("\"#{" + key + "}\"");
                });

                operation.addExtension("x-gatling-body-feeder", operation.getOperationId() + "BodyFeeder");
                operation.addExtension("x-gatling-body-feeder-params", StringUtils.join(sessionBodyVars, ","));

                ObjectNode feederBodyNode = createJsonFromSchema(propertiesMap);
                writeBodyJsonFeederDataFile(operation, feederBodyNode);

                ObjectNode requestBodyNode = createJsonRequestBodyFromSchema(propertiesMap);
                writeBodyJsonFile(operation, requestBodyNode);
            }
        }
    }

    private ObjectNode createJsonRequestBodyFromSchema(Map<String, Schema<?>> propertiesMap) {
        ObjectNode rootNode = this.objectMapper.createObjectNode();

        for (Map.Entry<String, Schema<?>> stringSchemaEntry : propertiesMap.entrySet()) {
            Schema<?> schema = stringSchemaEntry.getValue();
            String propertyName = stringSchemaEntry.getKey();

            if (schema instanceof ArraySchema && schema.getType().equals("array")) {
                ArrayNode arrayNode = parseArraySchema(schema, this.objectMapper, this.openAPI, true);
                rootNode.set(propertyName, arrayNode);
            } else if (schema.get$ref() != null) {
                Map<String, Schema<?>> nestedPropertiesMap = getNestedProperties(schema, this.openAPI);
                rootNode.set(propertyName, this.createJsonRequestBodyFromSchema(nestedPropertiesMap));
            } else {
                rootNode.put(propertyName, "#{" + propertyName + "}");
            }
        }
        return rootNode;
    }

    private ObjectNode createJsonFromSchema(Map<String, Schema<?>> propertiesMap) {
        ObjectNode rootNode = this.objectMapper.createObjectNode();

        for (Map.Entry<String, Schema<?>> stringSchemaEntry : propertiesMap.entrySet()) {
            Schema<?> schema = stringSchemaEntry.getValue();
            String propertyName = stringSchemaEntry.getKey();

            if (schema instanceof ArraySchema && schema.getType().equals("array")) {
                ArrayNode arrayNode = parseArraySchema(schema, this.objectMapper, this.openAPI, false);
                rootNode.set(propertyName, arrayNode);
            } else if (schema.get$ref() != null) {
                Map<String, Schema<?>> nestedPropertiesMap = getNestedProperties(schema, this.openAPI);
                rootNode.set(propertyName, this.createJsonFromSchema(nestedPropertiesMap));
            } else {
                if (schema.getMaximum() != null) {
                    rootNode.put(propertyName, schema.getMaximum());
                } else if (schema.getFormat() != null) {
                    LocalDateTime currentDateTime = LocalDateTime.now();
                    switch (schema.getFormat()) {
                        case "date":
                            String dateString = currentDateTime.toLocalDate().toString();
                            rootNode.put(propertyName, dateString);
                            break;
                        case "date-time":
                            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                            String formattedDateTime = currentDateTime.format(formatter);
                            rootNode.put(propertyName, formattedDateTime);
                            break;
                        case "int32":
                        case "int64":
                            rootNode.put(propertyName, 0);
                            break;
                        case "uuid":
                            rootNode.put(propertyName, "9c5f2640-a590-4f14-8e88-536764e57251");
                            break;
                        default:
                            rootNode.put(propertyName, schema.getFormat());
                            break;
                    }
                } else if (schema.getEnum() != null) {
                    String enumValue = schema.getEnum().get(0).toString();
                    rootNode.put(propertyName, enumValue);
                } else if (schema.getPattern() != null) {
                    rootNode.put(propertyName, schema.getPattern());
                } else if (schema.getType() != null) {
                    switch (schema.getType()) {
                        case "boolean":
                            rootNode.put(propertyName, true);
                            break;
                        case "number":
                        case "integer":
                            rootNode.put(propertyName, 0);
                            break;
                        default:
                            rootNode.put(propertyName, schema.getType());
                            break;
                    }
                }
            }
        }
        return rootNode;
    }

    private ArrayNode parseArraySchema(Schema<?> schema, ObjectMapper objectMapper, OpenAPI openAPI, boolean isBody) {
        Schema<?> items = schema.getItems();
        ArrayNode arrayNode = objectMapper.createArrayNode();

        if (items.get$ref() != null) {
            String[] refArray = items.get$ref().split("/");
            Schema nestedModel = openAPI.getComponents().getSchemas().get(refArray[refArray.length - 1]);
            Map<String, Schema<?>> nestedPropertiesMap = nestedModel.getProperties();
            ObjectNode jsonFromSchema;

            if (isBody) {
                jsonFromSchema = this.createJsonRequestBodyFromSchema(nestedPropertiesMap);
            } else {
                jsonFromSchema = this.createJsonFromSchema(nestedPropertiesMap);
            }

            arrayNode.add(jsonFromSchema);
        } else {
            arrayNode.add(items.getType());
        }
        return arrayNode;
    }

    private Map<String, Schema<?>> getNestedProperties(Schema<?> schema, OpenAPI openAPI) {
        String[] refArray = schema.get$ref().split("/");
        Schema nestedModel = openAPI.getComponents().getSchemas().get(refArray[refArray.length - 1]);
        return nestedModel.getProperties();
    }

    private void writeBodyJsonFile(Operation operation, ObjectNode node) {
        try {
            String jsonBodyParams = this.objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(node);
            String bodyFilePath = this.outputFolder + File.separator + this.bodiesFolder + File.separator +
                    operation.getOperationId().replace("_", "") + "Body.json";

            FileUtils.writeStringToFile(new File(bodyFilePath), jsonBodyParams, StandardCharsets.UTF_8);
        } catch (IOException var5) {
            LOGGER.error("Could not create request body file for operationId" + operation.getOperationId(), var5);
        }
    }

    private void writeBodyJsonFeederDataFile(Operation operation, ObjectNode node) {
        try {
            String jsonFeeder = this.objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(node);
            String bodyParamsFeederFilePath = this.outputFolder + File.separator + this.dataFolder +
                    File.separator + operation.getOperationId() + "-bodyParams.json";

            FileUtils.writeStringToFile(new File(bodyParamsFeederFilePath),
                    "[" + replaceIntegerVariable(jsonFeeder) + "]", StandardCharsets.UTF_8);
        } catch (IOException var5) {
            LOGGER.error("Could not create body feeder file for operationId" + operation.getOperationId(), var5);
        }
    }

    public static String replaceIntegerVariable(String text) {
        return VAR_PATTERN.matcher(text).replaceAll("$1");
    }

    private void prepareGatlingData(Operation operation, Set<Parameter> parameters, String parameterType) {
        if (parameters.size() > 0 && !Objects.equals(parameterType, "body")) {
            List<Object> vendorList = new ArrayList<>();
            Map<String, String> parameterNameType = new HashMap<>();

            for (Parameter parameter : parameters) {
                Map<String, Object> extensionMap = new HashMap<>();
                extensionMap.put("gatlingParamName", parameter.getName());
                extensionMap.put("gatlingParamValue", "#{" + parameter.getName() + "}");
                vendorList.add(extensionMap);

                if (parameter.getSchema() != null) {
                    parameterNameType.put(parameter.getName(), parameter.getSchema().getType());
                }
            }

            operation.addExtension("x-gatling-" + parameterType.toLowerCase(Locale.ROOT) +
                    "-params", vendorList);
            operation.addExtension("x-gatling-" + parameterType.toLowerCase(Locale.ROOT) +
                    "-feeder", operation.getOperationId() + parameterType.toUpperCase(Locale.ROOT) + "Feeder");
            operation.addExtension("x-gatling-" + parameterType.toLowerCase(Locale.ROOT) +
                    "-database-feeder", operation.getOperationId() + parameterType.toUpperCase(Locale.ROOT) + "DatabaseFeeder");

            try {
                String feederFilePath = this.outputFolder + File.separator + this.dataFolder +
                        File.separator + operation.getOperationId().replace("_", "") + "-" +
                        parameterType.toLowerCase(Locale.ROOT) + "Params.csv";

                ImmutablePair<String, String> csvData = generateCsvFeederData(parameterNameType);

                FileUtils.writeStringToFile(new File(feederFilePath),
                        csvData.getLeft() + NEW_ROW + csvData.getRight(), StandardCharsets.UTF_8);
            } catch (IOException var9) {
                LOGGER.error("Could not create feeder file for operationId" + operation.getOperationId(), var9);
            }
        }
    }

    private ImmutablePair<String, String> generateCsvFeederData(Map<String, String> parameterNameType) {
        List<String> typeValues = new ArrayList<>();
        String headerRowString = String.join(",", parameterNameType.keySet());

        parameterNameType.values()
                .forEach(value -> {
                    switch (value) {
                        case "boolean":
                            typeValues.add("false");
                            break;
                        case "number":
                        case "integer":
                            typeValues.add("0");
                            break;
                        default:
                            typeValues.add("string");
                            break;
                    }
                });
        String valuesRowString = String.join(",", typeValues);

        return new ImmutablePair<>(headerRowString, valuesRowString);
    }

    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema<?> inner = ap.getItems();
            return this.getSchemaType(p) + "[" + this.getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = this.getAdditionalProperties(p);
            return this.getSchemaType(p) + "[String, " + this.getTypeDeclaration(inner) + "]";
        } else {
            return super.getTypeDeclaration(p);
        }
    }

    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type;
        if (this.typeMapping.containsKey(schemaType)) {
            type = this.typeMapping.get(schemaType);
            if (this.languageSpecificPrimitives.contains(type)) {
                return this.toModelName(type);
            }
        } else {
            type = schemaType;
        }

        return this.toModelName(type);
    }

    static {
        properties = Collections.singletonList(PACKAGE_PROPERTY);
        VAR_PATTERN = Pattern.compile("\"" + PREFIX_INTEGER_VAR + " (.*?)\"");
    }

    public static class PackageProperty extends StringProperty {
        public PackageProperty() {
            super("mainPackage", "Top-level package name, which defines 'apiPackage', 'modelPackage', 'invokerPackage'", "org.openapitools.client");
        }

        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            String mainPackage = this.getValue(additionalProperties);
            String invokerPackage;
            if (!additionalProperties.containsKey("apiPackage")) {
                invokerPackage = mainPackage + ".api";
                additionalProperties.put("apiPackage", invokerPackage);
            }

            if (!additionalProperties.containsKey("modelPackage")) {
                invokerPackage = mainPackage + ".model";
                additionalProperties.put("modelPackage", invokerPackage);
            }

            if (!additionalProperties.containsKey("invokerPackage")) {
                invokerPackage = mainPackage + ".core";
                additionalProperties.put("invokerPackage", invokerPackage);
            }

        }

        public String getApiPackage(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault("apiPackage", "org.openapitools.client.api").toString();
        }

        public String getModelPackage(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault("modelPackage", "org.openapitools.client.model").toString();
        }

        public String getInvokerPackage(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault("invokerPackage", "org.openapitools.client.core").toString();
        }
    }

    public abstract static class Property<T> {
        final String name;
        final String description;
        final T defaultValue;

        public Property(String name, String description, T defaultValue) {
            this.name = name;
            this.description = description;
            this.defaultValue = defaultValue;
        }

        public abstract List<CliOption> toCliOptions();

        public abstract void updateAdditionalProperties(Map<String, Object> var1);

        public abstract T getValue(Map<String, Object> var1);

        public void setValue(Map<String, Object> additionalProperties, T value) {
            additionalProperties.put(this.name, value);
        }
    }

    public static class StringProperty extends Property<String> {
        public StringProperty(String name, String description, String defaultValue) {
            super(name, description, defaultValue);
        }

        public List<CliOption> toCliOptions() {
            return Collections.singletonList(CliOption.newString(this.name, this.description).defaultValue(this.defaultValue));
        }

        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            if (!additionalProperties.containsKey(this.name)) {
                additionalProperties.put(this.name, this.defaultValue);
            }

        }

        public String getValue(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault(this.name, this.defaultValue).toString();
        }
    }
}

