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


public class KotlinPerfanaGatlingCodegen extends AbstractKotlinCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(KotlinPerfanaGatlingCodegen.class);
    private static final String PREFIX_INTEGER_VAR = "I@";
    private static final String NEW_ROW = "\n";
    private static final String ARRAY_TYPE = "array";
    private static final String BOOLEAN_TYPE = "boolean";
    private static final String PROPERTY_KEY_SYSTEM_UNDER_TEST = "systemUnderTest";
    private static final String PROPERTY_KEY_ARTIFACT_ID = "artifactId";
    private static final String PROPERTY_KEY_API_VERSION = "apiVersion";
    private static final String PROPERTY_KEY_FEEDER_PACKAGE = "feederPackage";
    private static final String PROPERTY_KEY_CONFIGURATION_PACKAGE = "configurationPackage";
    private static final String PROPERTY_KEY_SETUP_PACKAGE = "setUpPackage";
    private static final String PROPERTY_KEY_HELPER_PACKAGE = "helperPackage";
    private static final String PROPERTY_KEY_API_PACKAGE = "apiPackage";
    private static final String PROPERTY_KEY_MODEL_PACKAGE = "modelPackage";
    private static final String PROPERTY_KEY_INVOKER_PACKAGE = "invokerPackage";
    private static final String PROPERTY_KEY_SIMULATION_CLASS_NAME = "simulationClassName";
    private static final String PROPERTY_KEY_GATLING_VERSION = "gatlingVersion";
    private static final String PROPERTY_KEY_EVENTS_GATLING_MAVEN_PLUGIN_VERSION = "eventsGatlingMavenPluginVersion";
    private static final String PROPERTY_KEY_PERFANA_JAVA_CLIENT_VERSION = "perfanaJavaClientVersion";
    private static final String PROPERTY_KEY_TEST_EVENTS_WIREMOCK_VERSION = "testEventsWiremockVersion";
    private static final String PROPERTY_KEY_PERFANA_URL = "perfanaUrl";
    private static final String PROPERTY_KEY_TARGET_BASE_URL = "targetBaseUrl";
    private static final String PROPERTY_KEY_PERFANA_ENABLED = "perfanaEnabled";
    private static final String PROPERTY_KEY_INFLUX_HOST = "influxHost";
    private static final String PROPERTY_KEY_INFLUX_PORT = "influxPort";
    private static final String PROPERTY_KEY_INFLUX_PROTOCOL = "influxProtocol";
    private static final String PROPERTY_KEY_GRAPHITE_PREFIX = "graphitePrefix";
    private static final String PROPERTY_KEY_DB_URL = "dbUrl";
    private static final String PROPERTY_KEY_DB_USERNAME = "dbUsername";
    private static final String PROPERTY_KEY_DB_PASSWORD = "dbPassword";
    private static final String PROPERTY_KEY_X_GATLING = "x-gatling-";
    private static final String OPERATION_EXTENSION_X_GATLING_PATH = "x-gatling-path";

    protected String resourceFolder;
    protected String dataFolder;
    protected String bodiesFolder;
    private static final PackageProperty PACKAGE_PROPERTY = new PackageProperty();
    private static final List<Property<?>> properties;
    private final ObjectMapper objectMapper;
    private static final Pattern VAR_PATTERN;

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "kotlin-perfana-gatling";
    }

    @Override
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
        this.modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme)
                        .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                        .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                        .excludeGlobalFeatures(GlobalFeature.XMLStructureDefinitions, GlobalFeature.Callbacks, GlobalFeature.LinkObjects, GlobalFeature.ParameterStyling)
                        .excludeSchemaSupportFeatures(SchemaSupportFeature.Polymorphism)
                        .excludeParameterFeatures(ParameterFeature.Cookie)
                        .includeClientModificationFeatures(ClientModificationFeature.BasePath));
        this.sourceFolder = "src" + File.separator + "test" + File.separator + "kotlin";
        this.outputFolder = "generated-code/gatling";
        this.apiTemplateFiles.put("api.mustache", ".kt");
        this.templateDir = "kotlin-perfana-gatling";
        properties.stream()
                .map(Property::toCliOptions)
                .flatMap(Collection::stream)
                .forEach(option -> this.cliOptions.add(option));
        this.importMapping.remove("List");
        this.importMapping.remove("Set");
        this.importMapping.remove("Map");
        this.importMapping.put("Date", "java.util.Date");
        this.typeMapping = new HashMap<>();
        this.typeMapping.put("enum", "NSString");
        this.typeMapping.put(ARRAY_TYPE, "List");
        this.typeMapping.put("set", "Set");
        this.typeMapping.put(BOOLEAN_TYPE, "Boolean");
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
        this.instantiationTypes.put(ARRAY_TYPE, "ListBuffer");
        this.instantiationTypes.put("map", "HashMap");
        this.setReservedWordsLowerCase(Arrays.asList("path", "contentTypes", "contentType", "queryParams",
                "headerParams", "formParams", "postBody", "mp", "basePath", "apiInvoker", "abstract", "case",
                "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome",
                "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package",
                "private", "protected", "return", "sealed", "super", "this", "throw", "trait", "try", "true", "type",
                "val", "var", "while", "with", "yield"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        properties.forEach(p -> p.updateAdditionalProperties(this.additionalProperties));
        this.invokerPackage = PACKAGE_PROPERTY.getInvokerPackage(this.additionalProperties);
        this.apiPackage = PACKAGE_PROPERTY.getApiPackage(this.additionalProperties);
        this.modelPackage = PACKAGE_PROPERTY.getModelPackage(this.additionalProperties);
        String systemUnderTest;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_SYSTEM_UNDER_TEST)) {
            systemUnderTest = this.additionalProperties.get(PROPERTY_KEY_SYSTEM_UNDER_TEST).toString();
        } else {
            systemUnderTest = "AddSystemUnderTest";
        }

        String artifactId;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_ARTIFACT_ID)) {
            artifactId = this.additionalProperties.get(PROPERTY_KEY_ARTIFACT_ID).toString();
        } else {
            artifactId = "gatling-add-system-under-test";
        }

        String simulationClassName;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_SIMULATION_CLASS_NAME)) {
            simulationClassName = this.additionalProperties.get(PROPERTY_KEY_SIMULATION_CLASS_NAME).toString();
        } else {
            simulationClassName = "AddSimulationClassName";
        }

        String gatlingVersion;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_GATLING_VERSION)) {
            gatlingVersion = this.additionalProperties.get(PROPERTY_KEY_GATLING_VERSION).toString();
        } else {
            gatlingVersion = "3.7.4";
        }

        String eventsGatlingMavenPluginVersion;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_EVENTS_GATLING_MAVEN_PLUGIN_VERSION)) {
            eventsGatlingMavenPluginVersion = this.additionalProperties.get(PROPERTY_KEY_EVENTS_GATLING_MAVEN_PLUGIN_VERSION).toString();
        } else {
            eventsGatlingMavenPluginVersion = "4.1.0-events-2";
        }

        String perfanaJavaClientVersion;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_PERFANA_JAVA_CLIENT_VERSION)) {
            perfanaJavaClientVersion = this.additionalProperties.get(PROPERTY_KEY_PERFANA_JAVA_CLIENT_VERSION).toString();
        } else {
            perfanaJavaClientVersion = "2.0.1";
        }

        String testEventsWiremockVersion;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_TEST_EVENTS_WIREMOCK_VERSION)) {
            testEventsWiremockVersion = this.additionalProperties.get(PROPERTY_KEY_TEST_EVENTS_WIREMOCK_VERSION).toString();
        } else {
            testEventsWiremockVersion = "1.2.0-SNAPSHOT";
        }

        String perfanaUrl;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_PERFANA_URL)) {
            perfanaUrl = this.additionalProperties.get(PROPERTY_KEY_PERFANA_URL).toString();
        } else {
            perfanaUrl = "https://perfana.io";
        }

        String targetBaseUrl;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_TARGET_BASE_URL)) {
            targetBaseUrl = this.additionalProperties.get(PROPERTY_KEY_TARGET_BASE_URL).toString();
        } else {
            targetBaseUrl = "http://your-app.com";
        }

        boolean perfanaEnabled;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_PERFANA_ENABLED)) {
            perfanaEnabled = Boolean.parseBoolean(this.additionalProperties.get(PROPERTY_KEY_PERFANA_ENABLED).toString());
        } else {
            perfanaEnabled = false;
        }

        String influxHost;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_INFLUX_HOST)) {
            influxHost = this.additionalProperties.get(PROPERTY_KEY_INFLUX_HOST).toString();
        } else {
            influxHost = "http://influxdb";
        }

        String influxPort;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_INFLUX_PORT)) {
            influxPort = this.additionalProperties.get(PROPERTY_KEY_INFLUX_PORT).toString();
        } else {
            influxPort = "2003";
        }

        String influxProtocol;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_INFLUX_PROTOCOL)) {
            influxProtocol = this.additionalProperties.get(PROPERTY_KEY_INFLUX_PROTOCOL).toString();
        } else {
            influxProtocol = "tcp";
        }

        String graphitePrefix;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_GRAPHITE_PREFIX)) {
            graphitePrefix = this.additionalProperties.get(PROPERTY_KEY_GRAPHITE_PREFIX).toString();
        } else {
            graphitePrefix = "gatling2.debug";
        }

        String dbUrl;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_DB_URL)) {
            dbUrl = this.additionalProperties.get(PROPERTY_KEY_DB_URL).toString();
        } else {
            dbUrl = "jdbc://localhost:3306";
        }

        String dbUsername;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_DB_USERNAME)) {
            dbUsername = this.additionalProperties.get(PROPERTY_KEY_DB_USERNAME).toString();
        } else {
            dbUsername = "root";
        }

        String dbPassword;
        if (this.additionalProperties.containsKey(PROPERTY_KEY_DB_PASSWORD)) {
            dbPassword = this.additionalProperties.get(PROPERTY_KEY_DB_PASSWORD).toString();
        } else {
            dbPassword = "perfana";
        }

        String feederPackage = this.apiPackage.replace(".api", ".feeders");
        String configurationPackage = this.apiPackage.replace(".api", ".configuration");
        String helperPackage = this.apiPackage.replace(".api", ".helpers");
        String setUpPackage = this.apiPackage.replace(".api", ".setup");
        this.additionalProperties.put(PROPERTY_KEY_SYSTEM_UNDER_TEST, systemUnderTest);
        this.additionalProperties.put(PROPERTY_KEY_SIMULATION_CLASS_NAME, simulationClassName);
        this.additionalProperties.put(PROPERTY_KEY_GATLING_VERSION, gatlingVersion);
        this.additionalProperties.put(PROPERTY_KEY_PERFANA_JAVA_CLIENT_VERSION, perfanaJavaClientVersion);
        this.additionalProperties.put(PROPERTY_KEY_EVENTS_GATLING_MAVEN_PLUGIN_VERSION, eventsGatlingMavenPluginVersion);
        this.additionalProperties.put(PROPERTY_KEY_TEST_EVENTS_WIREMOCK_VERSION, testEventsWiremockVersion);
        this.additionalProperties.put(PROPERTY_KEY_PERFANA_URL, perfanaUrl);
        this.additionalProperties.put(PROPERTY_KEY_TARGET_BASE_URL, targetBaseUrl);
        this.additionalProperties.put(PROPERTY_KEY_PERFANA_ENABLED, perfanaEnabled);
        this.additionalProperties.put(PROPERTY_KEY_ARTIFACT_ID, artifactId);
        this.additionalProperties.put(PROPERTY_KEY_API_VERSION, this.apiVersion);
        this.additionalProperties.put(PROPERTY_KEY_FEEDER_PACKAGE, feederPackage);
        this.additionalProperties.put(PROPERTY_KEY_CONFIGURATION_PACKAGE, configurationPackage);
        this.additionalProperties.put(PROPERTY_KEY_SETUP_PACKAGE, setUpPackage);
        this.additionalProperties.put(PROPERTY_KEY_HELPER_PACKAGE, helperPackage);
        this.additionalProperties.put(PROPERTY_KEY_INFLUX_HOST, influxHost);
        this.additionalProperties.put(PROPERTY_KEY_INFLUX_PORT, influxPort);
        this.additionalProperties.put(PROPERTY_KEY_INFLUX_PROTOCOL, influxProtocol);
        this.additionalProperties.put(PROPERTY_KEY_GRAPHITE_PREFIX, graphitePrefix);
        this.additionalProperties.put(PROPERTY_KEY_DB_URL, dbUrl);
        this.additionalProperties.put(PROPERTY_KEY_DB_USERNAME, dbUsername);
        this.additionalProperties.put(PROPERTY_KEY_DB_PASSWORD, dbPassword);

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

    @Override
    public String escapeReservedWord(String name) {
        return name;
    }

    @Override
    public String modelFileFolder() {
        return this.outputFolder + File.separator + this.sourceFolder.replace("main", "test") +
                File.separator + this.modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return this.outputFolder + File.separator + this.sourceFolder.replace("main", "test") +
                File.separator + this.apiPackage().replace('.', File.separatorChar);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    @Override
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

                            if (!operation.getExtensions().containsKey(OPERATION_EXTENSION_X_GATLING_PATH)) {
                                if (pathname.contains("{")) {
                                    String gatlingPath = pathname.replaceAll("\\{", "\\#\\{");
                                    operation.addExtension(OPERATION_EXTENSION_X_GATLING_PATH, gatlingPath);
                                } else {
                                    operation.addExtension(OPERATION_EXTENSION_X_GATLING_PATH, pathname);
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

            if (schema instanceof ArraySchema && schema.getType().equals(ARRAY_TYPE)) {
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

            if (schema instanceof ArraySchema && schema.getType().equals(ARRAY_TYPE)) {
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
                        case BOOLEAN_TYPE:
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
        if (!parameters.isEmpty() && !Objects.equals(parameterType, "body")) {
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

            operation.addExtension(PROPERTY_KEY_X_GATLING + parameterType.toLowerCase(Locale.ROOT) +
                    "-params", vendorList);
            operation.addExtension(PROPERTY_KEY_X_GATLING + parameterType.toLowerCase(Locale.ROOT) +
                    "-feeder", operation.getOperationId() + parameterType.toUpperCase(Locale.ROOT) + "Feeder");
            operation.addExtension(PROPERTY_KEY_X_GATLING + parameterType.toLowerCase(Locale.ROOT) +
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
                        case BOOLEAN_TYPE:
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

    @Override
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

    @Override
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
            super("mainPackage", "Top-level package name, which defines '" + PROPERTY_KEY_API_PACKAGE + "', " +
                    "'" + PROPERTY_KEY_MODEL_PACKAGE + "', '" + PROPERTY_KEY_INVOKER_PACKAGE + "'", "org.openapitools.client");
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            String mainPackage = this.getValue(additionalProperties);
            String invokerPackage;
            if (!additionalProperties.containsKey(PROPERTY_KEY_API_PACKAGE)) {
                invokerPackage = mainPackage + ".api";
                additionalProperties.put(PROPERTY_KEY_API_PACKAGE, invokerPackage);
            }

            if (!additionalProperties.containsKey(PROPERTY_KEY_MODEL_PACKAGE)) {
                invokerPackage = mainPackage + ".model";
                additionalProperties.put(PROPERTY_KEY_MODEL_PACKAGE, invokerPackage);
            }

            if (!additionalProperties.containsKey(PROPERTY_KEY_INVOKER_PACKAGE)) {
                invokerPackage = mainPackage + ".core";
                additionalProperties.put(PROPERTY_KEY_INVOKER_PACKAGE, invokerPackage);
            }

        }

        public String getApiPackage(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault(PROPERTY_KEY_API_PACKAGE, "org.openapitools.client.api").toString();
        }

        public String getModelPackage(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault(PROPERTY_KEY_MODEL_PACKAGE, "org.openapitools.client.model").toString();
        }

        public String getInvokerPackage(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault(PROPERTY_KEY_INVOKER_PACKAGE, "org.openapitools.client.core").toString();
        }
    }

    public abstract static class Property<T> {
        final String name;
        final String description;
        final T defaultValue;

        protected Property(String name, String description, T defaultValue) {
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

