//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by FernFlower decompiler)
//

package org.openapitools.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
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
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class ScalaPerfanaGatlingCodegen extends AbstractScalaCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ScalaPerfanaGatlingCodegen.class);
    public static final String PREFIX_INTEGER_VAR = "I@";
    protected String resourceFolder;
    protected String dataFolder;
    protected String bodiesFolder;
    protected String apiVersion;
    public static final String DEFAULT_PACKAGE_NAME = "org.openapitools.client";
    private static final PackageProperty PACKAGE_PROPERTY = new PackageProperty();
    private static final List<Property<?>> properties;
    private ObjectMapper objectMapper;
    private static Pattern VAR_PATTERN;

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "scala-perfana-gatling";
    }

    public String getHelp() {
        return "Generates a Gatling script including to be used with Perfana, continuous performance testting dashboard";
    }

    public ScalaPerfanaGatlingCodegen() {
        this.resourceFolder = "src" + File.separator + "test" + File.separator + "resources";
        this.dataFolder = this.resourceFolder + File.separator + "data";
        this.bodiesFolder = this.resourceFolder + File.separator + "bodies";
        this.apiVersion = "1.0.0";
        this.objectMapper = new ObjectMapper();
        this.modifyFeatureSet((features) -> {
            features.includeDocumentationFeatures(new DocumentationFeature[]{DocumentationFeature.Readme}).wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom)).securityFeatures(EnumSet.noneOf(SecurityFeature.class)).excludeGlobalFeatures(new GlobalFeature[]{GlobalFeature.XMLStructureDefinitions, GlobalFeature.Callbacks, GlobalFeature.LinkObjects, GlobalFeature.ParameterStyling}).excludeSchemaSupportFeatures(new SchemaSupportFeature[]{SchemaSupportFeature.Polymorphism}).excludeParameterFeatures(new ParameterFeature[]{ParameterFeature.Cookie}).includeClientModificationFeatures(new ClientModificationFeature[]{ClientModificationFeature.BasePath});
        });
        this.sourceFolder = "src" + File.separator + "test" + File.separator + "scala";
        this.outputFolder = "generated-code/gatling";
        this.apiTemplateFiles.put("api.mustache", ".scala");
        this.templateDir = "scala-perfana-gatling";
        properties.stream().map(Property::toCliOptions).flatMap(Collection::stream).forEach((option) -> {
            this.cliOptions.add(option);
        });
        this.importMapping.remove("List");
        this.importMapping.remove("Set");
        this.importMapping.remove("Map");
        this.importMapping.put("Date", "java.util.Date");
        this.typeMapping = new HashMap();
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
        properties.forEach((p) -> {
            p.updateAdditionalProperties(this.additionalProperties);
        });
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
            simulationClassName = "AddSimulationClassNAme";
        }

        String gatlingVersion;
        if (this.additionalProperties.containsKey("gatlingVersion")) {
            gatlingVersion = this.additionalProperties.get("gatlingVersion").toString();
        } else {
            gatlingVersion = "3.3.1";
        }

        String eventsGatlingMavenPluginVersion;
        if (this.additionalProperties.containsKey("eventsGatlingMavenPluginVersion")) {
            eventsGatlingMavenPluginVersion = this.additionalProperties.get("eventsGatlingMavenPluginVersion").toString();
        } else {
            eventsGatlingMavenPluginVersion = "3.1.0-events-2.0-SNAPSHOT";
        }

        String perfanaJavaClientVersion;
        if (this.additionalProperties.containsKey("perfanaJavaClientVersion")) {
            perfanaJavaClientVersion = this.additionalProperties.get("perfanaJavaClientVersion").toString();
        } else {
            perfanaJavaClientVersion = "1.5.0-SNAPSHOT";
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
            perfanaUrl = "1.2.0-SNAPSHOT";
        }

        String targetBaseUrl;
        if (this.additionalProperties.containsKey("targetBaseUrl")) {
            targetBaseUrl = this.additionalProperties.get("targetBaseUrl").toString();
        } else {
            targetBaseUrl = "http://your-app.com";
        }

        boolean perfanaEnabled;
        if (this.additionalProperties.containsKey("perfanaEnabled")) {
            perfanaEnabled = (boolean) this.additionalProperties.get("perfanaEnabled");
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
        this.additionalProperties.put("simulationClassName", simulationClassName);
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
        this.supportingFiles.add(new SupportingFile("csvFeeders.mustache", feederFolder, "CsvFeeders.scala"));
        this.supportingFiles.add(new SupportingFile("jdbcFeeders.mustache", feederFolder, "JdbcFeeders.scala"));
        this.supportingFiles.add(new SupportingFile("memberIdFeeders.mustache", feederFolder, "MemberIdFeeders.scala"));
        this.supportingFiles.add(new SupportingFile("jsonFeeders.mustache", feederFolder, "JsonFeeders.scala"));
        this.supportingFiles.add(new SupportingFile("timestampFeeder.mustache", feederFolder, "TimestampFeeder.scala"));
        this.supportingFiles.add(new SupportingFile("jwtFeeder.mustache", feederFolder, "JwtFeeder.scala"));
        this.supportingFiles.add(new SupportingFile("jwtGenerator.mustache", helperFolder, "JWTGenerator.scala"));
        this.supportingFiles.add(new SupportingFile("httpProtocolConfiguration.mustache", configurationFolder, "HttpProtocolConfiguration.scala"));
        this.supportingFiles.add(new SupportingFile("scenarioConfiguration.mustache", configurationFolder, "ScenarioConfiguration.scala"));
        this.supportingFiles.add(new SupportingFile("testConfiguration.mustache", configurationFolder, "TestConfiguration.scala"));
        this.supportingFiles.add(new SupportingFile("configurationDumper.mustache", helperFolder, "ConfigurationDumper.scala"));
        this.supportingFiles.add(new SupportingFile("idePathHelper.mustache", helperFolder, "IDEPathHelper.scala"));
        this.supportingFiles.add(new SupportingFile("utils.mustache", helperFolder, "Utils.scala"));
        this.supportingFiles.add(new SupportingFile("scenarios.mustache", setUpFolder, "Scenarios.scala"));
        this.supportingFiles.add(new SupportingFile("setup.mustache", setUpFolder, simulationClassName + ".scala"));
    }

    public String escapeReservedWord(String name) {
        return name;
    }

    public String modelFileFolder() {
        return this.outputFolder + File.separator + this.sourceFolder.replace("main", "test") + File.separator + this.modelPackage().replace('.', File.separatorChar);
    }

    public String apiFileFolder() {
        return this.outputFolder + File.separator + this.sourceFolder.replace("main", "test") + File.separator + this.apiPackage().replace('.', File.separatorChar);
    }

    public void preprocessOpenAPI(OpenAPI openAPI) {
        Iterator var2 = openAPI.getPaths().keySet().iterator();

        label96:
        while(true) {
            String pathname;
            PathItem path;
            do {
                if (!var2.hasNext()) {
                    return;
                }

                pathname = (String)var2.next();
                path = (PathItem)openAPI.getPaths().get(pathname);
            } while(path.readOperations() == null);

            Iterator var5 = path.readOperations().iterator();

            while(true) {
                Operation operation;
                Schema schema;
                do {
                    do {
                        RequestBody requestBody;
                        do {
                            if (!var5.hasNext()) {
                                continue label96;
                            }

                            operation = (Operation)var5.next();
                            if (operation.getExtensions() == null) {
                                operation.setExtensions(new HashMap());
                            }

                            if (!operation.getExtensions().keySet().contains("x-gatling-path")) {
                                if (pathname.contains("{")) {
                                    String gatlingPath = pathname.replaceAll("\\{", "\\#\\{");
                                    operation.addExtension("x-gatling-path", gatlingPath);
                                } else {
                                    operation.addExtension("x-gatling-path", pathname);
                                }
                            }

                            Set<Parameter> headerParameters = new HashSet();
                            Set<Parameter> formParameters = new HashSet();
                            Set<Parameter> queryParameters = new HashSet();
                            Set<Parameter> pathParameters = new HashSet();
                            if (operation.getParameters() != null) {
                                Iterator var11 = operation.getParameters().iterator();

                                while(var11.hasNext()) {
                                    Parameter parameter = (Parameter)var11.next();
                                    if (parameter.getIn().equalsIgnoreCase("header")) {
                                        headerParameters.add(parameter);
                                    }

                                    if (parameter.getIn().equalsIgnoreCase("query")) {
                                        queryParameters.add(parameter);
                                    }

                                    if (parameter.getIn().equalsIgnoreCase("path")) {
                                        pathParameters.add(parameter);
                                    }
                                }
                            }

                            this.prepareGatlingData(operation, headerParameters, "header");
                            this.prepareGatlingData(operation, formParameters, "form");
                            this.prepareGatlingData(operation, queryParameters, "query");
                            this.prepareGatlingData(operation, pathParameters, "path");
                            requestBody = operation.getRequestBody();
                            ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());
                        } while(requestBody == null);

                        schema = ModelUtils.getSchemaFromRequestBody(requestBody);
                    } while(schema == null);
                } while(schema.get$ref() == null);

                String[] refArray = schema.get$ref().split("\\/");
                operation.addExtension("x-gatling-body-object", refArray[refArray.length - 1] + ".toStringBody");
                Set<String> bodyFeederParams = new HashSet();
                Set<String> sessionBodyVars = new HashSet();
                Schema model = (Schema)openAPI.getComponents().getSchemas().get(refArray[refArray.length - 1]);
                Map<String, Schema> propertiesMap = model.getProperties();
                Iterator var19 = propertiesMap.entrySet().iterator();

                while(var19.hasNext()) {
                    Map.Entry<String, Schema> propertyEntry = (Map.Entry)var19.next();
                    bodyFeederParams.add((String)propertyEntry.getKey());
                    sessionBodyVars.add("\"#{" + (String)propertyEntry.getKey() + "}\"");
                }

                operation.addExtension("x-gatling-body-feeder", operation.getOperationId() + "BodyFeeder");
                operation.addExtension("x-gatling-body-feeder-params", StringUtils.join(sessionBodyVars, ","));
                ObjectNode jsonFeederNode = this.createJsonFeeder(propertiesMap);

                String jsonFeeder;
                try {
                    jsonFeeder = this.objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(jsonFeederNode);
                } catch (JsonProcessingException var23) {
                    throw new RuntimeException("Writing object to json failed.", var23);
                }

                try {
                    FileUtils.writeStringToFile(new File(this.outputFolder + File.separator + this.dataFolder + File.separator + operation.getOperationId() + "-bodyParams.json"), "[" + replaceIntegerVariable(jsonFeeder) + "]", StandardCharsets.UTF_8);
                    this.prepareGatlingRequestBodies(operation, bodyFeederParams);
                } catch (IOException var22) {
                    LOGGER.error("Could not create feeder file for operationId" + operation.getOperationId(), var22);
                }
            }
        }
    }

    private void prepareGatlingRequestBodies(Operation operation, Set<String> parameters) {
        String jsonBody = (String)parameters.stream().map((par) -> {
            return "\"" + par + "\": \"#{" + par + "}\"";
        }).collect(Collectors.joining(",\n\t", "{\n\t", "\n}"));

        try {
            FileUtils.writeStringToFile(new File(this.outputFolder + File.separator + this.bodiesFolder + File.separator + operation.getOperationId().replace("_", "") + "Body.json"), jsonBody, StandardCharsets.UTF_8);
        } catch (IOException var5) {
            LOGGER.error("Could not create request body file for operationId" + operation.getOperationId(), var5);
        }

    }

    private ObjectNode createJsonFeeder(Map<String, Schema> propertiesMap) {
        ObjectNode rootNode = this.objectMapper.createObjectNode();
        Iterator var3 = propertiesMap.entrySet().iterator();

        while(var3.hasNext()) {
            Map.Entry<String, Schema> propertyEntry = (Map.Entry)var3.next();
            Schema schema = (Schema)propertyEntry.getValue();
            String key = (String)propertyEntry.getKey();
            if (schema instanceof ArraySchema) {
                if ("string".equals(schema.getType())) {
                    rootNode.put(key, this.asVariable(key));
                } else if ("boolean".equals(schema.getType())) {
                    rootNode.put(key, Boolean.TRUE);
                } else if ("array".equals(schema.getType())) {
                    Schema<?> items = ((ArraySchema)schema).getItems();
                    ArrayNode arrayNode = this.objectMapper.createArrayNode();
                    if ("integer".equals(items.getType())) {
                        arrayNode.add(this.asIntegerArrayVariable(key));
                    } else if ("string".equals(items.getType())) {
                        arrayNode.add(this.asArrayVariable(key));
                    } else {
                        arrayNode.add(this.asIntegerArrayVariable(key));
                    }

                    rootNode.set(key, arrayNode);
                }
            } else if (schema instanceof IntegerSchema) {
                rootNode.put(key, this.asIntegerVariable(key));
            } else if (schema instanceof StringSchema) {
                rootNode.put(key, this.asVariable(key));
            } else if (schema.get$ref() != null) {
                String[] refArray = schema.get$ref().split("\\/");
                Schema nestedModel = (Schema)this.openAPI.getComponents().getSchemas().get(refArray[refArray.length - 1]);
                Map<String, Schema> nestedPropertiesMap = nestedModel.getProperties();
                rootNode.set(key, this.createJsonFeeder(nestedPropertiesMap));
            }
        }

        return rootNode;
    }

    private String asIntegerArrayVariable(String key) {
        return "I@" + this.asArrayVariable(key);
    }

    private String asArrayVariable(String key) {
        return "#{" + key + "(0)}";
    }

    private String asVariable(String key) {
        return "#{" + key + "}";
    }

    private String asIntegerVariable(String key) {
        return "I@" + this.asVariable(key);
    }

    public static String replaceIntegerVariable(String text) {
        return VAR_PATTERN.matcher(text).replaceAll("$1");
    }

    private void prepareGatlingData(Operation operation, Set<Parameter> parameters, String parameterType) {
        if (parameters.size() > 0 && parameterType != "body") {
            List<String> parameterNames = new ArrayList();
            List<Object> vendorList = new ArrayList();
            Iterator var6 = parameters.iterator();

            while(var6.hasNext()) {
                Parameter parameter = (Parameter)var6.next();
                Map<String, Object> extensionMap = new HashMap();
                extensionMap.put("gatlingParamName", parameter.getName());
                extensionMap.put("gatlingParamValue", "#{" + parameter.getName() + "}");
                vendorList.add(extensionMap);
                parameterNames.add(parameter.getName());
            }

            operation.addExtension("x-gatling-" + parameterType.toLowerCase(Locale.ROOT) + "-params", vendorList);
            operation.addExtension("x-gatling-" + parameterType.toLowerCase(Locale.ROOT) + "-feeder", operation.getOperationId() + parameterType.toUpperCase(Locale.ROOT) + "Feeder");
            operation.addExtension("x-gatling-" + parameterType.toLowerCase(Locale.ROOT) + "-database-feeder", operation.getOperationId() + parameterType.toUpperCase(Locale.ROOT) + "DatabaseFeeder");

            try {
                FileUtils.writeStringToFile(new File(this.outputFolder + File.separator + this.dataFolder + File.separator + operation.getOperationId().replace("_", "") + "-" + parameterType.toLowerCase(Locale.ROOT) + "Params.csv"), StringUtils.join(parameterNames, ",") + "\n" + StringUtils.join(parameterNames, ",").replaceAll("([^,]+)", "dummy-data"), StandardCharsets.UTF_8);
            } catch (IOException var9) {
                LOGGER.error("Could not create feeder file for operationId" + operation.getOperationId(), var9);
            }
        }

    }

    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema)p;
            Schema inner = ap.getItems();
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
        String type = null;
        if (this.typeMapping.containsKey(schemaType)) {
            type = (String)this.typeMapping.get(schemaType);
            if (this.languageSpecificPrimitives.contains(type)) {
                return this.toModelName(type);
            }
        } else {
            type = schemaType;
        }

        return this.toModelName(type);
    }

    static {
        properties = Arrays.asList(PACKAGE_PROPERTY);
        VAR_PATTERN = Pattern.compile("\"I@(.*?)\"");
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
            return Collections.singletonList(CliOption.newString(this.name, this.description).defaultValue((String)this.defaultValue));
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
