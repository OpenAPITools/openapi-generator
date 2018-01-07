package io.swagger.codegen.languages;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenResponse;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;
import io.swagger.models.Tag;
import io.swagger.models.parameters.FormParameter;
import io.swagger.models.parameters.Parameter;

/**
 * Created by prokarma on 04/09/17.
 */
public class JavaPKMSTServerCodegen extends AbstractJavaCodegen {

    public static final String FULL_JAVA_UTIL = "fullJavaUtil";
    public static final String SUPPORT_JAVA6 = "supportJava6";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String TITLE = "title";
    public static final String WITH_XML = "withXml";
    public static final String EUREKA_URI = "eurekaUri";
    public static final String ZIPKIN_URI = "zipkinUri";
    public static final String SPRINGADMIN_URI = "springBootAdminUri";
    protected String projectFolder;
    protected String projectTestFolder;
    protected String sourceFolder;
    protected String testFolder;
    protected String basePackage = "com.prokarma.pkmst";
    protected String serviceName = "Pkmst";
    protected String configPackage = "com.prokarma.pkmst.config";
    protected boolean implicitHeaders = false;
    protected boolean serializeBigDecimalAsString = false;
    protected boolean withXml = false;
    protected boolean fullJavaUtil;
    protected String javaUtilPrefix = "";
    protected Boolean serializableModel = false;
    protected String invokerPackage;
    protected String title;
    protected String eurekaUri;
    protected String zipkinUri;
    protected String springBootAdminUri;

    public JavaPKMSTServerCodegen() {
        super();
        this.projectFolder = "src" + File.separator + "main";
        this.projectTestFolder = "src" + File.separator + "test";
        this.sourceFolder = this.projectFolder + File.separator + "java";
        this.testFolder = this.projectTestFolder + File.separator + "java";
        groupId = "com.prokarma";
        artifactId = "pkmst-microservice";
        artifactVersion = "1.0.0";
        embeddedTemplateDir = templateDir = "java-pkmst";
        apiPackage = "com.prokarma.pkmst.controller";
        modelPackage = "com.prokarma.pkmst.model";
        invokerPackage = "com.prokarma.pkmst.controller";
        additionalProperties.put("jackson", "true");
        this.cliOptions.add(new CliOption("groupId", "groupId in generated pom.xml"));
        this.cliOptions.add(new CliOption("artifactId", "artifactId in generated pom.xml"));
        this.cliOptions.add(new CliOption("artifactVersion", "artifact version in generated pom.xml"));
        this.cliOptions.add(new CliOption("basePackage", "base package for java source code"));
        this.cliOptions.add(new CliOption("serviceName", "Service Name"));
        this.cliOptions.add(new CliOption(TITLE, "server title name or client service name"));
        this.cliOptions.add(new CliOption("eurekaUri", "Eureka URI"));
        this.cliOptions.add(new CliOption("zipkinUri", "Zipkin URI"));
        this.cliOptions.add(new CliOption("springBootAdminUri", "Spring-Boot URI"));
        // Middleware config
        this.cliOptions.add(new CliOption("pkmstInterceptor", "PKMST Interceptor"));
        this.apiTestTemplateFiles.put("api_test.mustache", ".java");
        
        if (".md".equals(this.modelDocTemplateFiles.get("model_doc.mustache"))) {
            this.modelDocTemplateFiles.remove("model_doc.mustache");
        }
        if (".md".equals(this.apiDocTemplateFiles.get("api_doc.mustache"))) {
            this.apiDocTemplateFiles.remove("api_doc.mustache");
        }
    }

    private static String getAccept(Operation operation) {
        String accepts = null;
        String defaultContentType = "application/json";
        if (operation.getProduces() != null && !operation.getProduces().isEmpty()) {
            StringBuilder sb = new StringBuilder();
            for (String produces : operation.getProduces()) {
                if (defaultContentType.equalsIgnoreCase(produces)) {
                    accepts = defaultContentType;
                    break;
                } else {
                    if (sb.length() > 0) {
                        sb.append(",");
                    }
                    sb.append(produces);
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

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "java-pkmst";
    }

    public String getHelp() {
        return "Generates a PKMST SpringBoot Server application using the SpringFox integration."
                + " Also enables EurekaServerClient / Zipkin / Spring-Boot admin";
    }

    public void processOpts() {
        super.processOpts();
        if (this.additionalProperties.containsKey("basePackage")) {
            this.setBasePackage((String) this.additionalProperties.get("basePackage"));
            this.setInvokerPackage(this.getBasePackage());
            this.apiPackage = this.getBasePackage() + ".controller";
            this.modelPackage = this.getBasePackage() + ".model";
            this.setConfigPackage(this.getBasePackage() + ".config");
        } else {
            this.additionalProperties.put(BASE_PACKAGE, basePackage);
            this.additionalProperties.put(CONFIG_PACKAGE, this.getConfigPackage());
            this.additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
            this.additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
            this.additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }
        if (this.additionalProperties.containsKey("groupId")) {
            this.setGroupId((String) this.additionalProperties.get("groupId"));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        }
        if (this.additionalProperties.containsKey("artifactId")) {
            this.setArtifactId((String) this.additionalProperties.get("artifactId"));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        }
        if (this.additionalProperties.containsKey("artifactVersion")) {
            this.setArtifactVersion((String) this.additionalProperties.get("artifactVersion"));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }
        if (this.additionalProperties.containsKey("serviceName")) {
            this.setServiceName((String) this.additionalProperties.get("serviceName"));
        } else {
            // not set, use to be passed to template
            additionalProperties.put("serviceName", serviceName);
        }

        if (this.additionalProperties.containsKey(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING)) {
            this.setSerializeBigDecimalAsString(Boolean.valueOf(
                    this.additionalProperties.get(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING).toString()));
        }
        if (this.additionalProperties.containsKey(CodegenConstants.SERIALIZABLE_MODEL)) {
            this.setSerializableModel(
                    Boolean.valueOf(this.additionalProperties.get(CodegenConstants.SERIALIZABLE_MODEL).toString()));
        }
        if (this.additionalProperties.containsKey(TITLE)) {
            this.setTitle((String) this.additionalProperties.get(TITLE));
        }
        this.additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, serializableModel);
        if (this.additionalProperties.containsKey(FULL_JAVA_UTIL)) {
            this.setFullJavaUtil(Boolean.valueOf(this.additionalProperties.get(FULL_JAVA_UTIL).toString()));
        }

        if (this.additionalProperties.containsKey(EUREKA_URI)) {
            this.setEurekaUri((String) this.additionalProperties.get(EUREKA_URI));
        }
        if (this.additionalProperties.containsKey(ZIPKIN_URI)) {
            this.setZipkinUri((String) this.additionalProperties.get(ZIPKIN_URI));
        }
        if (this.additionalProperties.containsKey(SPRINGADMIN_URI)) {
            this.setSpringBootAdminUri((String) this.additionalProperties.get(SPRINGADMIN_URI));
        }
        if (fullJavaUtil) {
            javaUtilPrefix = "java.util.";
        }
        this.additionalProperties.put(FULL_JAVA_UTIL, fullJavaUtil);
        this.additionalProperties.put("javaUtilPrefix", javaUtilPrefix);
        this.additionalProperties.put(SUPPORT_JAVA6, false);
        this.additionalProperties.put("java8", true);

        if (this.additionalProperties.containsKey(WITH_XML)) {
            this.setWithXml(Boolean.valueOf(additionalProperties.get(WITH_XML).toString()));
        }
        this.additionalProperties.put(WITH_XML, withXml);

        this.apiTemplateFiles.put("api.mustache", ".java");
        this.apiTemplateFiles.put("apiController.mustache", "Controller.java");

        this.modelTemplateFiles.put("model.mustache", ".java");

        this.supportingFiles.add(new SupportingFile("SpringBootApplication.mustache",
                (this.getSourceFolder() + File.separator + this.getBasePackage()).replace(".", File.separator),
                this.getServiceName() + "Application" + ".java"));

        this.supportingFiles
                .add(new SupportingFile("config" + File.separator + "swaggerDocumentationConfig.mustache",
                        (this.sourceFolder + File.separator + this.getConfigPackage()).replace(".",
                                java.io.File.separator) + File.separator + "swagger",
                        "SwaggerDocumentationConfig.java"));

        this.supportingFiles.add(new SupportingFile("config" + File.separator + "pkmstproperties.mustache",
                (this.sourceFolder + File.separator + this.getConfigPackage()).replace(".", java.io.File.separator)
                        + File.separator + "swagger",
                "PkmstProperties.java"));
        this.supportingFiles.add(new SupportingFile("config" + File.separator + "appconfig.mustache",
                (this.sourceFolder + File.separator + this.getConfigPackage()).replace(".", java.io.File.separator)
                        + File.separator,
                "AppConfig.java"));

        // Security
        this.supportingFiles
                .add(new SupportingFile("security" + File.separator + "authorizationServerConfiguration.mustache",
                        (this.sourceFolder + File.separator + this.basePackage).replace(".", File.separator)
                                + File.separator + "security",
                        "AuthorizationServerConfiguration.java"));
        this.supportingFiles
                .add(new SupportingFile("security" + File.separator + "oAuth2SecurityConfiguration.mustache",
                        (this.sourceFolder + File.separator + this.basePackage).replace(".", File.separator)
                                + File.separator + "security",
                        "OAuth2SecurityConfiguration.java"));
        this.supportingFiles
                .add(new SupportingFile("security" + File.separator + "resourceServerConfiguration.mustache",
                        (this.sourceFolder + File.separator + this.basePackage).replace(".", File.separator)
                                + File.separator + "security",
                        "ResourceServerConfiguration.java"));

        // logging

        this.supportingFiles.add(new SupportingFile("logging" + File.separator + "httpLoggingFilter.mustache",
                (this.sourceFolder + File.separator + this.basePackage).replace(".", File.separator) + File.separator
                        + "logging",
                "HttpLoggingFilter.java"));

        // Resources
        this.supportingFiles.add(new SupportingFile("resources" + File.separator + "application-local.mustache",
                ("src.main.resources").replace(".", java.io.File.separator), "application-local.yml"));
        this.supportingFiles.add(new SupportingFile("resources" + File.separator + "application-dev.mustache",
                ("src.main.resources").replace(".", java.io.File.separator), "application-dev.yml"));
        this.supportingFiles.add(new SupportingFile("resources" + File.separator + "application-dev-config.mustache",
                ("src.main.resources").replace(".", java.io.File.separator), "application-dev-config.yml"));
        this.supportingFiles.add(new SupportingFile("resources" + File.separator + "bootstrap.mustache",
                ("src.main.resources").replace(".", java.io.File.separator), "bootstrap.yml"));

        // POM
        this.supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));

        // Readme
        this.supportingFiles.add(new SupportingFile("readme.mustache", "", "Readme.md"));

        // manifest

        this.supportingFiles.add(new SupportingFile("manifest.mustache", "", "manifest.yml"));

        // docker
        this.supportingFiles.add(new SupportingFile("docker.mustache", "", "Dockerfile"));

        // logstash

        this.supportingFiles.add(new SupportingFile("logstash.mustache", "", "logstash.conf"));

        // Cucumber
        this.supportingFiles.add(new SupportingFile("cucumber" + File.separator + "executeReport.mustache",
                this.testFolder + File.separator + this.basePackage.replace(".", File.separator) + File.separator
                        + "cucumber" + File.separator + "report",
                "ExecuteReport.java"));

        this.supportingFiles.add(new SupportingFile(
                "cucumber" + File.separator + "cucumberTest.mustache", this.testFolder + File.separator
                        + this.basePackage.replace(".", File.separator) + File.separator + "cucumber",
                serviceName + "Test.java"));

        this.supportingFiles.add(new SupportingFile(
                "cucumber" + File.separator + "cucumberSteps.mustache", this.testFolder + File.separator
                        + this.basePackage.replace(".", File.separator) + File.separator + "cucumber",
                serviceName + "Steps.java"));

        this.supportingFiles.add(new SupportingFile(
                "cucumber" + File.separator + "package.mustache", this.testFolder + File.separator
                        + this.basePackage.replace(".", File.separator) + File.separator + "cucumber",
                serviceName + "package-info.java"));

        // test resources
        this.supportingFiles.add(new SupportingFile("cucumber" + File.separator + "cucumber.mustache",
                (("src.test.resources") + File.separator + this.basePackage).replace(".", File.separator)
                        + File.separator + "cucumber",
                serviceName + ".feature"));

        this.supportingFiles.add(new SupportingFile("testresources" + File.separator + "bootstrap.mustache",
                ("src.test.resources").replace(".", java.io.File.separator), "bootstrap.yml"));
        this.supportingFiles.add(new SupportingFile("testresources" + File.separator + "application.mustache",
                ("src.test.resources").replace(".", java.io.File.separator), "application.properties"));
        this.supportingFiles.add(new SupportingFile("testresources" + File.separator + "application-test.mustache",
                ("src.test.resources").replace(".", java.io.File.separator), "application-test.properties"));

        // Gatling
        this.supportingFiles.add(new SupportingFile("gatling" + File.separator + "gatling.mustache",
                ("src.test.resources").replace(".", java.io.File.separator), "gatling.conf"));

        this.supportingFiles.add(new SupportingFile("gatling" + File.separator + "application.mustache",
                ("src.test.resources").replace(".", java.io.File.separator), "application.conf"));

        this.supportingFiles.add(new SupportingFile(
                "gatling" + File.separator + "testapi.mustache", ("src") + File.separator + ("test") + File.separator
                        + ("scala") + File.separator + ("scalaFiles").replace(".", java.io.File.separator),
                "testapi.scala"));

        // adding class for integration test
        this.supportingFiles.add(new SupportingFile(
                "integration" + File.separator + "integrationtest.mustache", this.testFolder + File.separator
                        + this.basePackage.replace(".", File.separator) + File.separator + "controller",
                serviceName + "IT.java"));
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (final CodegenOperation operation : ops) {
                List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    for (final CodegenResponse resp : responses) {
                        if ("0".equals(resp.code)) {
                            resp.code = "200";
                        }
                        doDataTypeAssignment(resp.dataType, new DataTypeAssigner() {

                            public void setReturnType(final String returnType) {
                                resp.dataType = returnType;
                            }

                            public void setReturnContainer(final String returnContainer) {
                                resp.containerType = returnContainer;
                            }
                        });
                    }
                }

                doDataTypeAssignment(operation.returnType, new DataTypeAssigner() {

                    public void setReturnType(final String returnType) {
                        operation.returnType = returnType;
                    }

                    public void setReturnContainer(final String returnContainer) {
                        operation.returnContainer = returnContainer;
                    }
                });

                if (implicitHeaders) {
                    removeHeadersFromAllParams(operation.allParams);
                }
            }
        }

        return objs;
    }

    /**
     * This method removes header parameters from the list of parameters and
     * also corrects last allParams hasMore state.
     *
     * @param allParams
     *            list of all parameters
     */
    private void removeHeadersFromAllParams(List<CodegenParameter> allParams) {
        if (allParams.isEmpty()) {
            return;
        }
        final ArrayList<CodegenParameter> copy = new ArrayList<>(allParams);
        allParams.clear();

        for (CodegenParameter p : copy) {
            if (!p.isHeaderParam) {
                allParams.add(p);
            }
        }
        allParams.get(allParams.size() - 1).hasMore = false;
    }

    /**
     * @param returnType
     *            The return type that needs to be converted
     * @param dataTypeAssigner
     *            An object that will assign the data to the respective fields
     *            in the model.
     */
    private void doDataTypeAssignment(String returnType, DataTypeAssigner dataTypeAssigner) {
        final String rt = returnType;
        if (rt == null) {
            dataTypeAssigner.setReturnType("Void");
        } else if (rt.startsWith("List")) {
            int end = rt.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(rt.substring("List<".length(), end).trim());
                dataTypeAssigner.setReturnContainer("List");
            }
        } else if (rt.startsWith("Map")) {
            int end = rt.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(rt.substring("Map<".length(), end).split(",")[1].trim());
                dataTypeAssigner.setReturnContainer("Map");
            }
        } else if (rt.startsWith("Set")) {
            int end = rt.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(rt.substring("Set<".length(), end).trim());
                dataTypeAssigner.setReturnContainer("Set");
            }
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        if ("null".equals(property.example)) {
            property.example = null;
        }

        // Add imports for Jackson
        if (!Boolean.TRUE.equals(model.isEnum)) {
            model.imports.add("JsonProperty");

            if (Boolean.TRUE.equals(model.hasEnums)) {
                model.imports.add("JsonValue");
            }
        } else { // enum class
            // Needed imports for Jackson's JsonCreator
            if (this.additionalProperties.containsKey("jackson")) {
                model.imports.add("JsonCreator");
            }
        }

    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);

        // Add imports for Jackson
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            // for enum model
            if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
                cm.imports.add(this.importMapping.get("JsonValue"));
                Map<String, String> item = new HashMap<String, String>();
                item.put("import", this.importMapping.get("JsonValue"));
                imports.add(item);
            }
        }

        return objs;
    }

    @SuppressWarnings("unchecked")
    @Override
    public void preprocessSwagger(Swagger swagger) {
        super.preprocessSwagger(swagger);
        if (swagger == null || swagger.getPaths() == null) {
            return;
        }
        if (swagger.getTags() != null) {
            List<ResourcePath> resourcePaths = new ArrayList<>();
            for (Tag tag : swagger.getTags()) {
                ResourcePath resourcePath = new ResourcePath();
                resourcePath.setPath(tag.getName());
                resourcePaths.add(resourcePath);
            }
            this.additionalProperties.put("resourcePaths", resourcePaths);
        }
        // get vendor extensions

        Map<String, Object> vendorExt = swagger.getInfo().getVendorExtensions();
        if (vendorExt != null && !vendorExt.toString().equals("")) {
            if (vendorExt.containsKey("x-codegen")) {

                Map<String, String> uris = (Map<String, String>) vendorExt.get("x-codegen");
                if (uris.containsKey("eurekaUri")) {
                    String eurekaUri = uris.get("eurekaUri");
                    additionalProperties.put(EUREKA_URI, eurekaUri);
                }
                if (uris.containsKey("zipkinUri")) {
                    String zipkinUri = uris.get("zipkinUri");
                    additionalProperties.put(ZIPKIN_URI, zipkinUri);
                }
                if (uris.containsKey("springBootAdminUri")) {
                    String springBootAdminUri = uris.get("springBootAdminUri");
                    additionalProperties.put(SPRINGADMIN_URI, springBootAdminUri);
                }
                if (uris.containsKey("pkmstInterceptor")) {
                    String pkmstInterceptor = uris.get("pkmstInterceptor");
                    additionalProperties.put("pkmstInterceptor", pkmstInterceptor);
                }
            }
        }

        for (String pathname : swagger.getPaths().keySet()) {
            Path path = swagger.getPath(pathname);
            if (path.getOperations() == null) {
                continue;
            }
            for (Operation operation : path.getOperations()) {
                boolean hasFormParameters = false;
                for (Parameter parameter : operation.getParameters()) {
                    if (parameter instanceof FormParameter) {
                        hasFormParameters = true;
                    }
                }
                // only add content-Type if its no a GET-Method
                if (path.getGet() != null || !operation.equals(path.getGet())) {
                    String defaultContentType = hasFormParameters ? "application/x-www-form-urlencoded"
                            : "application/json";
                    String contentType = operation.getConsumes() == null || operation.getConsumes().isEmpty()
                            ? defaultContentType : operation.getConsumes().get(0);
                    operation.setVendorExtension("x-contentType", contentType);
                }
                String accepts = getAccept(operation);
                operation.setVendorExtension("x-accepts", accepts);
            }
        }

        if ("/".equals(swagger.getBasePath())) {
            swagger.setBasePath("");
        }

        if (!additionalProperties.containsKey(TITLE)) {
            // From the title, compute a reasonable name for the package and the
            // API
            String title = swagger.getInfo().getTitle();

            // Drop any API suffix
            if (title != null) {
                title = title.trim().replace(" ", "-");
                if (title.toUpperCase().endsWith("API")) {
                    title = title.substring(0, title.length() - 3);
                }

                this.title = camelize(sanitizeName(title), true);
            }
            additionalProperties.put(TITLE, this.title);
        }

        String host = swagger.getHost();
        String port = "8008";
        if (host != null) {
            String[] parts = host.split(":");
            if (parts.length > 1) {
                port = parts[1];
            }
        }

        this.additionalProperties.put("serverPort", port);
        if (swagger.getPaths() != null) {
            for (String pathname : swagger.getPaths().keySet()) {
                Path path = swagger.getPath(pathname);
                if (path.getOperations() != null) {
                    for (Operation operation : path.getOperations()) {
                        if (operation.getTags() != null) {
                            List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
                            for (String tag : operation.getTags()) {
                                Map<String, String> value = new HashMap<String, String>();
                                value.put("tag", tag);
                                value.put("hasMore", "true");
                                tags.add(value);
                            }
                            if (tags.size() > 0) {
                                tags.get(tags.size() - 1).remove("hasMore");
                            }
                            if (operation.getTags().size() > 0) {
                                String tag = operation.getTags().get(0);
                                operation.setTags(Arrays.asList(tag));
                            }
                            operation.setVendorExtension("x-tags", tags);
                        }
                    }
                }
            }
        }
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co,
            Map<String, List<CodegenOperation>> operations) {
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);
        co.subresourceOperation = !co.path.isEmpty();
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        name = sanitizeName(name);
        return camelize(name) + "Api";
    }

    @Override
    public String apiFileFolder() {
        return this.outputFolder + "/" + this.sourceFolder + "/" + apiPackage().replace(".", "/");
    }

    @Override
    public String apiTestFileFolder() {
        return this.outputFolder + "/" + this.testFolder + "/" + apiPackage().replace(".", "/");
    }

    @Override
    public String modelFileFolder() {
        return this.outputFolder + "/" + this.sourceFolder + "/" + modelPackage().replace(".", "/");
    }

    @Override
    public String apiDocFileFolder() {
        return (this.outputFolder + "/" + this.apiDocPath).replace("/", File.separator);
    }

    @Override
    public String modelDocFileFolder() {
        return (this.outputFolder + "/" + this.modelDocPath).replace("/", File.separator);
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public String getArtifactId() {
        return artifactId;
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public String getArtifactVersion() {
        return artifactVersion;
    }

    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public String getProjectFolder() {
        return projectFolder;
    }

    public void setProjectFolder(String projectFolder) {
        this.projectFolder = projectFolder;
    }

    public String getEurekaUri() {
        return eurekaUri;
    }

    public void setEurekaUri(String eurekaUri) {
        this.eurekaUri = eurekaUri;
    }

    public String getZipkinUri() {
        return zipkinUri;
    }

    public void setZipkinUri(String zipkinUri) {
        this.zipkinUri = zipkinUri;
    }

    public String getSpringBootAdminUri() {
        return springBootAdminUri;
    }

    public void setSpringBootAdminUri(String springBootAdminUri) {
        this.springBootAdminUri = springBootAdminUri;
    }

    public String getProjectTestFolder() {
        return projectTestFolder;
    }

    public void setProjectTestFolder(String projectTestFolder) {
        this.projectTestFolder = projectTestFolder;
    }

    public String getSourceFolder() {
        return sourceFolder;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public String getTestFolder() {
        return testFolder;
    }

    public void setTestFolder(String testFolder) {
        this.testFolder = testFolder;
    }

    public String getBasePackage() {
        return basePackage;
    }

    public void setBasePackage(String basePackage) {
        this.basePackage = basePackage;
    }

    public String getServiceName() {
        return serviceName;
    }

    public void setServiceName(String serviceName) {
        this.serviceName = serviceName;
    }

    public String getConfigPackage() {
        return configPackage;
    }

    public void setConfigPackage(String configPackage) {
        this.configPackage = configPackage;
    }

    public boolean isImplicitHeaders() {
        return implicitHeaders;
    }

    public void setImplicitHeaders(boolean implicitHeaders) {
        this.implicitHeaders = implicitHeaders;
    }

    public boolean isSerializeBigDecimalAsString() {
        return serializeBigDecimalAsString;
    }

    public void setSerializeBigDecimalAsString(boolean serializeBigDecimalAsString) {
        this.serializeBigDecimalAsString = serializeBigDecimalAsString;
    }

    public boolean isFullJavaUtil() {
        return fullJavaUtil;
    }

    public void setFullJavaUtil(boolean fullJavaUtil) {
        this.fullJavaUtil = fullJavaUtil;
    }

    public Boolean getSerializableModel() {
        return serializableModel;
    }

    public void setSerializableModel(Boolean serializableModel) {
        this.serializableModel = serializableModel;
    }

    public String getInvokerPackage() {
        return invokerPackage;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public boolean isWithXml() {
        return withXml;
    }

    public void setWithXml(boolean withXml) {
        this.withXml = withXml;
    }

    private interface DataTypeAssigner {

        void setReturnType(String returnType);

        void setReturnContainer(String returnContainer);
    }

    private class ResourcePath {

        private String path;

        public String getPath() {
            return path;
        }

        public void setPath(String path) {
            this.path = path;
        }

        @Override
        public String toString() {
            return this.path;
        }
    }
}
