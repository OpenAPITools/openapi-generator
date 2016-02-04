package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;
import org.apache.commons.lang.StringUtils;

import java.io.File;
import java.util.*;

public class JavaResteasyServerCodegen extends JavaClientCodegen implements CodegenConfig {

    protected String dateLibrary = "default";
    protected String title = "Swagger Server";
    protected String implFolder = "src/main/java";

    public static final String DATE_LIBRARY = "dateLibrary";

    public JavaResteasyServerCodegen() {

        super();

        sourceFolder = "src/gen/java";
        invokerPackage = "io.swagger.api";
        artifactId = "swagger-jaxrs-resteasy-server";

        outputFolder = "generated-code/javaJaxRS";
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        apiTemplateFiles.put("apiService.mustache", ".java");
        apiTemplateFiles.put("apiServiceImpl.mustache", ".java");
        apiTemplateFiles.put("apiServiceFactory.mustache", ".java");
        apiPackage = "io.swagger.api";
        modelPackage = "io.swagger.model";

        additionalProperties.put("title", title);

        embeddedTemplateDir = templateDir = "JavaJaxRS" + File.separator + "resteasy";

        for (int i = 0; i < cliOptions.size(); i++) {
            if (CodegenConstants.LIBRARY.equals(cliOptions.get(i).getOpt())) {
                cliOptions.remove(i);
                break;
            }
        }

        CliOption dateLibrary = new CliOption(DATE_LIBRARY, "Option. Date library to use");
        Map<String, String> dateOptions = new HashMap<String, String>();
        dateOptions.put("java8", "Java 8 native");
        dateOptions.put("joda", "Joda");
        dateLibrary.setEnum(dateOptions);

        cliOptions.add(dateLibrary);

        CliOption library = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        library.setDefault(DEFAULT_LIBRARY);

        Map<String, String> supportedLibraries = new LinkedHashMap<String, String>();

        supportedLibraries.put(DEFAULT_LIBRARY, "Resteasy core 3.0.11");
        library.setEnum(supportedLibraries);

        cliOptions.add(library);
        cliOptions.add(new CliOption(CodegenConstants.IMPL_FOLDER, CodegenConstants.IMPL_FOLDER_DESC));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "jaxrs-resteasy";
    }

    @Override
    public String getHelp() {
        return "Generates a Java JAXRS-Resteasy Server application.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.IMPL_FOLDER)) {
            implFolder = (String) additionalProperties.get(CodegenConstants.IMPL_FOLDER);
        }

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("gradle.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("settingsGradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                (sourceFolder + '/' + apiPackage).replace(".", "/"), "ApiException.java"));
        supportingFiles.add(new SupportingFile("ApiOriginFilter.mustache",
                (sourceFolder + '/' + apiPackage).replace(".", "/"), "ApiOriginFilter.java"));
        supportingFiles.add(new SupportingFile("ApiResponseMessage.mustache",
                (sourceFolder + '/' + apiPackage).replace(".", "/"), "ApiResponseMessage.java"));
        supportingFiles.add(new SupportingFile("NotFoundException.mustache",
                (sourceFolder + '/' + apiPackage).replace(".", "/"), "NotFoundException.java"));
        supportingFiles.add(new SupportingFile("web.mustache",
                ("src/main/webapp/WEB-INF"), "web.xml"));
        supportingFiles.add(new SupportingFile("jboss-web.mustache",
                ("src/main/webapp/WEB-INF"), "jboss-web.xml"));
        supportingFiles.add(new SupportingFile("RestApplication.mustache",
                (sourceFolder + '/' + invokerPackage).replace(".", "/"), "RestApplication.java"));
        supportingFiles.add(new SupportingFile("StringUtil.mustache",
                (sourceFolder + '/' + invokerPackage).replace(".", "/"), "StringUtil.java"));

        if (additionalProperties.containsKey("dateLibrary")) {
            setDateLibrary(additionalProperties.get("dateLibrary").toString());
            additionalProperties.put(dateLibrary, "true");
        }

        if ("joda".equals(dateLibrary)) {
            typeMapping.put("date", "LocalDate");
            typeMapping.put("DateTime", "DateTime");

            importMapping.put("LocalDate", "org.joda.time.LocalDate");
            importMapping.put("DateTime", "org.joda.time.DateTime");

            supportingFiles.add(new SupportingFile("JacksonConfig.mustache",
                    (sourceFolder + '/' + invokerPackage).replace(".", "/"), "JacksonConfig.java"));

            supportingFiles.add(new SupportingFile("JodaDateTimeProvider.mustache",
                    (sourceFolder + '/' + apiPackage).replace(".", "/"), "JodaDateTimeProvider.java"));
            supportingFiles.add(new SupportingFile("JodaLocalDateProvider.mustache",
                    (sourceFolder + '/' + apiPackage).replace(".", "/"), "JodaLocalDateProvider.java"));
        } else if ("java8".equals(dateLibrary)) {
            additionalProperties.put("java8", "true");
            additionalProperties.put("javaVersion", "1.8");
            typeMapping.put("date", "LocalDate");
            typeMapping.put("DateTime", "LocalDateTime");
            importMapping.put("LocalDate", "java.time.LocalDate");
            importMapping.put("LocalDateTime", "java.time.LocalDateTime");

            supportingFiles.add(new SupportingFile("LocalDateTimeProvider.mustache",
                    (sourceFolder + '/' + apiPackage).replace(".", "/"), "LocalDateTimeProvider.java"));
            supportingFiles.add(new SupportingFile("LocalDateProvider.mustache",
                    (sourceFolder + '/' + apiPackage).replace(".", "/"), "LocalDateProvider.java"));
        }
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        String basePath = resourcePath;
        if (basePath.startsWith("/")) {
            basePath = basePath.substring(1);
        }
        int pos = basePath.indexOf("/");
        if (pos > 0) {
            basePath = basePath.substring(0, pos);
        }

        if (basePath == "") {
            basePath = "default";
        } else {
            if (co.path.startsWith("/" + basePath)) {
                co.path = co.path.substring(("/" + basePath).length());
            }
            co.subresourceOperation = !co.path.isEmpty();
        }
        List<CodegenOperation> opList = operations.get(basePath);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(basePath, opList);
        }
        opList.add(co);
        co.baseName = basePath;
    }


    @Override
    public void preprocessSwagger(Swagger swagger) {
        if ("/".equals(swagger.getBasePath())) {
            swagger.setBasePath("");
        }

        String host = swagger.getHost();
        String port = "8080";
        if (host != null) {
            String[] parts = host.split(":");
            if (parts.length > 1) {
                port = parts[1];
            }
        }
        this.additionalProperties.put("serverPort", port);
        if (swagger != null && swagger.getPaths() != null) {
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
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {

        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.hasConsumes == Boolean.TRUE) {
                    Map<String, String> firstType = operation.consumes.get(0);
                    if (firstType != null) {
                        if ("multipart/form-data".equals(firstType.get("mediaType"))) {
                            operation.isMultipart = Boolean.TRUE;
                        }
                    }
                }
                List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    for (CodegenResponse resp : responses) {
                        if ("0".equals(resp.code)) {
                            resp.code = "200";
                        }
                    }
                }
                if (operation.returnType == null) {
                    operation.returnType = "Void";
                } else if (operation.returnType.startsWith("List")) {
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if (end > 0) {
                        operation.returnType = rt.substring("List<".length(), end).trim();
                        operation.returnContainer = "List";
                    }
                } else if (operation.returnType.startsWith("Map")) {
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if (end > 0) {
                        operation.returnType = rt.substring("Map<".length(), end).split(",")[1].trim();
                        operation.returnContainer = "Map";
                    }
                } else if (operation.returnType.startsWith("Set")) {
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if (end > 0) {
                        operation.returnType = rt.substring("Set<".length(), end).trim();
                        operation.returnContainer = "Set";
                    }
                }
            }
        }
        return objs;
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
    public String apiFilename(String templateName, String tag) {

        String result = super.apiFilename(templateName, tag);

        if (templateName.endsWith("Impl.mustache")) {
            int ix = result.lastIndexOf('/');
            result = result.substring(0, ix) + "/impl" + result.substring(ix, result.length() - 5) + "ServiceImpl.java";

            result = result.replace(apiFileFolder(), implFileFolder(implFolder));
        } else if (templateName.endsWith("Factory.mustache")) {
            int ix = result.lastIndexOf('/');
            result = result.substring(0, ix) + "/factories" + result.substring(ix, result.length() - 5) + "ServiceFactory.java";

            result = result.replace(apiFileFolder(), implFileFolder(implFolder));
        } else if (templateName.endsWith("Service.mustache")) {
            int ix = result.lastIndexOf('.');
            result = result.substring(0, ix) + "Service.java";
        }

        return result;
    }


    private String implFileFolder(String output) {
        return outputFolder + "/" + output + "/" + apiPackage().replace('.', '/');
    }

    @Override
    public boolean shouldOverwrite(String filename) {
        return super.shouldOverwrite(filename) && !filename.endsWith("ServiceImpl.java") && !filename.endsWith("ServiceFactory.java");
    }

    public void setDateLibrary(String library) {
        this.dateLibrary = library;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if(serializeBigDecimalAsString) {
            if (property.baseType.equals("BigDecimal")) {
                // we serialize BigDecimal as `string` to avoid precision loss
                property.vendorExtensions.put("extraAnnotation", "@JsonSerialize(using = ToStringSerializer.class)");

                // this requires some more imports to be added for this model...
                model.imports.add("ToStringSerializer");
                model.imports.add("JsonSerialize");
            }
        }

        if(model.isEnum == null || model.isEnum) {

            final String lib = getLibrary();
            if(StringUtils.isEmpty(lib)) {
                model.imports.add("JsonProperty");

                if(model.hasEnums != null || model.hasEnums == true) {
                    model.imports.add("JsonValue");
                }
            }
        }
        return;
    }
}