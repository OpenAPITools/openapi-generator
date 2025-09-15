/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law of or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import lombok.Getter;
import lombok.Setter;
import org.openapitools.codegen.*;

import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.TrimWhitespaceLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;

import org.openapitools.codegen.utils.CamelizeOption;

import static org.openapitools.codegen.utils.StringUtils.camelize;


public class JavaDubboServerCodegen extends AbstractJavaCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(JavaDubboServerCodegen.class);

    public static final String TITLE = "title";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String INTERFACE_ONLY = "interfaceOnly";
    public static final String USE_TAGS = "useTags";
    public static final String DUBBO_VERSION = "dubboVersion";
    public static final String JAVA_VERSION = "javaVersion";
    public static final String SERVICE_INTERFACE = "serviceInterface";
    public static final String SERVICE_IMPLEMENTATION = "serviceImplementation";
    public static final String USE_GENERIC_RESPONSE = "useGenericResponse";
    public static final String REGISTRY_ADDRESS = "registry-address";

    @Setter
    protected String title = null;
    @Getter
    @Setter
    protected String configPackage = "org.openapitools.configuration";
    @Getter
    @Setter
    protected String basePackage = "org.openapitools";

    @Setter
    protected boolean interfaceOnly = false;
    @Setter
    protected boolean useTags = true;
    @Setter
    protected String javaVersion = "17";
    @Setter
    protected String dubboVersion = "3.2.18";
    @Setter
    protected boolean serviceInterface = true;
    @Setter
    protected boolean serviceImplementation = true;
    @Setter
    protected boolean useGenericResponse = false;
    @Setter
    protected String registryAddress = "zookeeper://127.0.0.1:2181";

    public JavaDubboServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(SecurityFeature.OAuth2_Implicit, SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_ClientCredentials, SecurityFeature.OAuth2_Password,
                        SecurityFeature.ApiKey, SecurityFeature.BasicAuth))
                .excludeGlobalFeatures(GlobalFeature.Callbacks, GlobalFeature.LinkObjects, GlobalFeature.ParameterStyling)
                .includeSchemaSupportFeatures(SchemaSupportFeature.Polymorphism)
                .excludeParameterFeatures(ParameterFeature.Cookie));

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        dateLibrary = "java8";

        useBeanValidation = false;
        serializableModel = true;
        outputFolder = "generated-code/java-dubbo";
        embeddedTemplateDir = templateDir = "java-dubbo";
        invokerPackage = "org.openapitools";
        apiPackage = invokerPackage + ".api";
        modelPackage = invokerPackage + ".model";
        artifactId = "openapi-dubbo";

        additionalProperties.put("hideGenerationTimestamp", true);
        additionalProperties.put("withXml", false);

        importMapping.remove("ApiModel");
        importMapping.remove("ApiModelProperty");

        setDocumentationProvider(DocumentationProvider.NONE);
        setAnnotationLibrary(AnnotationLibrary.NONE);

        supportsInheritance = false;
        supportsMixins = false;

        importMapping.remove("ApiModel");
        importMapping.remove("ApiModelProperty");
        importMapping.remove("Schema");
        importMapping.remove("io.swagger.annotations.ApiModel");
        importMapping.remove("io.swagger.annotations.ApiModelProperty");
        importMapping.remove("io.swagger.v3.oas.annotations.media.Schema");

        typeMapping.clear();
        typeMapping.put("integer", "Integer");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Double");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("object", "Object");
        typeMapping.put("date", "LocalDate");
        typeMapping.put("DateTime", "OffsetDateTime");
        typeMapping.put("date-time", "OffsetDateTime");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("decimal", "BigDecimal");
        typeMapping.put("binary", "byte[]");
        typeMapping.put("file", "org.springframework.web.multipart.MultipartFile");
        typeMapping.put("uuid", "UUID");
        typeMapping.put("byte", "byte[]");
        typeMapping.put("ByteArray", "byte[]");
        typeMapping.put("binary", "byte[]");
        typeMapping.put("password", "String");

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("String");
        languageSpecificPrimitives.add("boolean");
        languageSpecificPrimitives.add("Boolean");
        languageSpecificPrimitives.add("Double");
        languageSpecificPrimitives.add("Integer");
        languageSpecificPrimitives.add("Long");
        languageSpecificPrimitives.add("Float");
        languageSpecificPrimitives.add("Object");
        languageSpecificPrimitives.add("List");
        languageSpecificPrimitives.add("Map");
        languageSpecificPrimitives.add("Set");

        importMapping.put("List", "java.util.List");
        importMapping.put("Map", "java.util.Map");
        importMapping.put("Set", "java.util.Set");
        importMapping.put("ArrayList", "java.util.ArrayList");
        importMapping.put("HashMap", "java.util.HashMap");
        importMapping.put("HashSet", "java.util.HashSet");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Arrays", "java.util.Arrays");
        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("BigInteger", "java.math.BigInteger");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("MultipartFile", "org.springframework.web.multipart.MultipartFile");

        typeMapping.put("List", "List");
        typeMapping.put("Set", "Set");
        typeMapping.put("Map", "Map");

        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        apiTestTemplateFiles.clear();

        apiTemplateFiles.clear();
        apiTemplateFiles.put("api.mustache", ".java");
        apiTemplateFiles.put("apiDubbo.mustache", ".java");
        apiTemplateFiles.put("apiController.mustache", ".java");
        apiDocTemplateFiles.clear();

        modelTemplateFiles.clear();
        modelTemplateFiles.put("model.mustache", ".java");
        modelDocTemplateFiles.clear();

        additionalProperties.put("dubboVersion", dubboVersion);
        additionalProperties.put("javaVersion", javaVersion);

        cliOptions.add(new CliOption(TITLE, "API title name").defaultValue(title));
        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code")
                .defaultValue(this.getConfigPackage()));
        cliOptions.add(new CliOption(BASE_PACKAGE, "base package (invokerPackage) for generated code")
                .defaultValue(this.getBasePackage()));
        cliOptions.add(CliOption.newBoolean(INTERFACE_ONLY,
                "Whether to generate only API interface stubs without the server files.", interfaceOnly));
        cliOptions.add(CliOption.newBoolean(USE_TAGS, "use tags for creating interface and controller classnames", useTags));
        cliOptions.add(new CliOption(DUBBO_VERSION, "Dubbo version").defaultValue(dubboVersion));
        cliOptions.add(new CliOption(JAVA_VERSION, "Java version").defaultValue(javaVersion));
        cliOptions.add(CliOption.newBoolean(SERVICE_INTERFACE, "Generate service interface", serviceInterface));
        cliOptions.add(CliOption.newBoolean(SERVICE_IMPLEMENTATION, "Generate service implementation", serviceImplementation));
        cliOptions.add(CliOption.newBoolean(USE_GENERIC_RESPONSE, "Use generic response wrapper", useGenericResponse));
        cliOptions.add(new CliOption(REGISTRY_ADDRESS, "Registry address (e.g., zookeeper://127.0.0.1:2181 or nacos://127.0.0.1:8848)")
                .defaultValue(registryAddress));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "java-dubbo";
    }

    @Override
    public String getHelp() {
        return "Generates a Java Apache Dubbo server application.";
    }

    private boolean isOutputFolderPointingToSourceDirectory() {
        if (outputFolder == null) {
            return false;
        }

        String normalizedPath = outputFolder.replace('\\', '/');
        return normalizedPath.endsWith("/src/main/java") || normalizedPath.endsWith("src/main/java");
    }

    @Override
    public String outputFolder() {
        if (isOutputFolderPointingToSourceDirectory()) {
            StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();

            for (StackTraceElement element : stackTrace) {
                String methodName = element.getMethodName();
                String className = element.getClassName();

                if (className.equals("org.openapitools.codegen.DefaultGenerator")) {
                    if (methodName.equals("generateVersionMetadata") ||
                            methodName.equals("generateFilesMetadata")) {

                        return outputFolder + File.separator + ".." + File.separator + ".." + File.separator + "..";
                    }
                }
            }
        }

        return outputFolder;
    }

    public String getIgnoreFileOutputPath() {
        if (isOutputFolderPointingToSourceDirectory()) {
            return outputFolder + File.separator + ".." + File.separator + ".." + File.separator + "..";
        }
        return outputFolder;
    }

    @Override
    public void processOpts() {
        final List<org.apache.commons.lang3.tuple.Pair<String, String>> configOptions =
                additionalProperties.entrySet().stream()
                        .filter(e -> !Arrays.asList("hideGenerationTimestamp").contains(e.getKey()))
                        .filter(e -> cliOptions.stream().map(CliOption::getOpt).anyMatch(opt -> opt.equals(e.getKey())))
                        .map(e -> org.apache.commons.lang3.tuple.Pair.of(e.getKey(), e.getValue().toString()))
                        .collect(Collectors.toList());
        additionalProperties.put("configOptions", configOptions);

        super.processOpts();

        supportingFiles.removeIf(sf -> sf.getDestinationFilename().equals(".openapi-generator-ignore"));

        openapiGeneratorIgnoreList = new HashSet<>();
        openapiGeneratorIgnoreList.add("# Dubbo generator explicitly disables .openapi-generator-ignore");

        importMapping.remove("ApiModel");
        importMapping.remove("ApiModelProperty");
        importMapping.remove("io.swagger.annotations.ApiModel");
        importMapping.remove("io.swagger.annotations.ApiModelProperty");
        importMapping.remove("Schema");
        importMapping.remove("io.swagger.v3.oas.annotations.media.Schema");

        String userTitle = (String) additionalProperties.get(TITLE);
        boolean userProvidedTitle = userTitle != null && !userTitle.equals("OpenAPI Dubbo");

        if (userProvidedTitle) {
            this.title = userTitle;
        }

        additionalProperties.put("userProvidedTitle", userProvidedTitle);

        additionalProperties.put("title", this.title);

        if (additionalProperties.containsKey(CONFIG_PACKAGE)) {
            this.setConfigPackage((String) additionalProperties.get(CONFIG_PACKAGE));
        }

        if (additionalProperties.containsKey(BASE_PACKAGE)) {
            String basePackageName = (String) additionalProperties.get(BASE_PACKAGE);
            this.setBasePackage(basePackageName);
            this.setInvokerPackage(basePackageName);

            this.apiPackage = basePackageName + ".api";
            this.modelPackage = basePackageName + ".model";

            if (!additionalProperties.containsKey(CONFIG_PACKAGE)) {
                this.configPackage = basePackageName + ".configuration";
            }

            updateOption(CodegenConstants.API_PACKAGE, this.apiPackage);
            updateOption(CodegenConstants.MODEL_PACKAGE, this.modelPackage);
            updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        }

        if (additionalProperties.containsKey(INTERFACE_ONLY)) {
            this.interfaceOnly = Boolean.parseBoolean(additionalProperties.get(INTERFACE_ONLY).toString());
        }

        if (additionalProperties.containsKey(USE_TAGS)) {
            this.useTags = Boolean.parseBoolean(additionalProperties.get(USE_TAGS).toString());
        }

        if (additionalProperties.containsKey(DUBBO_VERSION)) {
            this.dubboVersion = (String) additionalProperties.get(DUBBO_VERSION);
        }

        if (additionalProperties.containsKey(JAVA_VERSION)) {
            this.javaVersion = (String) additionalProperties.get(JAVA_VERSION);
        }

        if (additionalProperties.containsKey(SERVICE_INTERFACE)) {
            this.serviceInterface = Boolean.parseBoolean(additionalProperties.get(SERVICE_INTERFACE).toString());
        }

        if (additionalProperties.containsKey(SERVICE_IMPLEMENTATION)) {
            this.serviceImplementation = Boolean.parseBoolean(additionalProperties.get(SERVICE_IMPLEMENTATION).toString());
        }

        if (additionalProperties.containsKey(USE_GENERIC_RESPONSE)) {
            this.useGenericResponse = Boolean.parseBoolean(additionalProperties.get(USE_GENERIC_RESPONSE).toString());
        }

        if (additionalProperties.containsKey(REGISTRY_ADDRESS)) {
            this.registryAddress = (String) additionalProperties.get(REGISTRY_ADDRESS);
        }

        additionalProperties.put("title", this.title);
        additionalProperties.put("configPackage", this.configPackage);
        additionalProperties.put("basePackage", this.basePackage);
        additionalProperties.put("apiPackage", this.apiPackage);
        additionalProperties.put("modelPackage", this.modelPackage);
        additionalProperties.put("invokerPackage", this.getInvokerPackage());
        additionalProperties.put("interfaceOnly", interfaceOnly);
        additionalProperties.put("useTags", useTags);
        additionalProperties.put("dubboVersion", dubboVersion);
        additionalProperties.put("javaVersion", javaVersion);
        additionalProperties.put("serviceInterface", serviceInterface);
        additionalProperties.put("serviceImplementation", serviceImplementation);
        additionalProperties.put("useGenericResponse", useGenericResponse);
        additionalProperties.put("registryAddress", registryAddress);

        supportingFiles.clear();

        if (isOutputFolderPointingToSourceDirectory()) {
            String rootPath = ".." + File.separator + ".." + File.separator + "..";
            supportingFiles.add(new SupportingFile("pom.mustache", rootPath, "pom.xml"));
            supportingFiles.add(new SupportingFile("README.mustache", rootPath, "README.md"));

        } else {
            supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
            supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        }

        if (serviceImplementation && !interfaceOnly) {
            String mainClassName;
            boolean isUserTitle = (Boolean) additionalProperties.getOrDefault("userProvidedTitle", false);

            if (isUserTitle) {
                String titleName = (String) additionalProperties.get(TITLE);
                mainClassName = (camelize(titleName.trim(), CamelizeOption.UPPERCASE_FIRST_CHAR) + "Application").replaceAll("\\s+", "");
                ;
            } else {
                mainClassName = "OpenApiGeneratorApplication";
            }

            String testClassName = mainClassName + "Tests";
            additionalProperties.put("mainClassName", mainClassName);
            additionalProperties.put("testClassName", testClassName);

            if (isOutputFolderPointingToSourceDirectory()) {
                supportingFiles.add(new SupportingFile("mainApplication.mustache",
                        basePackage.replace(".", File.separator),
                        mainClassName + ".java"));

                supportingFiles.add(new SupportingFile("applicationTest.mustache",
                        ".." + File.separator + ".." + File.separator + ".." + File.separator + "src" + File.separator + "test" + File.separator + "java" + File.separator + basePackage.replace(".", File.separator),
                        testClassName + ".java"));

                supportingFiles.add(new SupportingFile("application.mustache",
                        ".." + File.separator + ".." + File.separator + ".." + File.separator + "src" + File.separator + "main" + File.separator + "resources",
                        "application.yml"));
            } else {
                supportingFiles.add(new SupportingFile("mainApplication.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", File.separator),
                        mainClassName + ".java"));

                supportingFiles.add(new SupportingFile("applicationTest.mustache",
                        (testFolder + File.separator + basePackage).replace(".", File.separator),
                        testClassName + ".java"));

                supportingFiles.add(new SupportingFile("application.mustache",
                        "src/main/resources".replace("/", File.separator),
                        "application.yml"));
            }
        }

        additionalProperties.put("lambdaTrimWhitespace", new TrimWhitespaceLambda());

        additionalProperties.put("documentationProvider", "none");
        additionalProperties.put("annotationLibrary", "none");
        additionalProperties.put("hideGenerationTimestamp", true);
        additionalProperties.put("useBeanValidation", false);
        additionalProperties.put("performBeanValidation", false);

    }

    private boolean isDubbo33OrHigher(String version) {
        if (version == null || version.trim().isEmpty()) {
            return false;
        }

        try {
            String cleanVersion = version.split("-")[0];
            String[] parts = cleanVersion.split("\\.");
            if (parts.length >= 2) {
                int major = Integer.parseInt(parts[0]);
                int minor = Integer.parseInt(parts[1]);

                return major > 3 || (major == 3 && minor >= 3);
            }
        } catch (NumberFormatException e) {
            LOGGER.warn("Unable to parse Dubbo version: " + version);
        }

        return false;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        objs = super.postProcessSupportingFileData(objs);

        objs.put("interfacePackage", apiPackage + ".interfaces");
        objs.put("consumerPackage", apiPackage + ".consumer");
        objs.put("providerPackage", apiPackage + ".provider");

        objs.put("registryAddress", registryAddress);

        boolean isDubbo33Plus = isDubbo33OrHigher(dubboVersion);
        objs.put("isDubbo33Plus", isDubbo33Plus);

        boolean isZookeeperRegistry = registryAddress != null && registryAddress.startsWith("zookeeper://");
        boolean isNacosRegistry = registryAddress != null && registryAddress.startsWith("nacos://");
        objs.put("isZookeeperRegistry", isZookeeperRegistry);
        objs.put("isNacosRegistry", isNacosRegistry);

        String nacosClientVersion = isDubbo33Plus ? "2.5.0" : "2.2.4";
        objs.put("nacosClientVersion", nacosClientVersion);

        return objs;
    }


    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationsMap results = super.postProcessOperationsWithModels(objs, allModels);

        if (results.getImports() != null) {
            boolean hasJavaUtilImports = false;
            Iterator<Map<String, String>> importIterator = results.getImports().iterator();
            while (importIterator.hasNext()) {
                Map<String, String> importMap = importIterator.next();
                String importName = importMap.get("import");
                if (importName != null && importName.startsWith("java.util.") &&
                        !importName.equals("java.util.*") &&
                        !importName.equals("java.util.UUID")) {
                    importIterator.remove();
                    hasJavaUtilImports = true;
                }
            }

            if (hasJavaUtilImports) {
                Map<String, String> utilImport = new HashMap<>();
                utilImport.put("import", "java.util.*");
                utilImport.put("classname", "*");
                results.getImports().add(utilImport);
            }
        }

        OperationMap operations = results.getOperations();
        if (operations != null) {
            String baseName = (String) operations.get("baseName");
            String originalClassName = (String) operations.get("classname");

            if (baseName == null) {
                baseName = originalClassName;
                if (baseName != null && baseName.endsWith("Service")) {
                    baseName = baseName.substring(0, baseName.length() - 7).toLowerCase(java.util.Locale.ROOT);
                } else if (baseName != null && baseName.endsWith("Api")) {
                    baseName = baseName.substring(0, baseName.length() - 3).toLowerCase(java.util.Locale.ROOT);
                }
            }

            if (baseName != null) {
                if ("ashares".equals(baseName)) {
                    operations.put("baseName", "a");
                } else if ("usdata".equals(baseName)) {
                    operations.put("baseName", "us");
                } else if ("kline".equals(baseName)) {
                    operations.put("baseName", "kline");
                }
            }

            if (originalClassName != null) {
                String serviceName = originalClassName;
                if (serviceName.endsWith("Api")) {
                    serviceName = serviceName.replace("Api", "Service");
                } else if (serviceName.endsWith("Service")) {
                    serviceName = originalClassName;
                }
                operations.put("serviceName", serviceName);
                operations.put("serviceVarName", camelize(serviceName, LOWERCASE_FIRST_LETTER));

                operations.put("servicePackage", apiPackage + ".interfaces");
                operations.put("serviceImport", apiPackage + ".interfaces." + serviceName);
                operations.put("interfacePackage", apiPackage + ".interfaces");
                operations.put("consumerPackage", apiPackage + ".consumer");
                operations.put("providerPackage", apiPackage + ".provider");
            }

            List<CodegenOperation> ops = operations.getOperation();
            for (CodegenOperation operation : ops) {
                if (useGenericResponse) {
                    operation.vendorExtensions.put("x-generic-response", true);
                }
            }
        }

        results.put("interfacePackage", apiPackage + ".interfaces");
        results.put("consumerPackage", apiPackage + ".consumer");
        results.put("providerPackage", apiPackage + ".provider");

        return results;
    }

    @Override
    public String apiFileFolder() {
        if (isOutputFolderPointingToSourceDirectory()) {
            return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
        } else {
            return outputFolder + File.separator + "src" + File.separator + "main" + File.separator + "java" + File.separator + apiPackage().replace('.', File.separatorChar);
        }
    }

    public String getFileFolderForTemplate(String templateName) {
        String baseFolder;
        if (isOutputFolderPointingToSourceDirectory()) {
            baseFolder = outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
        } else {
            baseFolder = outputFolder + File.separator + "src" + File.separator + "main" + File.separator + "java" + File.separator + apiPackage().replace('.', File.separatorChar);
        }

        if ("api.mustache".equals(templateName)) {
            return baseFolder + File.separator + "interfaces";
        } else if ("apiController.mustache".equals(templateName)) {
            return baseFolder + File.separator + "consumer";
        } else if ("apiDubbo.mustache".equals(templateName)) {
            return baseFolder + File.separator + "provider";
        }

        return baseFolder;
    }

    public String getPackageForTemplate(String templateName) {
        if ("api.mustache".equals(templateName)) {
            return apiPackage() + ".interfaces";
        } else if ("apiController.mustache".equals(templateName)) {
            return apiPackage() + ".consumer";
        } else if ("apiDubbo.mustache".equals(templateName)) {
            return apiPackage() + ".provider";
        }
        return apiPackage();
    }

    @Override
    public String modelFileFolder() {
        if (isOutputFolderPointingToSourceDirectory()) {
            return outputFolder + File.separator + modelPackage().replace('.', File.separatorChar);
        } else {
            return outputFolder + File.separator + "src" + File.separator + "main" + File.separator + "java" + File.separator + modelPackage().replace('.', File.separatorChar);
        }
    }


    @Override
    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        if (suffix == null) {
            return null;
        }

        String folder = getFileFolderForTemplate(templateName);
        String filename;

        String apiName = toApiName(tag);

        if ("api.mustache".equals(templateName)) {
            filename = apiName;
        } else if ("apiDubbo.mustache".equals(templateName)) {
            filename = apiName + "Impl";
        } else if ("apiController.mustache".equals(templateName)) {
            filename = apiName + "Controller";
        } else {
            filename = toApiFilename(tag);
        }

        return folder + File.separator + filename + suffix;
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultService";
        }
        name = sanitizeName(name);
        String apiName = camelize(name, CamelizeOption.UPPERCASE_FIRST_CHAR);

        if (!apiName.endsWith("Service")) {
            apiName = apiName + "Service";
        }

        return apiName;
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        if (useTags) {
            String basePath = resourcePath;
            if (basePath.startsWith("/")) {
                basePath = basePath.substring(1);
            }
            int pos = basePath.indexOf("/");
            if (pos > 0) {
                basePath = basePath.substring(0, pos);
            }

            if (basePath.length() == 0) {
                basePath = "default";
            } else {
                String subPath = resourcePath;
                if (subPath.startsWith("/" + basePath)) {
                    subPath = subPath.substring(("/" + basePath).length());
                }
                if (subPath.isEmpty()) {
                    subPath = "/";
                }
                co.vendorExtensions.put("x-sub-path", subPath);
                co.subresourceOperation = !subPath.equals("/");

                co.vendorExtensions.put("x-base-path", "/" + basePath);

                if ("a".equals(basePath)) {
                    basePath = "ashares";
                } else if ("us".equals(basePath)) {
                    basePath = "usdata";
                }
            }
            List<CodegenOperation> opList = operations.computeIfAbsent(basePath, k -> new ArrayList<>());
            opList.add(co);
            co.baseName = basePath;
        } else {
            super.addOperationToGroup(tag, resourcePath, operation, co, operations);
        }
    }

    @Override
    public boolean isDataTypeString(String dataType) {
        return "String".equals(dataType);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            Schema inner = ModelUtils.getSchemaItems(p);
            String innerType = getTypeDeclaration(inner);
            return "List<" + innerType + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            String innerType = getTypeDeclaration(inner);
            return "Map<String, " + innerType + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }
        return openAPIType;
    }


    @Override
    public Set<String> defaultIncludes() {
        return new HashSet<String>(
                Arrays.asList(
                        "double",
                        "int",
                        "long",
                        "short",
                        "char",
                        "float",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Void",
                        "Integer",
                        "Long",
                        "Float")
        );
    }

    @Override
    public String toModelImport(String name) {
        if ("ApiModel".equals(name) || "ApiModelProperty".equals(name)) {
            return null;
        }
        if ("".equals(modelPackage())) {
            return name;
        } else {
            return modelPackage() + "." + name;
        }
    }

    @Override
    public String getTypeDeclaration(String name) {
        if ("ApiModel".equals(name) || "ApiModelProperty".equals(name)) {
            return null;
        }
        return super.getTypeDeclaration(name);
    }

    @Override
    public boolean needToImport(String type) {
        if ("ApiModel".equals(type) ||
                "ApiModelProperty".equals(type) ||
                "io.swagger.annotations.ApiModel".equals(type) ||
                "io.swagger.annotations.ApiModelProperty".equals(type) ||
                "Schema".equals(type) ||
                "io.swagger.v3.oas.annotations.media.Schema".equals(type)) {
            return false;
        }

        if ("LocalDate".equals(type) ||
                "LocalDateTime".equals(type) ||
                "LocalTime".equals(type) ||
                "OffsetDateTime".equals(type) ||
                "BigDecimal".equals(type) ||
                "BigInteger".equals(type) ||
                "UUID".equals(type)) {
            return true;
        }

        return super.needToImport(type);
    }


    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);

        for (ModelMap modelMap : objs.getModels()) {
            CodegenModel model = modelMap.getModel();
            model.imports.remove("ApiModel");
            model.imports.remove("ApiModelProperty");
            model.imports.remove("io.swagger.annotations.ApiModel");
            model.imports.remove("io.swagger.annotations.ApiModelProperty");
            model.imports.remove("Schema");
            model.imports.remove("io.swagger.v3.oas.annotations.media.Schema");

            boolean hasJavaUtilImports = false;
            Set<String> javaUtilImports = new HashSet<>();
            Iterator<String> iterator = model.imports.iterator();
            while (iterator.hasNext()) {
                String importName = iterator.next();
                if (importName.startsWith("java.util.") &&
                        !importName.equals("java.util.*") &&
                        !importName.equals("java.util.UUID")) {
                    javaUtilImports.add(importName);
                    iterator.remove();
                    hasJavaUtilImports = true;
                }
            }

            if (hasJavaUtilImports) {
                model.imports.add("java.util.*");
            }

            for (CodegenProperty var : model.vars) {
                if (var.allowableValues != null && var.allowableValues.get("imports") != null) {
                    ((Set<String>) var.allowableValues.get("imports")).remove("ApiModel");
                    ((Set<String>) var.allowableValues.get("imports")).remove("ApiModelProperty");
                    ((Set<String>) var.allowableValues.get("imports")).remove("io.swagger.annotations.ApiModel");
                    ((Set<String>) var.allowableValues.get("imports")).remove("io.swagger.annotations.ApiModelProperty");
                }
            }
        }

        if (objs.getImports() != null) {
            objs.getImports().removeIf(importMap -> {
                String className = importMap.get("classname");
                return "ApiModel".equals(className) || "ApiModelProperty".equals(className);
            });

            boolean hasJavaUtilImports = false;
            Iterator<Map<String, String>> importIterator = objs.getImports().iterator();
            while (importIterator.hasNext()) {
                Map<String, String> importMap = importIterator.next();
                String importName = importMap.get("import");
                if (importName != null && importName.startsWith("java.util.") &&
                        !importName.equals("java.util.*") &&
                        !importName.equals("java.util.UUID")) {
                    importIterator.remove();
                    hasJavaUtilImports = true;
                }
            }

            if (hasJavaUtilImports) {
                Map<String, String> utilImport = new HashMap<>();
                utilImport.put("import", "java.util.*");
                utilImport.put("classname", "*");
                objs.getImports().add(utilImport);
            }
        }

        return objs;
    }

    @Override
    public String toModelDocFilename(String name) {
        return null;
    }

    @Override
    public String toApiDocFilename(String name) {
        return null;
    }
}