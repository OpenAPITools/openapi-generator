/*
 * Copyright 2022 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2022 Oracle and/or its affiliates
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

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class JavaHelidonServerCodegen extends JavaHelidonCommonCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(JavaHelidonServerCodegen.class);

    public static final String USE_ABSTRACT_CLASS = "useAbstractClass";
    public static final String GRADLE_PROJECT = "gradleProject";

    protected boolean useBeanValidation = true;
    protected String implFolder = "src/main/java";
    protected String serializationLibrary = null;

    private boolean useAbstractClass = false;
    private boolean gradleProject = false;

    public JavaHelidonServerCodegen() {
        super();
        // beta for now
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        outputFolder = "generated-code" + File.separator + "java";
        embeddedTemplateDir = templateDir = "java-helidon" + File.separator + "server";
        invokerPackage = "org.openapitools.server";
        artifactId = "openapi-java-server";
        apiPackage = invokerPackage + ".api";
        modelPackage = invokerPackage + ".model";
        sourceFolder = "src" + File.separator + "main" + File.separator + "java";

        // clioOptions default redefinition need to be updated
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        modelTestTemplateFiles.put("model_test.mustache", ".java");

        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use Bean Validation"));
        cliOptions.add(CliOption.newBoolean(PERFORM_BEANVALIDATION, "Perform BeanValidation"));
        cliOptions.add(CliOption.newBoolean(USE_ABSTRACT_CLASS,
                "Whether to generate abstract classes for REST API instead of interfaces.", useAbstractClass));
        cliOptions.add(CliOption.newBoolean(GRADLE_PROJECT,
                "Whether to generate gradle project instead of maven.", gradleProject));

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        // clear test templates
        // as this codegen does not support api tests at the moment
        apiTestTemplateFiles.clear();

        supportedLibraries.put(HELIDON_MP, "Helidon MP Server");
        supportedLibraries.put(HELIDON_SE, "Helidon SE Server");
        supportedLibraries.put(HELIDON_NIMA, "Helidon NIMA Server");
        supportedLibraries.put(HELIDON_NIMA_ANNOTATIONS, "Helidon NIMA Annotations Server");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        libraryOption.setDefault(HELIDON_SE);
        cliOptions.add(libraryOption);
        setLibrary(HELIDON_SE);

        CliOption serializationLibrary = new CliOption(CodegenConstants.SERIALIZATION_LIBRARY,
                "Serialization library, defaults to Jackson");
        Map<String, String> serializationOptions = new HashMap<>();
        serializationOptions.put(SERIALIZATION_LIBRARY_JACKSON, "Use Jackson as serialization library");
        serializationOptions.put(SERIALIZATION_LIBRARY_JSONB, "Use JSON-B as serialization library");
        serializationLibrary.setEnum(serializationOptions);
        cliOptions.add(serializationLibrary);
        setSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);

        removeUnusedOptions();

        this.setLegacyDiscriminatorBehavior(false);
    }

    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.clear();
        dateLibrary = "java8";

        addApiTemplateFiles();
        SupportingFile pomFile = new SupportingFile("pom.mustache", "", "pom.xml");
        SupportingFile readmeFile = new SupportingFile("README.mustache", "", "README.md");
        SupportingFile openApiFile = new SupportingFile("openapi.mustache",
                ("src/main/resources/META-INF").replace("/", File.separator), "openapi.yml");
        SupportingFile logFile = new SupportingFile("logging.mustache",
                ("src.main.resources").replace(".", File.separator), "logging.properties");
        SupportingFile packageInfoFile = new SupportingFile("package-info.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator),
                "package-info.java");
        List<SupportingFile> modifiable = new ArrayList<>();
        modifiable.add(pomFile);
        modifiable.add(readmeFile);
        modifiable.add(logFile);
        modifiable.add(packageInfoFile);
        List<SupportingFile> unmodifiable = new ArrayList<>();
        unmodifiable.add(openApiFile);

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        importMapping.put("ObjectMapper", "com.fasterxml.jackson.databind.ObjectMapper");
        importMapping.put("Jsonb", rootJavaEEPackage() + ".json.bind.Jsonb");
        importMapping.put("JsonbBuilder", rootJavaEEPackage() + ".json.bind.JsonbBuilder");

        if (additionalProperties.containsKey(USE_ABSTRACT_CLASS)) {
            useAbstractClass = Boolean.parseBoolean(additionalProperties.get(USE_ABSTRACT_CLASS).toString());
        }
        if (!useAbstractClass) {
            additionalProperties.remove(USE_ABSTRACT_CLASS);
        }

        if (additionalProperties.containsKey(GRADLE_PROJECT)) {
            gradleProject = Boolean.parseBoolean(additionalProperties.get(GRADLE_PROJECT).toString());
        }
        if (!gradleProject) {
            additionalProperties.remove(GRADLE_PROJECT);
        } else {
            modifiable.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
            modifiable.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
            modifiable.remove(pomFile);
        }

        if (additionalProperties.containsKey(CodegenConstants.SERIALIZATION_LIBRARY)) {
            setSerializationLibrary(additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY).toString());
        }

        String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");

        if (additionalProperties.containsKey("jsr310") && isLibrary(HELIDON_MP)) {
            supportingFiles.add(new SupportingFile("JavaTimeFormatter.mustache", invokerFolder, "JavaTimeFormatter.java"));
        }

        if (isLibrary(HELIDON_MP)) {
            String resourceFolder = "src" + File.separator + "main" + File.separator + "resources";
            String metaInfFolder = resourceFolder + File.separator + "META-INF";
            supportingFiles.add(new SupportingFile("RestApplication.mustache", invokerFolder, "RestApplication.java"));
            supportingFiles.add(new SupportingFile("microprofile-config.properties.mustache", metaInfFolder, "microprofile" +
                    "-config.properties"));
            supportingFiles.add(new SupportingFile("beans.xml.mustache", metaInfFolder, "beans.xml"));
            processSupportingFiles(modifiable, unmodifiable);
        } else if (isLibrary(HELIDON_SE)) {
            artifactId = "openapi-helidon-se-server";

            modifiable.add(new SupportingFile("application.mustache",
                    ("src.main.resources").replace(".", java.io.File.separator), "application.yaml"));
            modifiable.add(new SupportingFile("mainTest.mustache",
                    (testFolder + File.separator + invokerPackage).replace(".", java.io.File.separator),
                    "MainTest.java"));
            modifiable.add(new SupportingFile("main.mustache",
                    (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator),
                    "Main.java"));
            unmodifiable.add(new SupportingFile("validatorUtils.mustache",
                    (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator),
                    "ValidatorUtils.java"));
            if (useAbstractClass) {
                importMapping.put("Map", "java.util.Map");
                importMapping.put("HashMap", "java.util.HashMap");
                importMapping.put("InputStream", "java.io.InputStream");
                importMapping.put("ReadableBodyPart", "io.helidon.media.multipart.ReadableBodyPart");
                importMapping.put("ArrayList", "java.util.ArrayList");
                importMapping.put("ByteArrayOutputStream", "java.io.ByteArrayOutputStream");
                importMapping.put("DataChunk", "io.helidon.common.http.DataChunk");
                importMapping.put("UncheckedIOException", "java.io.UncheckedIOException");
                importMapping.put("IOException", "java.io.IOException");
                importMapping.put("ByteArrayInputStream", "java.io.ByteArrayInputStream");
            }
            importMapping.put("Handler", "io.helidon.webserver.Handler");
            processSupportingFiles(modifiable, unmodifiable);
        } else if (isLibrary(HELIDON_NIMA)) {
            throw new UnsupportedOperationException("Not implemented");
        } else if (isLibrary(HELIDON_NIMA_ANNOTATIONS)) {
            throw new UnsupportedOperationException("Not implemented");
        } else {
            LOGGER.error("Unknown library option (-l/--library): {}", getLibrary());
            throw new IllegalArgumentException(
                    String.format(Locale.ROOT,
                            "Unknown library option %s for Helidon Server",
                            getLibrary()
                    )
            );
        }

        if (getSerializationLibrary() == null) {
            LOGGER.info("No serializationLibrary configured, using '{}' as fallback", SERIALIZATION_LIBRARY_JACKSON);
            setSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        }
        switch (getSerializationLibrary()) {
            case SERIALIZATION_LIBRARY_JACKSON:
                additionalProperties.put(SERIALIZATION_LIBRARY_JACKSON, "true");
                additionalProperties.remove(SERIALIZATION_LIBRARY_JSONB);
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache", invokerFolder, "RFC3339DateFormat.java"));
                if (isLibrary(HELIDON_SE)) {
                    supportingFiles.add(new SupportingFile("jsonProvider.mustache",
                            (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator),
                            "JsonProvider.java"));
                }
                break;
            case SERIALIZATION_LIBRARY_JSONB:
                openApiNullable = false;
                additionalProperties.put(OPENAPI_NULLABLE, false);
                additionalProperties.put(SERIALIZATION_LIBRARY_JSONB, "true");
                additionalProperties.remove(SERIALIZATION_LIBRARY_JACKSON);
                break;
            default:
                additionalProperties.remove(SERIALIZATION_LIBRARY_JACKSON);
                additionalProperties.remove(SERIALIZATION_LIBRARY_JSONB);
                LOGGER.error("Unknown serialization library option");
                break;
        }
    }

    private void addApiTemplateFiles() {
        Boolean fullProject = !additionalProperties.containsKey(FULL_PROJECT) ? null :
                Boolean.parseBoolean(additionalProperties.get(FULL_PROJECT).toString());
        if (fullProject == null && !projectFilesExist()) {
            apiTemplateFiles.put("apiImpl.mustache", "Impl.java");
        } else if (Boolean.TRUE.equals(fullProject)) {
            apiTemplateFiles.put("apiImpl.mustache", "Impl.java");
        }
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation codegenOperation = super.fromOperation(path, httpMethod, operation, servers);
        if (HELIDON_SE.equals(getLibrary())) {
            if (additionalProperties.containsKey(JACKSON)) {
                codegenOperation.imports.add("ObjectMapper");
            }
            if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_JSONB)) {
                codegenOperation.imports.add("Jsonb");
                codegenOperation.imports.add("JsonbBuilder");
            }
            if (codegenOperation.bodyParam != null) {
                codegenOperation.imports.add("Handler");
            }
            if (codegenOperation.queryParams.size() > 0 && useAbstractClass) {
                codegenOperation.imports.add("List");
            }
            if (codegenOperation.formParams.size() > 0 && useAbstractClass) {
                codegenOperation.imports.add("Map");
                codegenOperation.imports.add("HashMap");
                codegenOperation.imports.add("InputStream");
                codegenOperation.imports.add("ReadableBodyPart");
                codegenOperation.imports.add("ArrayList");
                codegenOperation.imports.add("DataChunk");
                codegenOperation.imports.add("ByteArrayOutputStream");
                codegenOperation.imports.add("IOException");
                codegenOperation.imports.add("UncheckedIOException");
                codegenOperation.imports.add("ByteArrayInputStream");
            }
        }
        return codegenOperation;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultService";
        }
        name = sanitizeName(name);
        return camelize(name) + "Service";
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        // remove swagger imports
        codegenModel.imports.remove("ApiModelProperty");
        codegenModel.imports.remove("ApiModel");

        return codegenModel;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap operations = objs.getOperations();
        if (HELIDON_MP.equals(getLibrary())) {
            return AbstractJavaJAXRSServerCodegen.jaxrsPostProcessOperations(objs);
        }
        if (operations != null && HELIDON_SE.equals(getLibrary())) {
            List<CodegenOperation> ops = operations.getOperation();
            for (CodegenOperation operation : ops) {
                if (operation.formParams.size() > 0) {
                    objs.put("isFormParamsFunctions", true);
                }
            }
        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        if (Boolean.TRUE.equals(model.hasEnums)) {
            // Add imports for Jackson
            if (additionalProperties.containsKey(JACKSON)) {
                model.imports.add("JsonValue");
                model.imports.add("JsonCreator");
            }
        }
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "java-helidon-server";
    }

    @Override
    public String getHelp() {
        return "Generates a Java Helidon Server application.";
    }


    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    @Override
    public void setPerformBeanValidation(boolean performBeanValidation) {
        throw new UnsupportedOperationException("Not implemented");
    }

    public String getSerializationLibrary() {
        return serializationLibrary;
    }

    public void setSerializationLibrary(String serializationLibrary) {
        if (SERIALIZATION_LIBRARY_JACKSON.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_JACKSON;
        } else if (SERIALIZATION_LIBRARY_JSONB.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_JSONB;
        } else {
            throw new IllegalArgumentException("Unexpected serializationLibrary value: " + serializationLibrary);
        }
    }

    /**
     * Check if pom file and src directory already exist.
     *
     * @return outcome of test
     */
    @Override
    protected boolean projectFilesExist() {
        Path projectFolder = Paths.get(getOutputTestFolder());
        Path pom = projectFolder.resolve("pom.xml");
        Path buildGradle = projectFolder.resolve("build.gradle");
        Path src = projectFolder.resolve(Paths.get(sourceFolder, invokerPackage.replace('.', File.separatorChar)));
        return (pom.toFile().exists() || buildGradle.toFile().exists()) && src.toFile().exists();
    }
}

