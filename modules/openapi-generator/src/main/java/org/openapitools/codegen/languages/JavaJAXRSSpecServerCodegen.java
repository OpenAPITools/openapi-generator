/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import java.util.Locale;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.openapitools.codegen.CodegenConstants.INTERFACE_ONLY;
import static org.openapitools.codegen.CodegenConstants.INTERFACE_ONLY_DESC;
import static org.openapitools.codegen.languages.features.GzipFeatures.USE_GZIP_FEATURE;

/**
 * <p>Mustache templates are located in
 * {@code src/main/resources/JavaJaxRS/spec/} (root templates shared across all libraries) and
 * {@code src/main/resources/JavaJaxRS/spec/libraries/} (library-specific overrides).
 * A library-specific template shadows a root-level template of the same name.
 */
public class JavaJAXRSSpecServerCodegen extends AbstractJavaJAXRSServerCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(JavaJAXRSSpecServerCodegen.class);

    public static final String RETURN_RESPONSE = "returnResponse";
    public static final String RETURN_JBOSS_RESPONSE = "returnJBossResponse";
    public static final String GENERATE_POM = "generatePom";
    public static final String GENERATE_ROOT_RESOURCES = "generateRootResources";
    public static final String USE_SWAGGER_ANNOTATIONS = "useSwaggerAnnotations";
    public static final String USE_MICROPROFILE_OPENAPI_ANNOTATIONS = "useMicroProfileOpenAPIAnnotations";
    public static final String USE_SWAGGER_V3_ANNOTATIONS = "useSwaggerV3Annotations";
    public static final String USE_MUTINY = "useMutiny";
    public static final String OPEN_API_SPEC_FILE_LOCATION = "openApiSpecFileLocation";
    public static final String GENERATE_JSON_CREATOR = "generateJsonCreator";
    public static final String USE_JAKARTA_SECURITY_ANNOTATIONS = "useJakartaSecurityAnnotations";
    public static final String USE_ENUM_CASE_INSENSITIVE = "useEnumCaseInsensitive";
    public static final String USE_SEALED = "useSealed";
    public static final String JSONB_DISCRIMINATOR_DESERIALIZERS = "jsonbDiscriminatorDeserializers";
    public static final String SERIALIZATION_LIBRARY_JACKSON = "jackson";
    public static final String SERIALIZATION_LIBRARY_JSONB = "jsonb";

    public static final String QUARKUS_LIBRARY = "quarkus";
    public static final String THORNTAIL_LIBRARY = "thorntail";
    public static final String OPEN_LIBERTY_LIBRARY = "openliberty";
    public static final String HELIDON_LIBRARY = "helidon";
    public static final String KUMULUZEE_LIBRARY = "kumuluzee";

    private boolean interfaceOnly = false;
    private boolean generateRootResources = true;
    private boolean returnResponse = false;
    private boolean returnJbossResponse = false;
    private boolean generatePom = true;
    private boolean useSwaggerAnnotations = true;
    private boolean useSwaggerV3Annotations = false;
    private boolean useMicroProfileOpenAPIAnnotations = false;
    private boolean useMutiny = false;
    private boolean useJakartaSecurityAnnotations = false;
    private String serializationLibrary = SERIALIZATION_LIBRARY_JACKSON;

    @Setter
    private boolean useEnumCaseInsensitive = false;

    @Setter
    protected boolean useSealed = false;

    @Getter @Setter
    protected boolean jsonbDiscriminatorDeserializers = false;

    private final JakartaSecurityAnnotationProcessor jakartaSecurityAnnotationProcessor = new JakartaSecurityAnnotationProcessor();

    @Getter @Setter
    protected boolean generateJsonCreator = true;

    @Setter
    protected boolean useGzipFeature = false;
    /**
     * -- SETTER --
     * Location where the file containing the spec will be generated in the output folder.
     *
     * @param location location inside the output folder. No file generated when set to null or empty string.
     */
    @Getter @Setter
    private String openApiSpecFileLocation = "src/main/openapi/openapi.yaml";

    public JavaJAXRSSpecServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .includeSecurityFeatures(SecurityFeature.OpenIDConnect,//quarkus only
                        SecurityFeature.OAuth2_ClientCredentials,
                        SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_Password)
        );

        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-jaxrs-server";
        outputFolder = "generated-code/JavaJaxRS-Spec";
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";

        // cliOptions default redefinition need to be updated
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");

        apiTestTemplateFiles.clear(); // TODO: add api test template
        modelTestTemplateFiles.clear(); // TODO: add model test template

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        additionalProperties.put("title", title);

        typeMapping.put("date", "LocalDate");

        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        super.embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "spec";

        removeOption(CodegenConstants.LIBRARY);
        CliOption library = new CliOption(CodegenConstants.LIBRARY, CodegenConstants.LIBRARY_DESC).defaultValue(DEFAULT_LIBRARY);
        supportedLibraries.put(DEFAULT_LIBRARY, "JAXRS spec only, to be deployed in an app server (TomEE, JBoss, WLS, ...)");
        supportedLibraries.put(QUARKUS_LIBRARY, "Server using Quarkus");
        supportedLibraries.put(THORNTAIL_LIBRARY, "Server using Thorntail");
        supportedLibraries.put(OPEN_LIBERTY_LIBRARY, "Server using Open Liberty");
        supportedLibraries.put(HELIDON_LIBRARY, "Server using Helidon");
        supportedLibraries.put(KUMULUZEE_LIBRARY, "Server using KumuluzEE");
        library.setEnum(supportedLibraries);

        cliOptions.add(library);
        cliOptions.add(CliOption.newBoolean(GENERATE_POM, "Whether to generate pom.xml if the file does not already exist.").defaultValue(String.valueOf(generatePom)));
        cliOptions.add(CliOption.newBoolean(INTERFACE_ONLY, INTERFACE_ONLY_DESC).defaultValue(String.valueOf(interfaceOnly)));
        cliOptions.add(CliOption.newBoolean(GENERATE_ROOT_RESOURCES, "Whether to generate the root resource and application classes, only useful if interfaceOnly is true.", generateRootResources));
        cliOptions.add(CliOption.newBoolean(RETURN_RESPONSE, "Whether generate API interface should return javax.ws.rs.core.Response instead of a deserialized entity. Only useful if interfaceOnly is true.").defaultValue(String.valueOf(returnResponse)));
        cliOptions.add(CliOption.newBoolean(RETURN_JBOSS_RESPONSE, "Whether generate API interface should return `org.jboss.resteasy.reactive.RestResponse` instead of a deserialized entity. This flag cannot be combined with `returnResponse` flag. It requires the flag `interfaceOnly` and `useJakartaEE` set to true, because `org.jboss.resteasy.reactive.RestResponse` was introduced in Quarkus 2.x").defaultValue(String.valueOf(returnJbossResponse)));
        cliOptions.add(CliOption.newBoolean(USE_SWAGGER_ANNOTATIONS, "Whether to generate Swagger annotations.", useSwaggerAnnotations));
        cliOptions.add(CliOption.newBoolean(USE_SWAGGER_V3_ANNOTATIONS, "Whether to generate Swagger v3 (OpenAPI v3) annotations.", useSwaggerV3Annotations));
        cliOptions.add(CliOption.newBoolean(USE_MICROPROFILE_OPENAPI_ANNOTATIONS, "Whether to generate Microprofile OpenAPI annotations. Only valid when library is set to quarkus.", useMicroProfileOpenAPIAnnotations));
        cliOptions.add(CliOption.newString(OPEN_API_SPEC_FILE_LOCATION, "Location where the file containing the spec will be generated in the output folder. No file generated when set to null or empty string."));
        cliOptions.add(CliOption.newBoolean(SUPPORT_ASYNC, "Wrap responses in CompletionStage type, allowing asynchronous computation (requires JAX-RS 2.1).", supportAsync));
        cliOptions.add(CliOption.newBoolean(USE_MUTINY, "Whether to use Smallrye Mutiny instead of CompletionStage for asynchronous computation. Only valid when library is set to quarkus.", useMutiny));
        cliOptions.add(CliOption.newBoolean(USE_JAKARTA_SECURITY_ANNOTATIONS, "Whether to generate Jakarta security annotations (@RolesAllowed, @PermitAll). Requires useJakartaEe=true. Currently only supported when library is set to quarkus.", useJakartaSecurityAnnotations));
        cliOptions.add(CliOption.newBoolean(GENERATE_JSON_CREATOR, "Whether to generate a JSON creator constructor for required properties.", generateJsonCreator));
        cliOptions.add(CliOption.newBoolean(USE_ENUM_CASE_INSENSITIVE, "Use `equalsIgnoreCase` when String for enum comparison", useEnumCaseInsensitive));
        cliOptions.add(CliOption.newBoolean(USE_SEALED, "Whether to generate sealed model interfaces and classes.", useSealed));
        cliOptions.add(CliOption.newBoolean(JSONB_DISCRIMINATOR_DESERIALIZERS, "With 'serializationLibrary=jsonb' and 'useJakartaEe=true': generate "
                + "JSON-B deserializers that resolve the concrete class of a model, also in list, set and map properties, from its "
                + "discriminator mapping, and create concrete models with their discriminator values. With Yasson before 3.0.5, "
                + "this does not cover root containers, such as a request body of type Map<String, Pet> (with Yasson 3.0.2 also "
                + "List<Pet>). With Johnzon 2.0.2 and 2.2.0, reading a model fails when a required list, set or map property "
                + "with elements of such models is absent from the JSON (an explicit null is read).", jsonbDiscriminatorDeserializers));
        CliOption serializationLibraryOption = new CliOption(CodegenConstants.SERIALIZATION_LIBRARY, "Serialization library used for the generated models. 'jsonb' is only supported by the default library (" + DEFAULT_LIBRARY + "). With 'jsonb', dateLibrary defaults to 'java8' ('java8-localdatetime' is also supported, 'joda' and 'legacy' are not), openApiNullable is disabled and no type annotations are generated for discriminator/oneOf polymorphism (see jsonbDiscriminatorDeserializers). Models with additionalProperties are generated as subclasses of HashMap without a JSON-B specific mapping, so they are (de)serialized as plain maps: declared properties are not written and are read into map entries instead of their fields. Properties with a null value are omitted, except required nullable properties, which are written as null; JsonbConfig.withNullValues(true) does not change this because the generated @JsonbProperty annotations default to nillable = false. Depending on the JSON-B implementation, enum values inside maps (and, with older implementations, inside lists) may not use the generated enum serializers/deserializers.")
                .defaultValue(SERIALIZATION_LIBRARY_JACKSON);
        Map<String, String> serializationOptions = new HashMap<>();
        serializationOptions.put(SERIALIZATION_LIBRARY_JACKSON, "Use Jackson as serialization library");
        serializationOptions.put(SERIALIZATION_LIBRARY_JSONB, "Use JSON-B as serialization library");
        serializationLibraryOption.setEnum(serializationOptions);
        cliOptions.add(serializationLibraryOption);
        cliOptions.add(CliOption.newBoolean(USE_JSPECIFY, "Use JSpecify for null checks: @NullMarked package-info and @Nullable on optional properties and parameters.", useJspecify));
    }

    @Override
    public void processOpts() {
        convertPropertyToBooleanAndWriteBack(GENERATE_POM, value -> generatePom = value);

        convertPropertyToBooleanAndWriteBack(INTERFACE_ONLY, value -> interfaceOnly = value);
        convertPropertyToBooleanAndWriteBack(GENERATE_ROOT_RESOURCES, value -> generateRootResources = value);
        convertPropertyToBooleanAndWriteBack(RETURN_RESPONSE, value -> returnResponse = value);
        convertPropertyToBooleanAndWriteBack(RETURN_JBOSS_RESPONSE, value -> returnJbossResponse = value);
        convertPropertyToBooleanAndWriteBack(SUPPORT_ASYNC, this::setSupportAsync);
        if (QUARKUS_LIBRARY.equals(library) || THORNTAIL_LIBRARY.equals(library) || HELIDON_LIBRARY.equals(library) || OPEN_LIBERTY_LIBRARY.equals(library) || KUMULUZEE_LIBRARY.equals(library)) {
            // disable Swagger v2 annotations in library modes; MicroProfile or Swagger v3 may be used instead
            useSwaggerAnnotations = false;
        } else {
            convertPropertyToBooleanAndWriteBack(USE_SWAGGER_ANNOTATIONS, value -> useSwaggerAnnotations = value);
        }
        // Swagger v3 can be used regardless of library
        convertPropertyToBooleanAndWriteBack(USE_SWAGGER_V3_ANNOTATIONS, value -> useSwaggerV3Annotations = value);
        // prefer v3 when requested
        if (useSwaggerV3Annotations) {
            useSwaggerAnnotations = false;
        }
        if (KUMULUZEE_LIBRARY.equals(library)) {
            super.setSourceFolder("src/main/java");
        }

        if (useSwaggerAnnotations && useSwaggerV3Annotations) {
            throw new IllegalArgumentException("Flags 'useSwaggerAnnotations' (v2) and 'useSwaggerV3Annotations' (v3) are mutually exclusive. Please enable only one.");
        }
        if (useSwaggerV3Annotations && useMicroProfileOpenAPIAnnotations) {
            throw new IllegalArgumentException("Flags 'useSwaggerV3Annotations' and 'useMicroProfileOpenAPIAnnotations' are mutually exclusive. Please enable only one.");
        }

        if (QUARKUS_LIBRARY.equals(library)) {
            convertPropertyToBooleanAndWriteBack(USE_MICROPROFILE_OPENAPI_ANNOTATIONS, value -> useMicroProfileOpenAPIAnnotations = value);
        }

        if (QUARKUS_LIBRARY.equals(library)) {
            convertPropertyToBooleanAndWriteBack(USE_MUTINY, value -> useMutiny = value);
        }

        convertPropertyToBooleanAndWriteBack(GENERATE_JSON_CREATOR, this::setGenerateJsonCreator);
        convertPropertyToBooleanAndWriteBack(USE_ENUM_CASE_INSENSITIVE, this::setUseEnumCaseInsensitive);
        convertPropertyToBooleanAndWriteBack(USE_SEALED, this::setUseSealed);
        convertPropertyToBooleanAndWriteBack(JSONB_DISCRIMINATOR_DESERIALIZERS, this::setJsonbDiscriminatorDeserializers);

        if (additionalProperties.containsKey(OPEN_API_SPEC_FILE_LOCATION)) {
            openApiSpecFileLocation = additionalProperties.get(OPEN_API_SPEC_FILE_LOCATION).toString();
        } else if (QUARKUS_LIBRARY.equals(library) || THORNTAIL_LIBRARY.equals(library) || HELIDON_LIBRARY.equals(library) || KUMULUZEE_LIBRARY.equals(library)) {
            openApiSpecFileLocation = "src/main/resources/META-INF/openapi.yaml";
        } else if (OPEN_LIBERTY_LIBRARY.equals(library)) {
            openApiSpecFileLocation = "src/main/webapp/META-INF/openapi.yaml";
        }

        additionalProperties.put(OPEN_API_SPEC_FILE_LOCATION, openApiSpecFileLocation);

        if (interfaceOnly) {
            // Change default artifactId if generating interfaces only, before command line options are applied in base class.
            artifactId = "openapi-jaxrs-client";
        }

        convertPropertyToStringAndWriteBack(CodegenConstants.SERIALIZATION_LIBRARY, this::setSerializationLibrary);
        // JSON-B cannot map the Joda types of the legacy default; select java8 before super.processOpts() applies the date type mappings
        if (isJsonb() && !additionalProperties.containsKey(DATE_LIBRARY) && "legacy".equals(getDateLibrary())) {
            setDateLibrary("java8");
        }

        super.processOpts();

        // We need to call super.processOpts() before evaluating the `library`, otherwise `library` is null when set via `configOptions` instead of via `library.set("quarkus")` in Gradle
        if (QUARKUS_LIBRARY.equals(library)) {
            convertPropertyToBooleanAndWriteBack(USE_JAKARTA_SECURITY_ANNOTATIONS, value -> useJakartaSecurityAnnotations = value);
        }

        if (useJakartaSecurityAnnotations && !useJakartaEe) {
            throw new IllegalArgumentException(
                    "Flag '" + USE_JAKARTA_SECURITY_ANNOTATIONS + "' requires '" + USE_JAKARTA_EE
                            + "=true'. The generated annotation '@jakarta.annotation.security.RolesAllowed' "
                            + "is incompatible with the javax.* namespace.");
        }

        if (isJsonb()) {
            applyJsonbSerialization();
        }
        if (jsonbDiscriminatorDeserializers && !isJsonb()) {
            LOGGER.warn("'{}' only applies to '{}={}' and is ignored.", JSONB_DISCRIMINATOR_DESERIALIZERS, CodegenConstants.SERIALIZATION_LIBRARY, SERIALIZATION_LIBRARY_JSONB);
            jsonbDiscriminatorDeserializers = false;
            additionalProperties.put(JSONB_DISCRIMINATOR_DESERIALIZERS, false);
        }
        if (jsonbDiscriminatorDeserializers && !useJakartaEe) {
            throw new IllegalArgumentException("'" + JSONB_DISCRIMINATOR_DESERIALIZERS + "=true' requires '" + USE_JAKARTA_EE
                    + "=true': the generated deserializers use the Jakarta JSON-B 3.0 API (jakarta.json.bind).");
        }

        // expose flags to templates
        additionalProperties.put(USE_SWAGGER_ANNOTATIONS, useSwaggerAnnotations);
        additionalProperties.put(USE_SWAGGER_V3_ANNOTATIONS, useSwaggerV3Annotations);
        additionalProperties.put(USE_MICROPROFILE_OPENAPI_ANNOTATIONS, useMicroProfileOpenAPIAnnotations);

        supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md")
                .doNotOverwrite());

        // after the clear() above, which would otherwise drop the @NullMarked package-info files
        if (useJspecify) {
            applyJspecify();
        }

        if ((!interfaceOnly) || generateRootResources) {
            supportingFiles.add(new SupportingFile("RestResourceRoot.mustache",
                    (sourceFolder + '/' + invokerPackage).replace(".", "/"), "RestResourceRoot.java")
                    .doNotOverwrite());

            supportingFiles.add(new SupportingFile("RestApplication.mustache",
                    (sourceFolder + '/' + invokerPackage).replace(".", "/"), "RestApplication.java")
                    .doNotOverwrite());
        }

        if (generatePom) {
            supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml")
                    .doNotOverwrite());
        }

        if (StringUtils.isNotEmpty(openApiSpecFileLocation)) {
            int index = openApiSpecFileLocation.lastIndexOf('/');
            String fileFolder;
            String fileName;
            if (index >= 0) {
                fileFolder = openApiSpecFileLocation.substring(0, index);
                fileName = openApiSpecFileLocation.substring(index + 1);
            } else {
                fileFolder = "";
                fileName = openApiSpecFileLocation;
            }
            supportingFiles.add(new SupportingFile("openapi.mustache", fileFolder, fileName));
        }

        if (QUARKUS_LIBRARY.equals(library)) {
            supportingFiles.add(new SupportingFile("application.properties.mustache", "src/main/resources", "application.properties")
                    .doNotOverwrite());
            supportingFiles.add(new SupportingFile("Dockerfile.jvm.mustache", "src/main/docker", "Dockerfile.jvm")
                    .doNotOverwrite());
            supportingFiles.add(new SupportingFile("Dockerfile.native.mustache", "src/main/docker", "Dockerfile.native")
                    .doNotOverwrite());
            supportingFiles.add(new SupportingFile("dockerignore.mustache", "", ".dockerignore")
                    .doNotOverwrite());
            if(returnResponse && returnJbossResponse) {
              String msg = String.format(Locale.ROOT,
                  "You cannot combine [%s] and [%s] since they are mutually exclusive",
                  RETURN_RESPONSE, RETURN_JBOSS_RESPONSE);
              throw new IllegalArgumentException(msg);
            }
            if(returnJbossResponse && !useJakartaEe) {
              String msg = String.format(Locale.ROOT,
                  "The [%s] requires [%s] to be true, because org.jboss.resteasy.reactive.RestResponse was introduced in Quarkus 2.x",
                  RETURN_JBOSS_RESPONSE, USE_JAKARTA_EE);
              throw new IllegalArgumentException(msg);
            }
        } else if (OPEN_LIBERTY_LIBRARY.equals(library)) {
            supportingFiles.add(new SupportingFile("server.xml.mustache", "src/main/liberty/config", "server.xml")
                    .doNotOverwrite());
            supportingFiles.add(new SupportingFile("beans.xml.mustache", "src/main/webapp/META-INF", "beans.xml")
                    .doNotOverwrite());
            supportingFiles.add(new SupportingFile("MANIFEST.MF.mustache", "src/main/webapp/META-INF", "MANIFEST.MF")
                    .doNotOverwrite());
            supportingFiles.add(new SupportingFile("microprofile-config.properties.mustache", "src/main/webapp/META-INF", "microprofile-config.properties")
                    .doNotOverwrite());
            supportingFiles.add(new SupportingFile("ibm-web-ext.xml.mustache", "src/main/webapp/WEB-INF", "ibm-web-ext.xml")
                    .doNotOverwrite());
        } else if (HELIDON_LIBRARY.equals(library)) {
            additionalProperties.computeIfAbsent("helidonVersion", key -> "2.4.1");
            supportingFiles.add(new SupportingFile("logging.properties.mustache", "src/main/resources", "logging.properties")
                    .doNotOverwrite());
            supportingFiles.add(new SupportingFile("microprofile-config.properties.mustache", "src/main/resources/META-INF", "microprofile-config.properties")
                    .doNotOverwrite());
            supportingFiles.add(new SupportingFile("beans.xml.mustache", "src/main/resources/META-INF", "beans.xml")
                    .doNotOverwrite());
        } else if (KUMULUZEE_LIBRARY.equals(library)) {
            supportingFiles.add(new SupportingFile("config.yaml.mustache", "src/main/resources", "config.yaml"));
        }

        convertPropertyToBooleanAndWriteBack(USE_GZIP_FEATURE, this::setUseGzipFeature);
    }

    @Override
    public String getName() {
        return "jaxrs-spec";
    }

    @Override
    public List<VendorExtension> getSupportedVendorExtensions() {
        List<VendorExtension> extensions = super.getSupportedVendorExtensions();
        extensions.add(VendorExtension.X_ABSTRACT);
        return extensions;
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
            throw new IllegalArgumentException("Unexpected " + CodegenConstants.SERIALIZATION_LIBRARY + " value: '"
                    + serializationLibrary + "'. Supported values: " + SERIALIZATION_LIBRARY_JACKSON + ", " + SERIALIZATION_LIBRARY_JSONB);
        }
    }

    public boolean isJsonb() {
        return SERIALIZATION_LIBRARY_JSONB.equals(serializationLibrary);
    }

    private void applyJsonbSerialization() {
        if (StringUtils.isNotEmpty(library) && !DEFAULT_LIBRARY.equals(library)) {
            throw new IllegalArgumentException("'" + CodegenConstants.SERIALIZATION_LIBRARY + "=" + SERIALIZATION_LIBRARY_JSONB
                    + "' is only supported by the library '" + DEFAULT_LIBRARY + "', not by '" + library + "'.");
        }
        if (!getDateLibrary().startsWith("java8")) {
            throw new IllegalArgumentException("'" + DATE_LIBRARY + "=" + getDateLibrary() + "' is not supported with '"
                    + CodegenConstants.SERIALIZATION_LIBRARY + "=" + SERIALIZATION_LIBRARY_JSONB + "'. Use 'java8' or 'java8-localdatetime'.");
        }
        if (additionalProperties.containsKey(JACKSON) && jackson) {
            throw new IllegalArgumentException("Flags '" + JACKSON + "=true' and '" + CodegenConstants.SERIALIZATION_LIBRARY + "="
                    + SERIALIZATION_LIBRARY_JSONB + "' are mutually exclusive. Please enable only one.");
        }
        if (additionalProperties.containsKey(OPENAPI_NULLABLE) && openApiNullable) {
            LOGGER.warn("'{}' is not supported with '{}={}' and will be disabled.",
                    OPENAPI_NULLABLE, CodegenConstants.SERIALIZATION_LIBRARY, SERIALIZATION_LIBRARY_JSONB);
        }
        setJackson(false);
        additionalProperties.put(JACKSON, false);
        setOpenApiNullable(false);
        additionalProperties.put(OPENAPI_NULLABLE, false);
        additionalProperties.put(SERIALIZATION_LIBRARY_JSONB, true);
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        if (!useSwaggerAnnotations) {
            codegenModel.imports.remove("ApiModelProperty");
            codegenModel.imports.remove("ApiModel");
        }
        if (!jackson) {
            codegenModel.imports.remove("JsonValue");
            codegenModel.imports.remove("JsonProperty");
            codegenModel.imports.remove("JsonTypeName");
        }
        if (isJsonb() && codegenModel.getAdditionalProperties() != null) {
            LOGGER.warn("Model '{}' declares additionalProperties and is (de)serialized as a plain map with '{}={}'; declared properties are not mapped to its fields.",
                    codegenModel.name, CodegenConstants.SERIALIZATION_LIBRARY, SERIALIZATION_LIBRARY_JSONB);
        }
        if (useJspecify) {
            codegenModel.imports.add("Nullable");
        }

        return codegenModel;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String getHelp() {
        return "Generates a Java JAXRS Server according to JAXRS 2.0 specification.";
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        // Add imports for java.util.Arrays
        if (property.isByteArray) {
            model.imports.add("Arrays");
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        return super.postProcessModels(objs);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        removeImport(objs, "java.util.List");
        if (QUARKUS_LIBRARY.equals(library) && !returnResponse && !returnJbossResponse && useJakartaEe) {
            for (CodegenOperation op : objs.getOperations().getOperation()) {
                op.responses.stream()
                        .filter(r -> r.is2xx && r.code.matches("\\d+"))
                        .findFirst()
                        .ifPresent(r -> op.vendorExtensions.put("x-java-success-response-code", r.code));
            }
            boolean hasAnnotations = objs.getOperations().getOperation().stream()
                    .anyMatch(op -> op.vendorExtensions.containsKey("x-java-success-response-code"));
            // Always set explicitly so Mustache does not fall through to the global additionalProperties
            // value when this file has no annotated operations.
            objs.put("hasResponseStatusAnnotations", hasAnnotations);
            if (hasAnnotations) {
                // Global flag used by pom.mustache, which is rendered once per project.
                additionalProperties.put("hasResponseStatusAnnotations", true);
            }
        }

        // The quarkus templates bind file form parameters to org.jboss.resteasy.reactive types, so the
        // corresponding imports must only be emitted for API files that actually declare such a parameter.
        // Always set explicitly so Mustache does not fall through to the global additionalProperties value.
        objs.put("hasFileFormParams", objs.getOperations().getOperation().stream()
                .flatMap(op -> op.formParams.stream())
                .anyMatch(p -> p.isFile));
        return objs;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> result = super.postProcessAllModels(objs);
        List<CodegenModel> allModels = new ArrayList<>();
        result.values().forEach(modelsMap -> modelsMap.getModels().forEach(modelMap -> allModels.add(modelMap.getModel())));
        List<CodegenModel> abstractModels = supportsAbstractModels() ? applyAbstractModels(allModels) : Collections.emptyList();

        // Index the discriminators of the generated oneOf interfaces by classname. A child of a oneOf
        // interface (useOneOfInterfaces) inherits a shared base via allOf and therefore has no Java
        // parentModel carrying the discriminator - the discriminator lives on the interface it implements.
        Map<String, CodegenDiscriminator> oneOfInterfaceDiscriminators = new HashMap<>();
        for (ModelsMap modelsMap : result.values()) {
            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel model = modelMap.getModel();
                if (isOneOfInterface(model) && model.getDiscriminator() != null) {
                    oneOfInterfaceDiscriminators.put(model.classname, model.getDiscriminator());
                }
            }
        }

        for (ModelsMap modelsMap : result.values()) {
            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel model = modelMap.getModel();
                // Resolve the @JsonTypeName mapping value from the Java parent's discriminator, if any,
                // otherwise from the discriminator of the oneOf interface the model implements.
                CodegenDiscriminator discriminator = model.parentModel != null
                        ? model.parentModel.getDiscriminator()
                        : discriminatorOfImplementedOneOfInterface(model, oneOfInterfaceDiscriminators);
                String discriminatorValue = mappingNameOf(discriminator, model);
                if (discriminatorValue == null) {
                    // a grandchild may be mapped by the discriminator of a more distant ancestor; without it Jackson
                    // would write the class name as type id
                    discriminatorValue = ancestorMappingNameOf(model);
                }
                if (discriminatorValue != null) {
                    model.getVendorExtensions().put("x-discriminator-value", discriminatorValue);
                }
                // A oneOf container rendered as a plain class (useOneOfInterfaces disabled or the model
                // not selected for interface generation) must not be sealed over its oneOf members: they
                // do not extend it, so a permits clause would not compile. Only child-derived permits
                // (subclasses that actually extend the model) may remain.
                if (useSealed && !isOneOfInterface(model)) {
                    model.permits.removeAll(model.oneOf);
                }
            }
        }
        validateAbstractModelsHaveSubclasses(allModels, abstractModels);
        if (jsonbDiscriminatorDeserializers) {
            new JsonbPolymorphismProcessor(this, allModels).apply();
        }
        return result;
    }

    /**
     * Whether this generator's templates implement the {@code x-abstract} vendor extension.
     */
    protected boolean supportsAbstractModels() {
        return true;
    }

    /**
     * Normalizes {@code x-abstract} to a {@link Boolean} (a string {@code "false"} would be truthy in the templates) and
     * ensures that no discriminator value resolves to an abstract class, which neither Jackson nor JSON-B can
     * instantiate: an explicit mapping (a {@code mapping} entry or {@code x-discriminator-value}) to an abstract model
     * fails the generation, an implicit one is replaced by mappings to its concrete subclasses. Every copy of a
     * discriminator is checked, since with {@code legacyDiscriminatorBehavior=false} children inherit their parent's.
     *
     * @return the models marked abstract
     */
    private List<CodegenModel> applyAbstractModels(List<CodegenModel> allModels) {
        String xAbstract = VendorExtension.X_ABSTRACT.getName();
        Map<String, CodegenModel> abstractModelsBySchemaName = new HashMap<>();
        for (CodegenModel model : allModels) {
            Object value = model.getVendorExtensions().get(xAbstract);
            if (value == null) {
                continue;
            }
            boolean isAbstract = Boolean.parseBoolean(String.valueOf(value));
            if (isAbstract && (model.isEnum || isOneOfInterface(model))) {
                LOGGER.warn("Ignoring '{}' on model '{}': only model classes can be abstract.", xAbstract, model.name);
                isAbstract = false;
            }
            model.getVendorExtensions().put(xAbstract, isAbstract);
            if (isAbstract) {
                abstractModelsBySchemaName.put(model.schemaName, model);
            }
        }
        if (abstractModelsBySchemaName.isEmpty()) {
            return Collections.emptyList();
        }

        // Visit the models that declare a discriminator before those that inherit a copy of it, so that an
        // error names the schema where the offending mapping is written.
        List<CodegenModel> discriminatorOwners = allModels.stream()
                .filter(model -> model.getDiscriminator() != null && model.getDiscriminator().getMappedModels() != null)
                .sorted(Comparator.comparing(model -> declaresDiscriminator(model) ? 0 : 1))
                .collect(Collectors.toList());
        for (CodegenModel owner : discriminatorOwners) {
            CodegenDiscriminator discriminator = owner.getDiscriminator();
            List<CodegenModel> droppedAbstractModels = new ArrayList<>();
            Iterator<CodegenDiscriminator.MappedModel> mappedModels = discriminator.getMappedModels().iterator();
            while (mappedModels.hasNext()) {
                CodegenDiscriminator.MappedModel mappedModel = mappedModels.next();
                CodegenModel target = abstractModelsBySchemaName.get(mappedModel.getSchemaName());
                if (target == null) {
                    continue;
                }
                if (mappedModel.isExplicitMapping()) {
                    throw new IllegalArgumentException(String.format(Locale.ROOT,
                            "The discriminator value '%s' of property '%s' in model '%s' maps to model '%s', which is marked '%s'. "
                                    + "An abstract class cannot be instantiated: map each discriminator value directly to a concrete "
                                    + "subclass, or remove '%s'.",
                            mappedModel.getMappingName(), discriminator.getPropertyBaseName(), owner.name, target.name, xAbstract, xAbstract));
                }
                LOGGER.info("Dropping the implicit discriminator mapping '{}' of model '{}': model '{}' is marked '{}'.",
                        mappedModel.getMappingName(), owner.name, target.name, xAbstract);
                mappedModels.remove();
                droppedAbstractModels.add(target);
            }
            for (CodegenModel droppedAbstractModel : droppedAbstractModels) {
                mapConcreteDescendants(owner, droppedAbstractModel, allModels);
            }
        }
        return new ArrayList<>(abstractModelsBySchemaName.values());
    }

    /**
     * Replaces a dropped implicit mapping to an abstract model by mappings to the concrete classes that extend it, under
     * every value that Jackson would resolve them by transitively through the abstract model. Without this, a concrete
     * descendant of an abstract oneOf member could not be read through the oneOf interface. The Java hierarchy and the
     * {@code permits} clauses stay unchanged.
     */
    private void mapConcreteDescendants(CodegenModel owner, CodegenModel abstractModel, List<CodegenModel> allModels) {
        CodegenDiscriminator discriminator = owner.getDiscriminator();
        for (CodegenModel descendant : allModels) {
            if (isAbstract(descendant) || !extendsModel(descendant, abstractModel)) {
                continue;
            }
            for (String mappingName : transitiveMappingNamesOf(descendant, abstractModel)) {
                Optional<CodegenDiscriminator.MappedModel> existing = discriminator.getMappedModels().stream()
                        .filter(mappedModel -> mappingName.equals(mappedModel.getMappingName()))
                        .findFirst();
                if (existing.isPresent()) {
                    if (descendant.schemaName.equals(existing.get().getSchemaName())) {
                        continue;
                    }
                    throw new IllegalArgumentException(String.format(Locale.ROOT,
                            "The discriminator value '%s' of property '%s' in model '%s' would map to both model '%s' and model '%s' "
                                    + "(a concrete subclass of the '%s' model '%s'). Give the models distinct discriminator values.",
                            mappingName, discriminator.getPropertyBaseName(), owner.name, existing.get().getModelName(), descendant.name,
                            VendorExtension.X_ABSTRACT.getName(), abstractModel.name));
                }
                // only a value that is not mapped yet must be dispatchable by the owner's discriminator property
                validateSameDiscriminatorProperty(owner, abstractModel, descendant);
                CodegenDiscriminator.MappedModel mappedModel =
                        new CodegenDiscriminator.MappedModel(mappingName, descendant.classname, descendant.schemaName, false);
                mappedModel.setModel(descendant);
                discriminator.getMappedModels().add(mappedModel);
                LOGGER.info("Mapping the discriminator value '{}' of model '{}' to model '{}', a concrete subclass of the '{}' model '{}'.",
                        mappingName, owner.name, descendant.name, VendorExtension.X_ABSTRACT.getName(), abstractModel.name);
            }
        }
    }

    /**
     * The values that Jackson resolves the descendant by through the abstract model: its own type name and every value
     * that a model on the inheritance path up to the abstract model maps to it.
     */
    private static Set<String> transitiveMappingNamesOf(CodegenModel descendant, CodegenModel abstractModel) {
        Set<String> mappingNames = new LinkedHashSet<>();
        mappingNames.add(Objects.requireNonNullElse(ancestorMappingNameOf(descendant), descendant.schemaName));
        for (CodegenModel ancestor = descendant.parentModel; ancestor != null; ancestor = ancestor.parentModel) {
            CodegenDiscriminator ancestorDiscriminator = ancestor.getDiscriminator();
            if (ancestorDiscriminator != null && ancestorDiscriminator.getMappedModels() != null) {
                ancestorDiscriminator.getMappedModels().stream()
                        .filter(mappedModel -> descendant.schemaName.equals(mappedModel.getSchemaName()))
                        .forEach(mappedModel -> mappingNames.add(mappedModel.getMappingName()));
            }
            if (ancestor.classname.equals(abstractModel.classname)) {
                break;
            }
        }
        return mappingNames;
    }

    /**
     * Multi-level discriminators are not supported here: every model on the inheritance path from the descendant up to
     * the abstract model must dispatch by the owner's discriminator property (the abstract model must have a
     * discriminator, intermediate models may have none), otherwise the owner cannot dispatch on the descendant's type id.
     */
    private static void validateSameDiscriminatorProperty(CodegenModel owner, CodegenModel abstractModel, CodegenModel descendant) {
        String property = owner.getDiscriminator().getPropertyBaseName();
        for (CodegenModel ancestor = descendant.parentModel; ancestor != null; ancestor = ancestor.parentModel) {
            boolean isAbstractModel = ancestor.classname.equals(abstractModel.classname);
            CodegenDiscriminator ancestorDiscriminator = ancestor.getDiscriminator();
            if ((ancestorDiscriminator == null && isAbstractModel)
                    || (ancestorDiscriminator != null && !Objects.equals(ancestorDiscriminator.getPropertyBaseName(), property))) {
                throw new IllegalArgumentException(String.format(Locale.ROOT,
                        "Model '%s' extends model '%s', which is marked '%s' and is implicitly mapped by the discriminator "
                                + "property '%s' of model '%s'. Model '%s' on this inheritance path resolves its subclasses by %s, "
                                + "so model '%s' cannot be mapped by '%s' instead: use the same discriminator property on every "
                                + "level or map the values of '%s' explicitly to concrete classes.",
                        descendant.name, abstractModel.name, VendorExtension.X_ABSTRACT.getName(), property, owner.name, ancestor.name,
                        ancestorDiscriminator == null
                                ? "no discriminator"
                                : "the discriminator property '" + ancestorDiscriminator.getPropertyBaseName() + "'",
                        descendant.name, property, owner.name));
            }
            if (isAbstractModel) {
                return;
            }
        }
    }

    private static boolean extendsModel(CodegenModel model, CodegenModel ancestor) {
        for (CodegenModel parent = model.parentModel; parent != null; parent = parent.parentModel) {
            if (parent.classname.equals(ancestor.classname)) {
                return true;
            }
        }
        return false;
    }

    static boolean isAbstract(CodegenModel model) {
        return Boolean.TRUE.equals(model.getVendorExtensions().get(VendorExtension.X_ABSTRACT.getName()));
    }

    static boolean isOneOfInterface(CodegenModel model) {
        return Boolean.TRUE.equals(model.getVendorExtensions().get(CodegenConstants.X_IS_ONE_OF_INTERFACE));
    }

    /**
     * An abstract model without generated subclasses could only be extended by hand-written code. Under
     * {@code useSealed} that subclass would have to be {@code non-sealed}, which reopens the sealed hierarchy, so such a
     * model is rejected there.
     */
    private void validateAbstractModelsHaveSubclasses(List<CodegenModel> allModels, List<CodegenModel> abstractModels) {
        if (abstractModels.isEmpty()) {
            return;
        }
        String xAbstract = VendorExtension.X_ABSTRACT.getName();
        Set<String> extendedClassnames = allModels.stream()
                .map(CodegenModel::getParent)
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        for (CodegenModel model : abstractModels) {
            if (extendedClassnames.contains(model.classname)) {
                continue;
            }
            Optional<CodegenModel> sealedSupertype = useSealed
                    ? allModels.stream().filter(candidate -> candidate.permits.contains(model.classname)).findFirst()
                    : Optional.empty();
            if (sealedSupertype.isPresent()) {
                throw new IllegalArgumentException(String.format(Locale.ROOT,
                        "Model '%s' is marked '%s' and is a permitted subtype of the sealed type '%s', but no generated model "
                                + "extends it. With '%s=true' it could only be declared non-sealed, which would reopen the sealed "
                                + "hierarchy. Remove '%s' or add a subclass to the specification.",
                        model.name, xAbstract, sealedSupertype.get().classname, USE_SEALED, xAbstract));
            }
            LOGGER.warn("Model '{}' is marked '{}', but no generated model extends it.", model.name, xAbstract);
        }
    }

    /**
     * Returns the discriminator value that the given discriminator maps to the model, or {@code null}.
     */
    private static String mappingNameOf(CodegenDiscriminator discriminator, CodegenModel model) {
        if (discriminator == null || discriminator.getMappedModels() == null) {
            return null;
        }
        for (CodegenDiscriminator.MappedModel mappedModel : discriminator.getMappedModels()) {
            if (model.schemaName.equals(mappedModel.getSchemaName())) {
                return mappedModel.getMappingName();
            }
        }
        return null;
    }

    /**
     * Returns the discriminator value that the nearest ancestor whose discriminator maps the model assigns to it, or
     * {@code null}.
     */
    private static String ancestorMappingNameOf(CodegenModel model) {
        for (CodegenModel ancestor = model.parentModel; ancestor != null; ancestor = ancestor.parentModel) {
            String mappingName = mappingNameOf(ancestor.getDiscriminator(), model);
            if (mappingName != null) {
                return mappingName;
            }
        }
        return null;
    }

    private boolean declaresDiscriminator(CodegenModel model) {
        Schema schema = ModelUtils.getSchema(openAPI, model.schemaName);
        return schema != null && schema.getDiscriminator() != null;
    }

    /**
     * Returns the discriminator of the oneOf interface (produced by useOneOfInterfaces) that the given
     * model implements, or {@code null} if the model does not implement such an interface. The interface
     * name is read from the model's {@code x-implements} vendor extension.
     */
    @SuppressWarnings("unchecked")
    private CodegenDiscriminator discriminatorOfImplementedOneOfInterface(
            CodegenModel model, Map<String, CodegenDiscriminator> oneOfInterfaceDiscriminators) {
        Object implementsExtension = model.getVendorExtensions().get("x-implements");
        if (!(implementsExtension instanceof Collection)) {
            return null;
        }
        for (Object intf : (Collection<Object>) implementsExtension) {
            CodegenDiscriminator discriminator = oneOfInterfaceDiscriminators.get(String.valueOf(intf));
            if (discriminator != null) {
                return discriminator;
            }
        }
        return null;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);
        if (QUARKUS_LIBRARY.equals(getLibrary()) && useJakartaSecurityAnnotations) {
            jakartaSecurityAnnotationProcessor.applyTo(op, operation, openAPI);
        }
        if (useJspecify) {
            addNullableImportForOperation(op);
        }
        return op;
    }

    @Override
    protected void applyJspecify() {
        super.applyJspecify();
        // nullable_var_annotations.mustache emits @{{javaxPackage}}.annotation.Nullable; the lambda
        // finds that and re-injects a bare @Nullable in type-use position, so tell it what to look for.
        jSpecifyNullableLambda.setNullableAnnotation("@" + additionalProperties.get(JAVAX_PACKAGE) + ".annotation.Nullable");
    }

}
