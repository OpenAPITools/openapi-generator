/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class KotlinClientCodegen extends AbstractKotlinCodegen {

    protected static final String JVM = "jvm";
    protected static final String JVM_OKHTTP4 = "jvm-okhttp4";
    protected static final String JVM_OKHTTP3 = "jvm-okhttp3";
    protected static final String MULTIPLATFORM = "multiplatform";

    public static final String DATE_LIBRARY = "dateLibrary";
    public static final String COLLECTION_TYPE = "collectionType";

    protected static final String VENDOR_EXTENSION_BASE_NAME_LITERAL = "x-base-name-literal";

    protected String dateLibrary = DateLibrary.JAVA8.value;
    protected String collectionType = CollectionType.ARRAY.value;

    public enum DateLibrary {
        STRING("string"),
        THREETENBP("threetenbp"),
        JAVA8("java8");

        public final String value;

        DateLibrary(String value) {
            this.value = value;
        }
    }

    public enum CollectionType {
        ARRAY("array"),
        LIST("list");

        public final String value;

        CollectionType(String value) {
            this.value = value;
        }
    }

    /**
     * Constructs an instance of `KotlinClientCodegen`.
     */
    public KotlinClientCodegen() {
        super();

        artifactId = "kotlin-client";
        packageName = "org.openapitools.client";

        // cliOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.artifactId);
        updateOption(CodegenConstants.PACKAGE_NAME, this.packageName);

        outputFolder = "generated-code" + File.separator + "kotlin-client";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "kotlin-client";
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        CliOption dateLibrary = new CliOption(DATE_LIBRARY, "Option. Date library to use");
        Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(DateLibrary.THREETENBP.value, "Threetenbp (jvm only)");
        dateOptions.put(DateLibrary.STRING.value, "String");
        dateOptions.put(DateLibrary.JAVA8.value, "Java 8 native JSR310 (jvm only)");
        dateLibrary.setEnum(dateOptions);
        dateLibrary.setDefault(this.dateLibrary);
        cliOptions.add(dateLibrary);

        CliOption collectionType = new CliOption(COLLECTION_TYPE, "Option. Collection type to use");
        Map<String, String> collectionOptions = new HashMap<>();
        collectionOptions.put(CollectionType.ARRAY.value, "kotlin.Array");
        collectionOptions.put(CollectionType.LIST.value, "kotlin.collections.List");
        collectionType.setEnum(collectionOptions);
        collectionType.setDefault(this.collectionType);
        cliOptions.add(collectionType);

        supportedLibraries.put(JVM_OKHTTP4, "[DEFAULT] Platform: Java Virtual Machine. HTTP client: OkHttp 4.2.0 (Android 5.0+ and Java 8+). JSON processing: Moshi 1.8.0.");
        supportedLibraries.put(JVM_OKHTTP3, "Platform: Java Virtual Machine. HTTP client: OkHttp 3.12.4 (Android 2.3+ and Java 7+). JSON processing: Moshi 1.8.0.");
        supportedLibraries.put(MULTIPLATFORM, "Platform: Kotlin multiplatform. HTTP client: Ktor 1.2.4. JSON processing: Kotlinx Serialization: 0.12.0.");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "Library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        libraryOption.setDefault(JVM_OKHTTP4);
        cliOptions.add(libraryOption);
        setLibrary(JVM_OKHTTP4);
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "kotlin";
    }

    public String getHelp() {
        return "Generates a Kotlin client.";
    }

    public void setDateLibrary(String library) {
        this.dateLibrary = library;
    }

    public void setCollectionType(String collectionType) {
        this.collectionType = collectionType;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (MULTIPLATFORM.equals(getLibrary())) {
            sourceFolder = "src/commonMain/kotlin";
        }

        // infrastructure destination folder
        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", "/");

        // additional properties
        if (additionalProperties.containsKey(DATE_LIBRARY)) {
            setDateLibrary(additionalProperties.get(DATE_LIBRARY).toString());
        }

        // common (jvm/multiplatform) supporting files
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("infrastructure/ApiClient.kt.mustache", infrastructureFolder, "ApiClient.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/ApiAbstractions.kt.mustache", infrastructureFolder, "ApiAbstractions.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/RequestConfig.kt.mustache", infrastructureFolder, "RequestConfig.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/RequestMethod.kt.mustache", infrastructureFolder, "RequestMethod.kt"));

        if (isJVMLibrary()) {
            additionalProperties.put(JVM, true);

            if (JVM_OKHTTP4.equals(getLibrary())) {
                additionalProperties.put(JVM_OKHTTP4, true);
            } else if (JVM_OKHTTP3.equals(getLibrary())) {
                additionalProperties.put(JVM_OKHTTP3, true);
            }

            supportedLibraries.put(JVM, "A workaround to use the same template folder for both 'jvm-okhttp3' and 'jvm-okhttp4'.");
            setLibrary(JVM);

            // jvm specific supporting files
            supportingFiles.add(new SupportingFile("infrastructure/ApplicationDelegates.kt.mustache", infrastructureFolder, "ApplicationDelegates.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/Errors.kt.mustache", infrastructureFolder, "Errors.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/ResponseExtensions.kt.mustache", infrastructureFolder, "ResponseExtensions.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/Serializer.kt.mustache", infrastructureFolder, "Serializer.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/ApiInfrastructureResponse.kt.mustache", infrastructureFolder, "ApiInfrastructureResponse.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/ByteArrayAdapter.kt.mustache", infrastructureFolder, "ByteArrayAdapter.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/LocalDateAdapter.kt.mustache", infrastructureFolder, "LocalDateAdapter.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/LocalDateTimeAdapter.kt.mustache", infrastructureFolder, "LocalDateTimeAdapter.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/UUIDAdapter.kt.mustache", infrastructureFolder, "UUIDAdapter.kt"));
            if (getSerializationLibrary() == SERIALIZATION_LIBRARY_TYPE.gson) {
                supportingFiles.add(new SupportingFile("infrastructure/DateAdapter.kt.mustache", infrastructureFolder,
                        "DateAdapter.kt"));
            }

        } else if (MULTIPLATFORM.equals(getLibrary())) {
            additionalProperties.put(MULTIPLATFORM, true);
            setDateLibrary(DateLibrary.STRING.value);

            // multiplatform default includes
            defaultIncludes.add("io.ktor.client.request.forms.InputProvider");
            defaultIncludes.add(packageName + ".infrastructure.Base64ByteArray");
            defaultIncludes.add(packageName + ".infrastructure.OctetByteArray");

            // multiplatform type mapping
            typeMapping.put("number", "kotlin.Double");
            typeMapping.put("file", "OctetByteArray");
            typeMapping.put("binary", "OctetByteArray");
            typeMapping.put("ByteArray", "Base64ByteArray");
            typeMapping.put("object", "kotlin.String");  // kotlin.Any not serializable

            // multiplatform import mapping
            importMapping.put("BigDecimal", "kotlin.Double");
            importMapping.put("UUID", "kotlin.String");
            importMapping.put("URI", "kotlin.String");
            importMapping.put("InputProvider", "io.ktor.client.request.forms.InputProvider");
            importMapping.put("File", packageName + ".infrastructure.OctetByteArray");
            importMapping.put("Timestamp", "kotlin.String");
            importMapping.put("LocalDateTime", "kotlin.String");
            importMapping.put("LocalDate", "kotlin.String");
            importMapping.put("LocalTime", "kotlin.String");
            importMapping.put("Base64ByteArray", packageName + ".infrastructure.Base64ByteArray");
            importMapping.put("OctetByteArray", packageName + ".infrastructure.OctetByteArray");

            // multiplatform specific supporting files
            supportingFiles.add(new SupportingFile("infrastructure/Base64ByteArray.kt.mustache", infrastructureFolder, "Base64ByteArray.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/Bytes.kt.mustache", infrastructureFolder, "Bytes.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/HttpResponse.kt.mustache", infrastructureFolder, "HttpResponse.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/OctetByteArray.kt.mustache", infrastructureFolder, "OctetByteArray.kt"));

            // multiplatform specific auth
            final String authFolder = (sourceFolder + File.separator + packageName + File.separator + "auth").replace(".", "/");
            supportingFiles.add(new SupportingFile("auth/ApiKeyAuth.kt.mustache", authFolder, "ApiKeyAuth.kt"));
            supportingFiles.add(new SupportingFile("auth/Authentication.kt.mustache", authFolder, "Authentication.kt"));
            supportingFiles.add(new SupportingFile("auth/HttpBasicAuth.kt.mustache", authFolder, "HttpBasicAuth.kt"));
            supportingFiles.add(new SupportingFile("auth/HttpBearerAuth.kt.mustache", authFolder, "HttpBearerAuth.kt"));
            supportingFiles.add(new SupportingFile("auth/OAuth.kt.mustache", authFolder, "OAuth.kt"));

            // multiplatform specific testing files
            supportingFiles.add(new SupportingFile("commonTest/Coroutine.kt.mustache", "src/commonTest/kotlin/util", "Coroutine.kt"));
            supportingFiles.add(new SupportingFile("iosTest/Coroutine.kt.mustache", "src/iosTest/kotlin/util", "Coroutine.kt"));
            supportingFiles.add(new SupportingFile("jsTest/Coroutine.kt.mustache", "src/jsTest/kotlin/util", "Coroutine.kt"));
            supportingFiles.add(new SupportingFile("jvmTest/Coroutine.kt.mustache", "src/jvmTest/kotlin/util", "Coroutine.kt"));

            // gradle wrapper supporting files
            supportingFiles.add(new SupportingFile("gradlew.mustache", "", "gradlew"));
            supportingFiles.add(new SupportingFile("gradlew.bat.mustache", "", "gradlew.bat"));
            supportingFiles.add(new SupportingFile("gradle-wrapper.properties.mustache", "gradle.wrapper".replace(".", File.separator), "gradle-wrapper.properties"));
            supportingFiles.add(new SupportingFile("gradle-wrapper.jar", "gradle.wrapper".replace(".", File.separator), "gradle-wrapper.jar"));
        }

        // date library processing
        if (DateLibrary.THREETENBP.value.equals(dateLibrary)) {
            additionalProperties.put(DateLibrary.THREETENBP.value, true);
            typeMapping.put("date", "LocalDate");
            typeMapping.put("DateTime", "LocalDateTime");
            importMapping.put("LocalDate", "org.threeten.bp.LocalDate");
            importMapping.put("LocalDateTime", "org.threeten.bp.LocalDateTime");
            defaultIncludes.add("org.threeten.bp.LocalDate");
            defaultIncludes.add("org.threeten.bp.LocalDateTime");
        } else if (DateLibrary.STRING.value.equals(dateLibrary)) {
            typeMapping.put("date-time", "kotlin.String");
            typeMapping.put("date", "kotlin.String");
            typeMapping.put("Date", "kotlin.String");
            typeMapping.put("DateTime", "kotlin.String");
        } else if (DateLibrary.JAVA8.value.equals(dateLibrary)) {
            additionalProperties.put(DateLibrary.JAVA8.value, true);
        }

        if (additionalProperties.containsKey(COLLECTION_TYPE)) {
            setCollectionType(additionalProperties.get(COLLECTION_TYPE).toString());
        }

        if (CollectionType.LIST.value.equals(collectionType)) {
            typeMapping.put("array", "kotlin.collections.List");
            typeMapping.put("list", "kotlin.collections.List");
            additionalProperties.put("isList", true);
        }

    }

    private boolean isJVMLibrary() {
        return getLibrary() != null && (getLibrary().contains(JVM_OKHTTP4) || getLibrary().contains(JVM_OKHTTP3));
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        Map<String, Object> objects = super.postProcessModels(objs);
        @SuppressWarnings("unchecked") List<Object> models = (List<Object>) objs.get("models");

        for (Object model : models) {
            @SuppressWarnings("unchecked") Map<String, Object> mo = (Map<String, Object>) model;
            CodegenModel cm = (CodegenModel) mo.get("model");

            // escape the variable base name for use as a string literal
            List<CodegenProperty> vars = Stream.of(
                    cm.vars,
                    cm.allVars,
                    cm.optionalVars,
                    cm.requiredVars,
                    cm.readOnlyVars,
                    cm.readWriteVars,
                    cm.parentVars
            )
                    .flatMap(List::stream)
                    .collect(Collectors.toList());

            for (CodegenProperty var : vars) {
                var.vendorExtensions.put(VENDOR_EXTENSION_BASE_NAME_LITERAL, var.baseName.replace("$", "\\$"));
            }
        }

        return objects;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {

                // set multipart against all relevant operations
                if (operation.hasConsumes == Boolean.TRUE) {
                    if (isMultipartType(operation.consumes)) {
                        operation.isMultipart = Boolean.TRUE;
                    }
                }

                // modify the data type of binary form parameters to a more friendly type for multiplatform builds
                if (MULTIPLATFORM.equals(getLibrary()) && operation.allParams != null) {
                    for (CodegenParameter param : operation.allParams) {
                        if (param.dataFormat != null && param.dataFormat.equals("binary")) {
                            param.baseType = param.dataType = "io.ktor.client.request.forms.InputProvider";
                        }
                    }
                }
            }
        }
        return operations;
    }

    private static boolean isMultipartType(List<Map<String, String>> consumes) {
        Map<String, String> firstType = consumes.get(0);
        if (firstType != null) {
            return "multipart/form-data".equals(firstType.get("mediaType"));
        }
        return false;
    }
}
