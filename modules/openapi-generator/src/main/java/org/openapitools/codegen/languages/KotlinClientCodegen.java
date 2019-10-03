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
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

public class KotlinClientCodegen extends AbstractKotlinCodegen {

    protected static final String VENDOR_EXTENSION_ESCAPED_NAME = "x-escapedName";

    protected static final String JVM = "jvm";
    protected static final String JVM_OKHTTP4 = "jvm-okhttp4";
    protected static final String JVM_OKHTTP3 = "jvm-okhttp3";
    protected static final String MULTIPLATFORM = "multiplatform";

    public static final String DATE_LIBRARY = "dateLibrary";
    public static final String COLLECTION_TYPE = "collectionType";

    protected String dateLibrary = DateLibrary.JAVA8.value;
    protected String collectionType = CollectionType.ARRAY.value;

    // https://kotlinlang.org/docs/reference/grammar.html#Identifier
    protected static final Pattern IDENTIFIER_PATTERN =
            Pattern.compile("[\\p{Ll}\\p{Lm}\\p{Lo}\\p{Lt}\\p{Lu}\\p{Nl}_][\\p{Ll}\\p{Lm}\\p{Lo}\\p{Lt}\\p{Lu}\\p{Nl}\\p{Nd}_]*");

    // https://kotlinlang.org/docs/reference/grammar.html#Identifier
    protected static final String IDENTIFIER_REPLACEMENTS =
            "[.;:/\\[\\]<>]";

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

        } else if (MULTIPLATFORM.equals(getLibrary())) {
            additionalProperties.put(MULTIPLATFORM, true);
            setDateLibrary(DateLibrary.STRING.value);

            // multiplatform default includes
            defaultIncludes.add("io.ktor.client.request.forms.InputProvider");

            // multiplatform type mapping
            typeMapping.put("number", "kotlin.Double");
            typeMapping.put("file", "InputProvider");

            // multiplatform import mapping
            importMapping.put("BigDecimal", "kotlin.Double");
            importMapping.put("UUID", "kotlin.String");
            importMapping.put("URI", "kotlin.String");
            importMapping.put("InputProvider", "io.ktor.client.request.forms.InputProvider");
            importMapping.put("File", "io.ktor.client.request.forms.InputProvider");
            importMapping.put("Timestamp", "kotlin.String");
            importMapping.put("LocalDateTime", "kotlin.String");
            importMapping.put("LocalDate", "kotlin.String");
            importMapping.put("LocalTime", "kotlin.String");

            // multiplatform specific supporting files
            supportingFiles.add(new SupportingFile("infrastructure/HttpResponse.kt.mustache", infrastructureFolder, "HttpResponse.kt"));

            // multiplatform specific testing files
            final String testFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", "/");
            supportingFiles.add(new SupportingFile("commonTest/coroutine.mustache", "src/commonTest/kotlin/util", "Coroutine.kt"));
            supportingFiles.add(new SupportingFile("iosTest/coroutine.mustache", "src/iosTest/kotlin/util", "Coroutine.kt"));
            supportingFiles.add(new SupportingFile("jvmTest/coroutine.mustache", "src/jvmTest/kotlin/util", "Coroutine.kt"));

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
        objs = super.postProcessModels(objs);
        return postProcessModelsEscapeNames(objs);
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> postProcessModelsEscapeNames(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            if (cm.vars != null) {
                for (CodegenProperty var : cm.vars) {
                    var.vendorExtensions.put(VENDOR_EXTENSION_ESCAPED_NAME, escapeIdentifier(var.name));
                }
            }
            if (cm.requiredVars != null) {
                for (CodegenProperty var : cm.requiredVars) {
                    var.vendorExtensions.put(VENDOR_EXTENSION_ESCAPED_NAME, escapeIdentifier(var.name));
                }
            }
            if (cm.optionalVars != null) {
                for (CodegenProperty var : cm.optionalVars) {
                    var.vendorExtensions.put(VENDOR_EXTENSION_ESCAPED_NAME, escapeIdentifier(var.name));
                }
            }
        }
        return objs;
    }

    private static String escapeIdentifier(String identifier) {

        // the kotlin grammar permits a wider set of characters in their identifiers that all target
        // platforms permit (namely jvm). in order to remain compatible with target platforms, we
        // initially replace all illegal target characters before escaping the identifier if required.
        identifier = identifier.replaceAll(IDENTIFIER_REPLACEMENTS, "_");
        if (IDENTIFIER_PATTERN.matcher(identifier).matches()) return identifier;
        return '`' + identifier + '`';
    }

    private static void removeDuplicates(List<CodegenProperty> list) {
        Set<String> set = new HashSet<>();
        Iterator<CodegenProperty> iterator = list.iterator();
        while (iterator.hasNext()) {
            CodegenProperty item = iterator.next();
            if (set.contains(item.name)) iterator.remove();
            else set.add(item.name);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.hasConsumes == Boolean.TRUE) {
                    if (isMultipartType(operation.consumes)) {
                        operation.isMultipart = Boolean.TRUE;
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
