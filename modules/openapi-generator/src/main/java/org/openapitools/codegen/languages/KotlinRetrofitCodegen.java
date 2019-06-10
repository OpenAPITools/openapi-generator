/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 * Copyright 2019 kroegerama
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

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.templating.mustache.CamelCaseLambda;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class KotlinRetrofitCodegen extends AbstractKotlinCodegen {

    private class EnumCaseLambda implements Mustache.Lambda {
        @Override
        public void execute(Template.Fragment fragment, Writer writer) throws IOException {
            String text = fragment.execute();
            text = camelize(text);
            text = toVarName(text);
            if (text.length() > 0) {
                text = text.substring(0, 1).toUpperCase(Locale.ROOT) + text.substring(1);
            }
            writer.write(text);
        }
    }

    public static final String DATE_LIBRARY = "dateLibrary";
    public static final String COLLECTION_TYPE = "collectionType";

    protected String dateLibrary = DateLibrary.STRING.value;
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
     * Constructs an instance of `KotlinRetrofitCodegen`.
     */
    public KotlinRetrofitCodegen() {
        super();

        artifactId = "kotlin-retrofit";
        packageName = "org.openapitools.client";

        sourceFolder = "src/main/java";

        // cliOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.artifactId);
        updateOption(CodegenConstants.PACKAGE_NAME, this.packageName);

        outputFolder = "generated-code" + File.separator + "kotlin-retrofit";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        embeddedTemplateDir = templateDir = "kotlin-retrofit";
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        CliOption dateLibrary = new CliOption(DATE_LIBRARY, "Option. Date library to use");
        Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(DateLibrary.THREETENBP.value, "Threetenbp");
        dateOptions.put(DateLibrary.STRING.value, "String");
        dateOptions.put(DateLibrary.JAVA8.value, "Java 8 native JSR310");
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

        additionalProperties.put("enumcase", new EnumCaseLambda());
        CamelCaseLambda camelCase = new CamelCaseLambda();
        camelCase.generator(this);
        camelCase.escapeAsParamName(true);
        additionalProperties.put("camelcase", camelCase);
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "kotlin-retrofit";
    }

    public String getHelp() {
        return "Generates an android library module. Uses Kotlin, Retrofit and Coroutines.";
    }

    public void setDateLibrary(String library) {
        this.dateLibrary = library;
    }

    public void setCollectionType(String collectionType) {
        this.collectionType = collectionType;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations == null) return objs;

        List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation operation : ops) {
            if (StringUtils.isNotEmpty(operation.path) && operation.path.startsWith("/")) {
                operation.path = operation.path.substring(1);
            }
        }
        return objs;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(DATE_LIBRARY)) {
            setDateLibrary(additionalProperties.get(DATE_LIBRARY).toString());
        }

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
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName).replace(".", "/");

        supportingFiles.add(new SupportingFile("AuthInterceptors.kt.mustache", infrastructureFolder, "AuthInterceptors.kt"));
        supportingFiles.add(new SupportingFile("RetrofitHolder.kt.mustache", infrastructureFolder, "RetrofitHolder.kt"));

        supportingFiles.add(new SupportingFile("proguard-rules.pro.mustache", "", "proguard-rules.pro"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        supportingFiles.add(new SupportingFile("AndroidManifest.xml.mustache", "src/main", "AndroidManifest.xml"));
    }
}
