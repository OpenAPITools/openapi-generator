package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.HashMap;
import java.util.List;

public class KotlinKtorClientCodegen extends AbstractKotlinCodegen {

    // By default, generated code is considered public
    private boolean nonPublicApi = Boolean.FALSE;

    public KotlinKtorClientCodegen() {
        super();

        artifactId = "kotlin-ktor-client";
        packageName = "org.openapitools.client";

        outputFolder = "generated-code" + File.separator + "kotlin-ktor-client";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "kotlin-ktor-client";
        //These should already be taken care off by AbstractKotlinCodegen TODO: double-check
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        // Arrays need to map to List because kotlinx serialization doesn't support deserialization to arrays.
        typeMapping.put("array", "kotlin.collections.List");
        typeMapping.put("number", "kotlin.Double");
        //TODO: find a date library/solution for kotlin native
        typeMapping.put("date-time", "kotlin.String");
        typeMapping.put("date", "kotlin.String");
        typeMapping.put("Date", "kotlin.String");
        typeMapping.put("DateTime", "kotlin.String");
        typeMapping.put("ByteArray", "kotlin.ByteArray");

        //TODO: change to something else that works with kotlin native (without reflection)
        typeMapping.put("file", "kotlin.String");
        typeMapping.put("object", "JsonElement");

        importMapping = new HashMap<>();
        importMapping.put("JsonElement", "kotlinx.serialization.json.JsonElement");
        importMapping.put("URI", "kotlin.String");

        addSwitch(CodegenConstants.NON_PUBLIC_API, CodegenConstants.NON_PUBLIC_API_DESC, this.nonPublicApi);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "kotlin-ktor-client";
    }

    @Override
    public String getHelp() {
        return "Generates a Kotline-Native client using ktor for http requests and kotlinx serialization for models.";
    }

    @Override
    public void processOpts() {
        //Right now we can't handle other source folders because commonMain is used by the sourceSets
        additionalProperties.put(CodegenConstants.SOURCE_FOLDER, "src/commonMain/kotlin");

        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.NON_PUBLIC_API)) {
            nonPublicApi = convertPropertyToBooleanAndWriteBack(CodegenConstants.NON_PUBLIC_API);
        } else {
            additionalProperties.put(CodegenConstants.NON_PUBLIC_API, nonPublicApi);
        }

        //At the point of writing this plugin kotlinx serialization does not yet support enum names that aren't exact matches.
        enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", "/");
        supportingFiles.add(new SupportingFile("infrastructure/ErrorWrapper.kt.mustache", infrastructureFolder, "ErrorWrapper.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/HttpClientConvenience.kt.mustache", infrastructureFolder, "HttpClientConvenience.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/ByteArraySerializer.kt.mustache", infrastructureFolder, "ByteArraySerializer.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/AuthProvider.kt.mustache", infrastructureFolder+"/authentication", "AuthProvider.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/Base64.kt.mustache", infrastructureFolder+"/authentication", "Base64.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/HttpBasicAuth.kt.mustache", infrastructureFolder+"/authentication", "HttpBasicAuth.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/ApiKeyAuth.kt.mustache", infrastructureFolder+"/authentication", "ApiKeyAuth.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/OAuth.kt.mustache", infrastructureFolder+"/authentication", "OAuth.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/HttpBasicBearerAuth.kt.mustache", infrastructureFolder+"/authentication", "HttpBasicBearerAuth.kt"));

        //gradle files
        supportingFiles.add(new SupportingFile("travis.mustache", ".travis.yml"));
        supportingFiles.add(new SupportingFile("gradle/settings.gradle.mustache", "settings.gradle"));
        supportingFiles.add(new SupportingFile("gradle/build.gradle.mustache", "build.gradle"));
        supportingFiles.add(new SupportingFile("gradle/gradle.properties.mustache", "gradle.properties"));
        supportingFiles.add(new SupportingFile("gradle/gradlew.mustache", "gradlew"));
        supportingFiles.add(new SupportingFile("gradle/gradlew.bat.mustache", "gradlew.bat"));
        supportingFiles.add(new SupportingFile("gradle/gradle/wrapper/gradle-wrapper.jar", "gradle/wrapper", "gradle-wrapper.jar"));
        supportingFiles.add(new SupportingFile("gradle/gradle/wrapper/gradle-wrapper.properties", "gradle/wrapper", "gradle-wrapper.properties"));
    }

    @Override
    protected void updatePropertyForMap(CodegenProperty property, CodegenProperty innerProperty) {
        super.updatePropertyForMap(property, innerProperty);
        //Working around an issue where if a Map maps to Int, the baseType will falsely report Map instead of Int
        property.baseType = innerProperty.baseType;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        if (parameter.isListContainer) {
            parameter.baseType = parameter.items.dataType;
        }
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        path = escapeText(path);
        return super.fromOperation(path, httpMethod, operation, servers);
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return super.escapeUnsafeCharacters(input).replace("$", "${'$'}");
    }

    @Override
    public String toModelName(String name) {
        String prefixedName = super.toModelName(name);
        if (prefixedName.startsWith("kotlinx.")) {
            return name;
        } else {
            return prefixedName;
        }
    }
}
