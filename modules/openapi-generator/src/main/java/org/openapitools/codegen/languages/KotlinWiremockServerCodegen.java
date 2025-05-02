package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.responses.ApiResponse;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;

import java.io.File;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class KotlinWiremockServerCodegen extends AbstractKotlinCodegen {

    protected static final String VENDOR_EXTENSION_BASE_NAME_LITERAL = "x-base-name-literal";

    protected static final String VENDOR_EXTENSION_IS_RANGE_RESPONSE_CODE = "x-is-range-code";

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "kotlin-wiremock";
    }

    @Override
    public String getHelp() {
        return "Generates Kotlin WireMock stub request and response samples.";
    }

    public KotlinWiremockServerCodegen() {
        super();

        embeddedTemplateDir = templateDir = "kotlin-wiremock";

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        setArtifactId("kotlin-wiremock");
        setApiPackage(packageName + ".apis");
        setModelPackage(packageName + ".models");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        modifyFeatureSet(features -> features
                .securityFeatures(EnumSet.allOf(SecurityFeature.class))
                .excludeWireFormatFeatures(
                        WireFormatFeature.XML,
                        WireFormatFeature.PROTOBUF
                )
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        typeMapping.put("array", "kotlin.collections.List");

        reservedWords.remove("ApiResponse");

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());

        apiTemplateFiles.put("api-stub.mustache", "Stubs.kt");
        apiTemplateFiles.put("api-stub-builder.mustache", "StubBuilders.kt");

        modelTemplateFiles.put("model.mustache", ".kt");

        supportingFiles.addAll(List.of(
                        new SupportingFile("build.gradle.kts.mustache", "", "build.gradle.kts"),
                        new SupportingFile("settings.gradle.kts.mustache", "", "settings.gradle.kts"),
                        new SupportingFile("libs.versions.toml.mustache", "gradle", "libs.versions.toml"),
                        new SupportingFile("gradlew.mustache", "", "gradlew"),
                        new SupportingFile("gradlew.bat.mustache", "", "gradlew.bat"),
                        new SupportingFile("gradle-wrapper.properties.mustache", "gradle" + File.separator + "wrapper", "gradle-wrapper.properties"),
                        new SupportingFile("gradle-wrapper.jar", "gradle" + File.separator + "wrapper", "gradle-wrapper.jar")
                )
        );

        supportingFiles.add(
                new SupportingFile(".gitignore.mustache", "", ".gitignore")
        );
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        var objects = super.postProcessModels(objs);

        for (ModelMap model : objects.getModels()) {
            var cm = model.getModel();
            var vars = Stream.of(
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
    public CodegenResponse fromResponse(String responseCode, ApiResponse response) {
        var r = super.fromResponse(responseCode, response);

        var isRange = List.of("1xx", "2xx", "3xx", "4xx", "5xx").contains(responseCode.toLowerCase(Locale.ROOT));
        r.vendorExtensions.put(VENDOR_EXTENSION_IS_RANGE_RESPONSE_CODE, isRange);
        return r;
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                              #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                     #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                              #");
        System.out.println("#                                                                                  #");
        System.out.println("# This generator's contributed by Stefan Koppier (https://github.com/stefankoppier)#");
        System.out.println("################################################################################");
    }

}
