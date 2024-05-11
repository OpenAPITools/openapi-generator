package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;

import java.io.File;
import java.util.EnumSet;
import java.util.List;

public class KotlinWiremockServerCodegen extends AbstractKotlinCodegen {

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
        return "Generates Kotlin WireMock stubs, requests and responses samples.";
    }

    public KotlinWiremockServerCodegen() {
        super();

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

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());

        apiTemplateFiles.put("api.mustache", ".kt");
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
