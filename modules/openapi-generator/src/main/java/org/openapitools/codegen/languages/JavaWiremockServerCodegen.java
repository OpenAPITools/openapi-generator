package org.openapitools.codegen.languages;

import com.samskivert.mustache.Mustache;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;

import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.templating.mustache.SplitStringLambda;
import org.openapitools.codegen.templating.mustache.TrimWhitespaceLambda;

public class JavaWiremockServerCodegen extends AbstractJavaCodegen implements CodegenConfig {

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "java-wiremock";
    }

    public String getHelp() {
        return "Generates Java Wiremock stubs, requests and responses samples.";
    }

    public JavaWiremockServerCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "java";
        embeddedTemplateDir = templateDir = "java-wiremock";
        invokerPackage = "org.openapitools.mockserver";
        artifactId = "openapi-java-mockserver";
        apiPackage = "org.openapitools.mockserver.api";

        apiDocTemplateFiles = new HashMap<>();
        apiTestTemplateFiles = new HashMap<>();
        modelDocTemplateFiles = new HashMap<>();
        modelTemplateFiles = new HashMap<>();

        apiTemplateFiles.clear();
        apiTemplateFiles.put("wiremock.mustache", "MockServer.java");

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml")
                .doNotOverwrite());

        // Ensure the OAS 3.x discriminator mappings include any descendent schemas that allOf
        // inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values,
        // and the discriminator mapping schemas in the OAS document.
        this.setLegacyDiscriminatorBehavior(false);

        // add lambda for mustache templates
        additionalProperties.put("lambdaRemoveDoubleQuote", (Mustache.Lambda) (fragment, writer) -> writer
                .write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement(""))));
        additionalProperties.put("lambdaEscapeDoubleQuote", (Mustache.Lambda) (fragment, writer) -> writer
                .write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement("\\\""))));
        additionalProperties.put("lambdaRemoveLineBreak",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\\r|\\n", "")));

        additionalProperties.put("lambdaTrimWhitespace", new TrimWhitespaceLambda());

        additionalProperties.put("lambdaSplitString", new SplitStringLambda());
    }


}
