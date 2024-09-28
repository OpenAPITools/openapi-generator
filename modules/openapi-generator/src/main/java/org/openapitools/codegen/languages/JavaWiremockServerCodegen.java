package org.openapitools.codegen.languages;

import static org.openapitools.codegen.utils.ModelUtils.getSchemaItems;

import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;

import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.templating.mustache.SplitStringLambda;
import org.openapitools.codegen.templating.mustache.TrimWhitespaceLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JavaWiremockServerCodegen extends AbstractJavaCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(JavaWiremockServerCodegen.class);

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "java-wiremock";
    }

    @Override
    public String getHelp() {
        return "Generates Java WireMock stubs, requests and responses samples.";
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        Schema<?> schema = unaliasSchema(p);
        Schema<?> target = ModelUtils.isGenerateAliasAsModel() ? p : schema;
        if (ModelUtils.isArraySchema(target)) {
            Schema<?> items = getSchemaItems(schema);
            String typeDeclaration = super.getTypeDeclarationForArray(items);
            return getSchemaType(target) + "<" + typeDeclaration + ">";
        } else if (ModelUtils.isMapSchema(target)) {
            // Note: ModelUtils.isMapSchema(p) returns true when p is a composed schema that also defines
            // additionalproperties: true
            Schema<?> inner = ModelUtils.getAdditionalProperties(target);
            if (inner == null) {
                LOGGER.error("`{}` (map property) does not have a proper inner type defined. Default to type:string", p.getName());
                inner = new StringSchema().description("TODO default missing map inner type to string");
                p.setAdditionalProperties(inner);
            }
            return getSchemaType(target) + "<String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(target);
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
