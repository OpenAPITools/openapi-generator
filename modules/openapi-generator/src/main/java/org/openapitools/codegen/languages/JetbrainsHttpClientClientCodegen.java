package org.openapitools.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.openapitools.codegen.*;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JetbrainsHttpClientClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "Jetbrains HTTP Client";

    static final Logger LOGGER = LoggerFactory.getLogger(JetbrainsHttpClientClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "jetbrains-http-client";
    }

    public String getHelp() {
        return "Generates a jetbrains-http client. See https://www.jetbrains.com/help/idea/http-client-in-product-code-editor.html";
    }

    public JetbrainsHttpClientClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "jetbrains-http-client";
//        modelTemplateFiles.put("model.mustache", ".http");
        apiTemplateFiles.put("api.mustache", ".http");
        embeddedTemplateDir = templateDir = "jetbrains-http-client";
        apiPackage = "Apis";
//        modelPackage = "Models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("http-client.env.json.mustache", apiPackage, "http-client.env.json"));
    }

    @Override
    protected ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas() {

        return super.addMustacheLambdas()
                .put("doubleMustache", new DoubleMustacheLambda());
    }

    public static class DoubleMustacheLambda implements Mustache.Lambda {
        @Override
        public void execute(Template.Fragment fragment, Writer writer) throws IOException {
            String text = fragment.execute();
            writer.write(text
                    .replaceAll("\\{", "{{")
                    .replaceAll("}", "}}")
            );
        }
    }
}
