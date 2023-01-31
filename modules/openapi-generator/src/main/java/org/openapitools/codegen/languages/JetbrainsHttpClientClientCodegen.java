package org.openapitools.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.openapitools.codegen.*;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ApiInfoMap;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JetbrainsHttpClientClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "Jetbrains HTTP Client";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "jetbrains-http-client";
    }

    public String getHelp() {
        return "Generates a jetbrains-http client. See https://www.jetbrains.com/help/idea/http-client-in-product-code-editor.html";
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.HTTP;
    }

    public JetbrainsHttpClientClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();

        outputFolder = "generated-code" + File.separator + "jetbrains-http-client";
        apiTemplateFiles.put("api.mustache", ".http");
        embeddedTemplateDir = templateDir = "jetbrains-http-client";
        apiPackage = "Apis";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
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

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> bundle) {

        return bundle;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        return super.postProcessOperationsWithModels(objs, allModels);
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# This generator was written by Julien Lengrand-Lambert (https://github.com/jlengrand)    #");
        System.out.println("################################################################################");
    }
}
