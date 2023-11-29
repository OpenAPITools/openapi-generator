package org.openapitools.codegen.v2.templates;

import org.openapitools.codegen.v2.*;
import org.openapitools.codegen.v2.reflection.GenericClass;
import org.openapitools.codegen.v2.templates.mustache.MustacheCodegenTemplateEngine;

import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class CodegenTemplateTagger extends AbstractCodegenTagger {

    public static class Builder {
        private CodegenTemplateEngine engine;
        private List<CodegenTemplate> sdkTemplates;
        private List<CodegenTemplate> apiTemplates;
        private List<CodegenTemplate> modelTemplates;

        public Builder() {
            try {
                Path resourcePath = Paths.get(ClassLoader.getSystemResource("").toURI());
                this.engine = new MustacheCodegenTemplateEngine(resourcePath);
            } catch (URISyntaxException e) {
                throw new RuntimeException(e);
            }
            this.sdkTemplates = new ArrayList<>();
            this.apiTemplates = new ArrayList<>();
            this.modelTemplates = new ArrayList<>();
        }

        public Builder withEngine(CodegenTemplateEngine engine) {
            this.engine = Objects.requireNonNull(engine);
            return this;
        }

        public Builder withSdkTemplate(String templateFile, String generatedName) {
            sdkTemplates.add(new CodegenTemplate(engine, templateFile, generatedName));
            return this;
        }

        public Builder withApiTemplate(String templateFile, String generatedName) {
            apiTemplates.add(new CodegenTemplate(engine, templateFile, generatedName));
            return this;
        }

        public Builder withModelTemplate(String templateFile, String generatedName) {
            modelTemplates.add(new CodegenTemplate(engine, templateFile, generatedName));
            return this;
        }

        public CodegenTagger build() {
            CodegenTagger tagger = new CodegenTemplateTagger(sdkTemplates, apiTemplates, modelTemplates);
            sdkTemplates = new ArrayList<>();
            apiTemplates = new ArrayList<>();
            modelTemplates = new ArrayList<>();
            return tagger;
        }
    }

    private final Collection<CodegenTemplate> sdkTemplates;
    private final Collection<CodegenTemplate> apiTemplates;
    private final Collection<CodegenTemplate> modelTemplates;

    private CodegenTemplateTagger(Collection<CodegenTemplate> sdkTemplates, Collection<CodegenTemplate> apiTemplates, Collection<CodegenTemplate> modelTemplates) {
        this.sdkTemplates = sdkTemplates;
        this.apiTemplates = apiTemplates;
        this.modelTemplates = modelTemplates;
    }

    @Override
    protected void tag(CodegenSdk sdk) {
        addTemplatesToObject(sdkTemplates, sdk);
    }

    @Override
    protected void tag(CodegenApi api) {
        addTemplatesToObject(apiTemplates, api);
    }

    @Override
    protected void tag(CodegenModel model) {
        addTemplatesToObject(modelTemplates, model);
    }

    private void addTemplatesToObject(Collection<CodegenTemplate> templates, CodegenObject object) {
        Collection<CodegenTemplate> objectTemplates = object
                .getTagOrDefault(CodegenTemplateTags.TEMPLATES, new GenericClass<Collection<CodegenTemplate>>() {}, ArrayList::new);
        objectTemplates.addAll(templates.stream()
                .map(x -> new CodegenTemplate(x.getEngine(), x.getTemplateFile(), object.getId() + x.getGeneratedName()))
                .collect(Collectors.toList()));
    }
}
