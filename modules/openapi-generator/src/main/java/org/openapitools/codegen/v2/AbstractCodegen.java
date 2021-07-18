package org.openapitools.codegen.v2;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.v2.outputs.CodegenOutputProcessor;
import org.openapitools.codegen.v2.templates.CodegenTemplateProcessor;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;

public abstract class AbstractCodegen implements Codegen {

    private static final CodegenTag OPTIONS = CodegenTag.of("codegen.options", CodegenOptions.class);
    private static final CodegenTag TIMESTAMP = CodegenTag.of("codegen.timestamp", Instant.class);

    private final CodegenTemplateProcessor templateProcessor;
    private final CodegenOutputProcessor outputProcessor;

    public AbstractCodegen(CodegenTemplateProcessor templateProcessor, CodegenOutputProcessor outputProcessor) {
        this.templateProcessor = Objects.requireNonNull(templateProcessor);
        this.outputProcessor = Objects.requireNonNull(outputProcessor);
    }

    @Override
    public final void generate(OpenAPI openAPI, CodegenOptions options) {
        onPreGenerate(openAPI, options);

        CodegenSdk sdk = buildCodegenSdk(openAPI, options);
        for (CodegenTagger tagger : getTaggers(options)) {
            CodegenObjectVisitor visitor = tagger::tag;
            visitor.visit(sdk);
        }

        templateProcessor.process(sdk);
        outputProcessor.process(sdk);
        onPostGenerate(openAPI, sdk, options);
    }

    protected void onPreGenerate(OpenAPI openAPI, CodegenOptions options) {
        // virtual no-op
    }

    protected void onPostGenerate(OpenAPI openAPI, CodegenSdk sdk, CodegenOptions options) {
        // virtual no-op
    }

    protected Collection<CodegenTagger> getTaggers(CodegenOptions options) {
        // virtual no-op
        return new ArrayList<>();
    }

    private CodegenSdk buildCodegenSdk(OpenAPI openAPI, CodegenOptions options) {
        CodegenSdk sdk = new CodegenSdk(options.getProject());
        sdk.setTag(OPTIONS, options);
        sdk.setTag(TIMESTAMP, Instant.now());

        // TODO build graph from OpenAPI spec

        return sdk;
    }
}
