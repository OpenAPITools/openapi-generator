package org.openapitools.codegen.v2.templates;

import java.util.Objects;

public class CodegenTemplate {

    private final CodegenTemplateEngine engine;
    private final String templateFile;
    private final String generatedName;

    public CodegenTemplate(CodegenTemplateEngine engine, String templateFile, String generatedName) {
        this.engine = Objects.requireNonNull(engine);
        this.templateFile = Objects.requireNonNull(templateFile);
        this.generatedName = Objects.requireNonNull(generatedName);
    }

    public CodegenTemplateEngine getEngine() {
        return engine;
    }

    public String getTemplateFile() {
        return templateFile;
    }

    public String getGeneratedName() {
        return generatedName;
    }
}
