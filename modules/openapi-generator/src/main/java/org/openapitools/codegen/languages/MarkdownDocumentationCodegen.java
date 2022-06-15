package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

import static org.openapitools.codegen.utils.StringUtils.escape;

public class MarkdownDocumentationCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    private final Logger LOGGER = LoggerFactory.getLogger(MarkdownDocumentationCodegen.class);

    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    public String getName() {
        return "markdown";
    }

    public String getHelp() {
        return "Generates a markdown documentation.";
    }

    public MarkdownDocumentationCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "markdown";
        modelTemplateFiles.put("model.mustache", ".md");
        apiTemplateFiles.put("api.mustache", ".md");
        embeddedTemplateDir = templateDir = "markdown-documentation";
        apiPackage = File.separator + "Apis";
        modelPackage = "Models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("ByteArray");
        languageSpecificPrimitives.add("DateTime");
        languageSpecificPrimitives.add("URI");
        languageSpecificPrimitives.add("UUID");
        languageSpecificPrimitives.add("boolean");
        languageSpecificPrimitives.add("char");
        languageSpecificPrimitives.add("date");
        languageSpecificPrimitives.add("decimal");
        languageSpecificPrimitives.add("double");
        languageSpecificPrimitives.add("file");
        languageSpecificPrimitives.add("float");
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("integer");
        languageSpecificPrimitives.add("long");
        languageSpecificPrimitives.add("number");
        languageSpecificPrimitives.add("object");
        languageSpecificPrimitives.add("short");
        languageSpecificPrimitives.add("string");

        // TODO: Fill this out.
    }

    @Override
    protected void initializeSpecialCharacterMapping() {
        // escape only those symbols that can mess up markdown
        specialCharReplacements.put("\\", "\\\\");
        specialCharReplacements.put("/", "\\/");
        specialCharReplacements.put("`", "\\`");
        specialCharReplacements.put("*", "\\*");
        specialCharReplacements.put("_", "\\_");
        specialCharReplacements.put("[", "\\[");
        specialCharReplacements.put("]", "\\]");

        // todo Current markdown api and model mustache templates display properties and parameters in tables. Pipe
        //  symbol in a table can be commonly escaped with a backslash (e.g. GFM supports this). However, in some cases
        //  it may be necessary to choose a different approach.
        specialCharReplacements.put("|", "\\|");
    }

    /**
     * Works identically to {@link DefaultCodegen#toParamName(String)} but doesn't camelize.
     *
     * @param name Codegen property object
     * @return the sanitized parameter name
     */
    @Override
    public String toParamName(String name) {
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        } else if (((CharSequence) name).chars().anyMatch(character -> specialCharReplacements.keySet().contains(String.valueOf((char) character)))) {
            return escape(name, specialCharReplacements, null, null);
        }
        return name;
    }

    @Override
    public String toModelName(final String name) {
        return name;
    }

    @Override
    public String toModelFilename(String name) {
        return name;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return null; }
}
