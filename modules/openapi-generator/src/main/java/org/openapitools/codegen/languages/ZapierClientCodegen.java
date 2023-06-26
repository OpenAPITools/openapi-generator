package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.escape;

public class ZapierClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    private final Logger LOGGER = LoggerFactory.getLogger(ZapierClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "zapier";
    }

    public String getHelp() {
        return "Generates a zapier client.";
    }

    public ZapierClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "zapier";
        modelTemplateFiles.put("model.mustache", ".js");
        apiTemplateFiles.put("api.mustache", ".js");
        embeddedTemplateDir = templateDir = "zapier";
        apiPackage = "apis";
        modelPackage = "models";
        supportingFiles.add(new SupportingFile("actions.mustache", "operations", "actions.js"));
        supportingFiles.add(new SupportingFile("utils.mustache", "utils", "utils.js"));
        supportingFiles.add(new SupportingFile("index.mustache", "", "index.js"));
        supportingFiles.add(new SupportingFile("authentication.mustache", "", "authentication.js"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("number", "integer", "string", "boolean", "array", "file")
        );

        instantiationTypes.put("array", "array");
        instantiationTypes.put("set", "array");
        instantiationTypes.put("list", "array");
        instantiationTypes.put("map", "object");
        typeMapping = new HashMap<>();
        typeMapping.put("array", "array");
        typeMapping.put("set", "array");
        typeMapping.put("map", "object");
        typeMapping.put("List", "array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "integer");
        typeMapping.put("float", "number");
        typeMapping.put("number", "number");
        typeMapping.put("decimal", "number");
        typeMapping.put("DateTime", "string");
        typeMapping.put("date", "string");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("char", "string");
        typeMapping.put("double", "number");
        typeMapping.put("object", "object");
        typeMapping.put("integer", "integer");
        typeMapping.put("binary", "file");
        typeMapping.put("file", "file");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
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
    public String toModelImport(String name) {
        return "const " + name + " = " + "require('../" + modelPackage() + "/" + name + "');";
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (!needToImport(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        if (null == type) {
            LOGGER.error("No Type defined for Schema {}", p);
        }
        return toModelName(type);
    }

    @Override
    public String toModelFilename(String name) {
        return name;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return null; }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // do nothing as the output is just doc
        return input;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // do nothing as the output is just doc
        return input;
    }
}
