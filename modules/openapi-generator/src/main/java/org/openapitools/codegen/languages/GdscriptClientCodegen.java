package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
//import io.swagger.models.properties.ArrayProperty;
//import io.swagger.models.properties.MapProperty;
//import io.swagger.models.properties.Property;
//import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.Arrays;
//import java.util.*;

//import org.apache.commons.lang3.StringUtils;

import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.templating.HandlebarsEngineAdapter;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class GdscriptClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    @SuppressWarnings("FieldCanBeLocal")
    private final Logger LOGGER = LoggerFactory.getLogger(GdscriptClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "gdscript";
    }

    public String getHelp() {
        return "Generates a GDScript client (Godot 4+).";
    }

    public GdscriptClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "gdscript";
        modelTemplateFiles.put("model.handlebars", ".gd");
        apiTemplateFiles.put("api.handlebars", ".gd");
        //apiTemplateFiles.put("api_base.handlebars", "plop.gd");

        embeddedTemplateDir = templateDir = "gdscript";
        apiPackage = "apis";
        modelPackage = "models";

        supportingFiles.add(new SupportingFile("ApiBee.handlebars", "core", "ApiBee.gd"));
        supportingFiles.add(new SupportingFile("ApiError.handlebars", "core", "ApiError.gd"));
        supportingFiles.add(new SupportingFile("README.handlebars", "", "README.md"));

        // Reserved keywords
        // https://github.com/godotengine/godot/blob/master/modules/gdscript/gdscript_tokenizer.cpp
        // FIXME
        setReservedWordsLowerCase(
                Arrays.asList(
                        // Local method names used in base API class
                        // FIXME: bzzzzz

                        // Local variable names used in API methods (endpoints)
                        // FIXME
//                        "all_params", "resource_path", "path_params", "query_params",
//                        "header_params", "form_params", "local_var_files", "body_params", "auth_settings",

                        // FIXME Global Scope
                        // https://github.com/godotengine/godot/blob/master/doc/classes/%40GlobalScope.xml

                        // @property
//                        "property",

                        // Tokens from GDScript
                        // https://github.com/godotengine/godot/blob/master/modules/gdscript/gdscript_tokenizer.cpp
                        // Logical
                        "and", "or", "not",
                        // Control flow
                        "if", "elif", "else", "for", "while", "break", "continue", "pass", "return", "match",
                        // Keywords
                        "as", "assert", "await", "breakpoint", "class", "class_name", "const", "enum", "extends",
                        "func", "in", "is", "namespace", "preload", "self", "signal", "static", "super", "trait",
                        "var", "void", "yield",
                        // Constants
                        "PI", "TAU", "INF", "NaN", // CONST_NAN,
                        // Special
                        "Color", // ERROR,

                        // Types
                        "float", "int", "String", "bool", "Dictionary", "Array", "Color"

                )
        );


        typeMapping.clear();
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "String");
        typeMapping.put("int", "int");
        typeMapping.put("float", "float");
        typeMapping.put("number", "float");
        typeMapping.put("long", "float");
        typeMapping.put("short", "float");
        typeMapping.put("char", "String");
        typeMapping.put("double", "int");
        typeMapping.put("object", "Object");
        typeMapping.put("integer", "int");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("set", "Array");
        typeMapping.put("date", "string");
        // FIXME: handle DateTime somehow
        //typeMapping.put("DateTime", "Date");
        //typeMapping.put("binary", "any");
        typeMapping.put("file", "File");
        typeMapping.put("ByteArray", "Array");
        typeMapping.put("UUID", "String");
        //typeMapping.put("Error", "Error");
        //typeMapping.put("AnyType", "Variant");


        // TODO: add meaningful parameters
        cliOptions.add(new CliOption(PROJECT_NAME, "The name of the project !!"));

        // This constructor is ran twice, because … reasons.
        LOGGER.warn("THIS GENERATOR IS UNSAFE AND MALICIOUS OAS3 YAML FILES MAY HURT YOU.");
        LOGGER.warn("PLEASE READ CAREFULLY THE OAS3 FILE YOU ARE USING BEFORE YOU TRUST IT.");
        LOGGER.info("(this generation itself should be safe, but not the generated code)");

        // TODO: Fill this out.

    }

    @Override
    public String defaultTemplatingEngine() {
        return "handlebars";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Ensure we're using the appropriate template engine, and configure it while we're at it.
        // We had to use handlebars because the truthy value of mustache includes `""` and `"null"`,
        // and things went south for default values and examples (but descriptions were OK, somehow)
        TemplatingEngineAdapter templatingEngine = getTemplatingEngine();
        if (templatingEngine instanceof HandlebarsEngineAdapter) {
            HandlebarsEngineAdapter handlebars = (HandlebarsEngineAdapter) templatingEngine;
            //handlebars.infiniteLoops(true); // will we want this eventually?
            handlebars.setPrettyPrint(true);  // removes blank lines from flow tags
        } else {
            throw new RuntimeException("Only the HandlebarsEngineAdapter is supported for this generator");
        }
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // There might be ways to inject code in Gdscript, but I don't see any for now.
        // TODO: review this with someone else
        return input;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // I've seen some other targets REMOVE the quotation marks altogether.
        // We might need to do that as well ?
        // TODO: review this with someone else
        return input
                .replace("\"", "\\\"")
                .replace("'", "\\'")
                ;
    }

    // When example is "null" (with quotes), mustache's {{#example}} triggers
    // Not sure why this happens on {{#example}} but not {{#description}}
    // Perhaps I'm just using mustache wrong…  Anyway, it's voodoo.
    // Also, even with this fix, {{#example}} still triggers.
    // → That just because of how mustache works. (false or [] only)
    // → I'll switch to handlebars.
    // → Pebble would perhaps help reduce injections, with custom filters
    // → Handlebars also has a discrepancy between description and example, both 'null'
    // → We need this (hot?)fix in the end.
    @Override
    public String toExampleValue(Schema schema) {
        if (schema.getExample() != null) {
            return super.toExampleValue(schema);
        }

        return "";
    }

    // → Same
    @Override
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            return schema.getDefault().toString();
        }

        return "";
    }
}
