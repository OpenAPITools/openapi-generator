package org.openapitools.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.openapitools.codegen.*;

import java.io.IOException;
import java.io.Writer;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class K6ClientClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "k6-client";

    private final Logger LOGGER = LoggerFactory.getLogger(K6ClientClientCodegen.class);

    private static final List<String> excludedImports = List
            .of(
                    "array",
                    "file",
                    "long",
                    "string",
                    "integer",
                    "map",
                    "any[]",
                    "DateTime"
            );

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "k6-client";
    }

    public String getHelp() {
        return "Generates a k6 API client.";
    }

    public K6ClientClientCodegen() {
        super();

        outputFolder = "";
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");

        embeddedTemplateDir = templateDir = "k6-client";
        apiPackage = "";
        modelPackage = "";

        // clean default includes
        defaultIncludes.clear();

        // define import mapping
        importMapping.clear();

        // define type mappings
        typeMapping.clear();

        typeMapping.put("array", "any[]");
        typeMapping.put("set", "any[]");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "number");
        typeMapping.put("float", "number");
        typeMapping.put("double", "number");
        typeMapping.put("number", "number");
        typeMapping.put("decimal", "number");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("integer", "number");
        typeMapping.put("UnsignedInteger", "number");
        typeMapping.put("UnsignedLong", "number");
        typeMapping.put("char", "string");
        typeMapping.put("object", "any");
        typeMapping.put("file", "FileData");
    }

    static class UrlLambda implements Mustache.Lambda {
        @Override
        public void execute(Template.Fragment fragment, Writer writer) throws IOException {
            String urlValue = fragment.execute();
            String changedValue = urlValue.replace("/{", "/${");
            writer.write(changedValue);
        }
    }

    static class BracesLambda implements Mustache.Lambda {
        @Override
        public void execute(Template.Fragment fragment, Writer writer) throws IOException {
            String urlValue = fragment.execute();
            writer.write("{" + urlValue + "}");
        }
    }

    static class LowerLambda implements Mustache.Lambda {
        @Override
        public void execute(Template.Fragment fragment, Writer writer) throws IOException {
            String urlValue = fragment.execute();
            writer.write(urlValue.toLowerCase(Locale.ROOT));
        }
    }

    @Override
    public ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("braces", new BracesLambda())
                .put("lower", new LowerLambda())
                .put("urlLambda", new UrlLambda());
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        // remove unused imports
        List<Map<String, String>> oldImports = objs.getImports();
        List<Map<String, String>> newImports = oldImports.stream()
                .filter(imp -> !excludedImports.contains(imp.get("classname")))
                .collect(Collectors.toList());

        objs.setImports(newImports);

        return objs;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        List<Map<String, String>> imports = objs.getImports();
        List<String> excludeImports = List
                .of(
                        "array",
                        "file",
                        "long",
                        "string",
                        "integer",
                        "boolean",
                        "map",
                        "any[]",
                        "ApiResponse",
                        "DateTime"
                );

        List<Map<String, String>> newImports = imports.stream()
                .filter(imp -> !excludeImports.contains(imp.get("import")))
                .collect(Collectors.toList());

        objs.setImports(newImports);

        return objs;
    }
}
