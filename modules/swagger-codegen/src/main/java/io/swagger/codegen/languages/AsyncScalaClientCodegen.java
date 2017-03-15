package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;

public class AsyncScalaClientCodegen extends AbstractScalaCodegen implements CodegenConfig {
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-async-scala-client";
    protected String artifactVersion = "1.0.0";
    protected String clientName = "SwaggerClient";
    protected String authScheme = "";
    protected boolean authPreemptive;
    protected boolean asyncHttpClient = !authScheme.isEmpty();

    public AsyncScalaClientCodegen() {
        super();
        outputFolder = "generated-code/async-scala";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "asyncscala";
        apiPackage = "io.swagger.client.api";
        modelPackage = "io.swagger.client.model";

        setReservedWordsLowerCase(
                Arrays.asList(
                    // local variable names used in API methods (endpoints)
                    "config", "path", "contentTypes", "contentType", "queryParams", "headerParams",
                    "formParams", "postBody", "resFuture", "client", "reader",

                    // scala reserved words
                    "abstract", "case", "catch", "class", "def", "do", "else", "extends",
                    "false", "final", "finally", "for", "forSome", "if", "implicit",
                    "import", "lazy", "match", "new", "null", "object", "override", "package",
                    "private", "protected", "return", "sealed", "super", "this", "throw",
                    "trait", "try", "true", "type", "val", "var", "while", "with", "yield")
        );

        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        additionalProperties.put("asyncHttpClient", asyncHttpClient);
        additionalProperties.put("authScheme", authScheme);
        additionalProperties.put("authPreemptive", authPreemptive);
        additionalProperties.put("clientName", clientName);

        supportingFiles.add(new SupportingFile("sbt.mustache", "", "build.sbt"));
        supportingFiles.add(new SupportingFile("client.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator), clientName + ".scala"));

        importMapping.remove("List");
        importMapping.remove("Set");
        importMapping.remove("Map");

        importMapping.put("DateTime", "org.joda.time.DateTime");
        importMapping.put("ListBuffer", "scala.collection.mutable.ListBuffer");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("enum", "NSString");
        typeMapping.put("array", "List");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("byte", "Byte");
        typeMapping.put("short", "Short");
        typeMapping.put("char", "Char");
        typeMapping.put("long", "Long");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Any");
        typeMapping.put("file", "File");

        instantiationTypes.put("array", "ListBuffer");
        instantiationTypes.put("map", "HashMap");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "async-scala";
    }

    @Override
    public String getHelp() {
        return "Generates an Asynchronous Scala client library.";
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }
}
