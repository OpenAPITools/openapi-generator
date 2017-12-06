package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.File;
import java.util.*;
import java.io.Writer;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import io.swagger.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public class ErlangClientCodegen extends DefaultCodegen implements CodegenConfig {
    static Logger LOGGER = LoggerFactory.getLogger(ErlangClientCodegen.class);

    protected String packageName = "swagger";
    protected String packageVersion = "1.0.0";
    protected String sourceFolder = "src";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "erlang-client";
    }

    public String getHelp() {
        return "Generates an Erlang client library (beta).";
    }

    public ErlangClientCodegen() {
        super();
        outputFolder = "generated-code/erlang";
        modelTemplateFiles.put("model.mustache", ".erl");
        apiTemplateFiles.put("api.mustache", ".erl");

        embeddedTemplateDir = templateDir = "erlang-client";

        setReservedWordsLowerCase(
                Arrays.asList(
                    "after","and","andalso","band","begin","bnot","bor","bsl","bsr","bxor","case",
                    "catch","cond","div","end","fun","if","let","not","of","or","orelse","receive",
                    "rem","try","when","xor"
                    )
                );

        instantiationTypes.clear();

        typeMapping.clear();
        typeMapping.put("enum", "binary()");
        typeMapping.put("date", "calendar:date()");
        typeMapping.put("datetime", "calendar:datetime()");
        typeMapping.put("date-time", "calendar:datetime()");
        typeMapping.put("boolean", "boolean()");
        typeMapping.put("string", "binary()");
        typeMapping.put("integer", "integer()");
        typeMapping.put("int", "integer()");
        typeMapping.put("float", "integer()");
        typeMapping.put("long", "integer()");
        typeMapping.put("double", "float()");
        typeMapping.put("array", "list()");
        typeMapping.put("map", "maps:map()");
        typeMapping.put("number", "integer()");
        typeMapping.put("bigdecimal", "float()");
        typeMapping.put("List", "list()");
        typeMapping.put("object", "maps:map()");
        typeMapping.put("file", "binary()");
        typeMapping.put("binary", "binary()");
        typeMapping.put("bytearray", "binary()");
        typeMapping.put("byte", "binary()");
        typeMapping.put("uuid", "binary()");
        typeMapping.put("password", "binary()");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Erlang application name (convention: lowercase).")
                .defaultValue(this.packageName));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Erlang application version")
                .defaultValue(this.packageVersion));
    }

    @Override
    public String getTypeDeclaration(String name) {
        return name + ":" + name + "()";
    }

    @Override
    public String getTypeDeclaration(Schema propertySchema) {
        String swaggerType = getSchemaType(propertySchema);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }
        return swaggerType;
    }

    @Override
    public String getSchemaType(Schema propertySchema) {
        String swaggerType = super.getSchemaType(propertySchema);
        String type = null;
        if(typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if(languageSpecificPrimitives.contains(type))
                return (type);
        }
        else
            type = getTypeDeclaration(toModelName(snakeCase(swaggerType)));
        return type;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }
        else {
            setPackageName("swagger");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }
        else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("length", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                writer.write(length(fragment.context()));
            }
        });

        additionalProperties.put("qsEncode", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                writer.write(qsEncode(fragment.context()));
            }
        });

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("rebar.config.mustache","", "rebar.config"));
        supportingFiles.add(new SupportingFile("app.src.mustache", "", "src" + File.separator + this.packageName + ".app.src"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    public String qsEncode(Object o) {
        String r = new String();
        CodegenParameter q = (CodegenParameter) o;
        if (q.isListContainer) {
            r += "[{<<\"" + q.baseName + "\">>, X} || X <- " + q.paramName + "]";
        } else {
            r += "{<<\"" + q.baseName + "\">>, " + q.paramName + "}";
        }
        return r;
    }

    @Override
    public String escapeReservedWord(String name)
    {
        // Can't start with an underscore, as our fields need to start with an
        // UppercaseLetter so that Go treats them as public/visible.

        // Options?
        // - MyName
        // - AName
        // - TheName
        // - XName
        // - X_Name
        // ... or maybe a suffix?
        // - Name_ ... think this will work.
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return camelize(name) + '_';
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator;
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));
        // for reserved word or word starting with number, append _
        if (isReservedWord(name))
            name = escapeReservedWord(name);

        return name;
    }

    @Override
    public String toParamName(String name) {
        return camelize(toVarName(name));
    }

    @Override
    public String toModelName(String name) {
        return this.packageName + "_" + underscore(name.replaceAll("-", "_"));
    }

    @Override
    public String toApiName(String name) {
        return this.packageName + "_" + underscore(name.replaceAll("-", "_"));
    }

    @Override
    public String toModelFilename(String name) {
        return this.packageName + "_" + underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PetApi.erl => pet_api.erl
        return this.packageName + "_" + underscore(name) + "_api";
    }

    @Override
    public String toOperationId(String operationId) {
        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + underscore(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return underscore(operationId);
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> os = (List<CodegenOperation>) operations.get("operation");
        List<ExtendedCodegenOperation> newOs = new ArrayList<ExtendedCodegenOperation>();
        Pattern pattern = Pattern.compile("(.*)\\{([^\\}]+)\\}(.*)");
        for (CodegenOperation o : os) {
            // force http method to lower case
            o.httpMethod = o.httpMethod.toLowerCase();

            if (getBooleanValue(o, CodegenConstants.IS_LIST_CONTAINER_EXT_NAME)) {
                o.returnType = "[" + o.returnBaseType + "]";
            }

            ArrayList<String> pathTemplateNames = new ArrayList<String>();
            Matcher matcher = pattern.matcher(o.path);
            StringBuffer buffer = new StringBuffer();
            while (matcher.find()) {
                String pathTemplateName = matcher.group(2);
                matcher.appendReplacement(buffer, "$1" + "\", " + camelize(pathTemplateName) + ", \"" + "$3");
                pathTemplateNames.add(pathTemplateName);
            }
            ExtendedCodegenOperation eco = new ExtendedCodegenOperation(o);
            if (buffer.toString().isEmpty()) {
                eco.setReplacedPathName(o.path);
            } else {
                eco.setReplacedPathName(buffer.toString());
            }
            eco.setPathTemplateNames(pathTemplateNames);
            newOs.add(eco);
        }
        operations.put("operation", newOs);
        return objs;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    String length(Object o) {
        return Integer.toString((((ExtendedCodegenOperation) o).allParams).size());
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    class ExtendedCodegenOperation extends CodegenOperation {
        private List<String> pathTemplateNames = new ArrayList<String>();
        private String replacedPathName;

        public ExtendedCodegenOperation(CodegenOperation o) {
            super();

            // Copy all fields of CodegenOperation
            this.responseHeaders.addAll(o.responseHeaders);
            this.returnTypeIsPrimitive = o.returnTypeIsPrimitive;
            this.returnSimpleType = o.returnSimpleType;
            this.subresourceOperation = o.subresourceOperation;
            this.path = o.path;
            this.operationId = o.operationId;
            this.returnType = o.returnType;
            this.httpMethod = o.httpMethod;
            this.returnBaseType = o.returnBaseType;
            this.returnContainer = o.returnContainer;
            this.summary = o.summary;
            this.unescapedNotes = o.unescapedNotes;
            this.notes = o.notes;
            this.baseName = o.baseName;
            this.defaultResponse = o.defaultResponse;
            this.discriminator = o.discriminator;
            this.consumes = o.consumes;
            this.produces = o.produces;
            this.bodyParam = o.bodyParam;
            this.allParams = o.allParams;
            this.bodyParams = o.bodyParams;
            this.pathParams = o.pathParams;
            this.queryParams = o.queryParams;
            this.headerParams = o.headerParams;
            this.formParams = o.formParams;
            this.authMethods = o.authMethods;
            this.tags = o.tags;
            this.responses = o.responses;
            this.imports = o.imports;
            this.examples = o.examples;
            this.externalDocs = o.externalDocs;
            this.vendorExtensions = o.vendorExtensions;
            this.nickname = o.nickname;
            this.operationIdLowerCase = o.operationIdLowerCase;
            this.operationIdCamelCase = o.operationIdCamelCase;
        }

        public List<String> getPathTemplateNames() {
            return pathTemplateNames;
        }

        public void setPathTemplateNames(List<String> pathTemplateNames) {
            this.pathTemplateNames = pathTemplateNames;
        }

        public String getReplacedPathName() {
            return replacedPathName;
        }

        public void setReplacedPathName(String replacedPathName) {
            this.replacedPathName = replacedPathName;
        }
    }
}
