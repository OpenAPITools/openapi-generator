package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.codegen.utils.ModelUtils;
import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.Operation;
import io.swagger.oas.models.info.Info;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.BooleanSchema;
import io.swagger.oas.models.media.ByteArraySchema;
import io.swagger.oas.models.media.DateSchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.EmailSchema;
import io.swagger.oas.models.media.FileSchema;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.NumberSchema;
import io.swagger.oas.models.media.ObjectSchema;
import io.swagger.oas.models.media.PasswordSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.oas.models.media.UUIDSchema;
import io.swagger.oas.models.parameters.Parameter;
import io.swagger.oas.models.responses.ApiResponse;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class ApexClientCodegen extends AbstractJavaCodegen {

    private static final String CLASS_PREFIX = "classPrefix";
    private static final String API_VERSION = "apiVersion";
    private static final String BUILD_METHOD = "buildMethod";
    private static final String NAMED_CREDENTIAL = "namedCredential";
    private static final Logger LOGGER = LoggerFactory.getLogger(ApexClientCodegen.class);
    private String classPrefix = "Swag";
    private String apiVersion = "39.0";
    private String buildMethod = "sfdx";
    private String namedCredential = classPrefix;
    private String srcPath = "force-app/main/default/";

    public ApexClientCodegen() {
        super();

        importMapping.clear();

        testFolder = sourceFolder = srcPath;

        embeddedTemplateDir = templateDir = "apex";
        outputFolder = "generated-code" + File.separator + "apex";
        apiPackage = "classes";
        modelPackage = "classes";
        testPackage = "force-app.main.default.classes";
        modelNamePrefix = classPrefix;
        dateLibrary = "";

        apiTemplateFiles.put("api.mustache", ".cls");
        apiTemplateFiles.put("cls-meta.mustache", ".cls-meta.xml");
        apiTestTemplateFiles.put("api_test.mustache", ".cls");
        apiTestTemplateFiles.put("cls-meta.mustache", ".cls-meta.xml");
        modelTemplateFiles.put("model.mustache", ".cls");
        modelTemplateFiles.put("cls-meta.mustache", ".cls-meta.xml");
        modelTestTemplateFiles.put("model_test.mustache", ".cls");
        modelTestTemplateFiles.put("cls-meta.mustache", ".cls-meta.xml");

        cliOptions.add(CliOption.newString(CLASS_PREFIX, "Prefix for generated classes. Set this to avoid overwriting existing classes in your org."));
        cliOptions.add(CliOption.newString(API_VERSION, "The Metadata API version number to use for components in this package."));
        cliOptions.add(CliOption.newString(BUILD_METHOD, "The build method for this package."));
        cliOptions.add(CliOption.newString(NAMED_CREDENTIAL, "The named credential name for the HTTP callouts"));

        supportingFiles.add(new SupportingFile("Swagger.cls", srcPath + "classes", "Swagger.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes", "Swagger.cls-meta.xml"));
        supportingFiles.add(new SupportingFile("SwaggerTest.cls", srcPath + "classes", "SwaggerTest.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes", "SwaggerTest.cls-meta.xml"));
        supportingFiles.add(new SupportingFile("SwaggerResponseMock.cls", srcPath + "classes", "SwaggerResponseMock.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes", "SwaggerResponseMock.cls-meta.xml"));

        typeMapping.put("BigDecimal", "Double");
        typeMapping.put("binary", "String");
        typeMapping.put("ByteArray", "Blob");
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "Datetime");
        typeMapping.put("file", "Blob");
        typeMapping.put("float", "Double");
        typeMapping.put("number", "Double");
        typeMapping.put("short", "Integer");
        typeMapping.put("UUID", "String");

        setReservedWordsLowerCase(
            Arrays.asList("abstract", "activate", "and", "any", "array", "as", "asc", "autonomous",
                "begin", "bigdecimal", "blob", "break", "bulk", "by", "byte", "case", "cast",
                "catch", "char", "class", "collect", "commit", "const", "continue",
                "convertcurrency", "date", "decimal", "default", "delete", "desc", "do", "else",
                "end", "enum", "exception", "exit", "export", "extends", "false", "final",
                "finally", "float", "for", "from", "future", "global", "goto", "group", "having",
                "hint", "if", "implements", "import", "inner", "insert", "instanceof", "int",
                "interface", "into", "join", "last_90_days", "last_month", "last_n_days",
                "last_week", "like", "limit", "list", "long", "loop", "map", "merge", "new",
                "next_90_days", "next_month", "next_n_days", "next_week", "not", "null", "nulls",
                "number", "object", "of", "on", "or", "outer", "override", "package", "parallel",
                "pragma", "private", "protected", "public", "retrieve", "return", "returning",
                "rollback", "savepoint", "search", "select", "set", "short", "sort", "stat",
                "static", "super", "switch", "synchronized", "system", "testmethod", "then", "this",
                "this_month", "this_week", "throw", "today", "tolabel", "tomorrow", "transaction",
                "trigger", "true", "try", "type", "undelete", "update", "upsert", "using",
                "virtual", "webservice", "when", "where", "while", "yesterday"
            ));

        languageSpecificPrimitives = new HashSet<String>(
            Arrays.asList("Blob", "Boolean", "Date", "Datetime", "Decimal", "Double", "ID",
                "Integer", "Long", "Object", "String", "Time"
            ));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CLASS_PREFIX)) {
            setClassPrefix((String) additionalProperties.get(CLASS_PREFIX));
        }
        additionalProperties.put(CLASS_PREFIX, classPrefix);

        if (additionalProperties.containsKey(API_VERSION)) {
            setApiVersion(toApiVersion((String) additionalProperties.get(API_VERSION)));
        }
        additionalProperties.put(API_VERSION, apiVersion);

        if (additionalProperties.containsKey(BUILD_METHOD)) {
            setBuildMethod((String)additionalProperties.get(BUILD_METHOD));
        }
        additionalProperties.put(BUILD_METHOD, buildMethod);

        if (additionalProperties.containsKey(NAMED_CREDENTIAL)) {
            setNamedCredential((String)additionalProperties.get(NAMED_CREDENTIAL));
        }
        additionalProperties.put(NAMED_CREDENTIAL, namedCredential);

        postProcessOpts();
    }

    @Override
    public String escapeReservedWord(String name) {
        // Identifiers must start with a letter
        return "r" + super.escapeReservedWord(name);
    }

    @Override
    public String toModelName(String name) {
        String modelName = super.toModelName(name);

        // Max length is 40; save the last 4 for "Test"
        if (modelName.length() > 36) {
            modelName = modelName.substring(0, 36);
        }
        return modelName;
    }

    @Override
    public String toDefaultValue(Schema propertySchema) {
        String out = null;
        if (propertySchema instanceof ArraySchema) {
            Schema inner = ((ArraySchema) propertySchema).getItems();
            out = String.format(
                "new List<%s>()",
                inner == null ? "Object" : getTypeDeclaration(inner)
            );
        } else if (propertySchema instanceof BooleanSchema) {
            // true => "true", false => "false", null => "null"
            out = String.valueOf(((BooleanSchema) propertySchema).getDefault());
        } else if (propertySchema instanceof IntegerSchema && SchemaTypeUtil.INTEGER64_FORMAT.equals(propertySchema.getFormat())) {
            Long def = propertySchema.getDefault() != null ? Long.valueOf(propertySchema.getDefault().toString()) : null;
            out = def == null ? out : def.toString() + "L";
        } else if (propertySchema instanceof MapSchema) {
            Schema inner = propertySchema.getAdditionalProperties();
            String s = inner == null ? "Object" : getTypeDeclaration(inner);
            out = String.format("new Map<String, %s>()", s);
        } else if (propertySchema instanceof StringSchema) {
            StringSchema schema = (StringSchema) propertySchema;
            String def = schema.getDefault();
            if (def != null) {
                out = schema.getEnum() == null ? String.format("'%s'", escapeText(def)) : def;
            }
        } else {
            out = super.toDefaultValue(propertySchema);
        }

        // we'll skip over null defaults in the model template to avoid redundant initialization
        return "null".equals(out) ? null : out;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        if (Boolean.TRUE.equals(p.isLong)) {
            p.example = "2147483648L";
        } else if (Boolean.TRUE.equals(p.isFile)) {
            p.example = "Blob.valueOf('Sample text file\\nContents')";
        } else if (Boolean.TRUE.equals(p.isDate)) {
            p.example = "Date.newInstance(1960, 2, 17)";
        } else if (Boolean.TRUE.equals(p.isDateTime)) {
            p.example = "Datetime.newInstanceGmt(2013, 11, 12, 3, 3, 3)";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            p.example = "new " + p.dataType + "{" + p.items.example + "}";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            p.example = "new " + p.dataType + "{" + p.items.example + "}";
        } else if (Boolean.TRUE.equals(p.isString)) {
            p.example = "'" + p.example + "'";
        } else if ("".equals(p.example) || p.example == null) {
            // Get an example object from the generated model
            p.example = p.dataType + ".getExample()";
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema, Map<String, Schema> allSchemas) {
        CodegenModel cm = super.fromModel(name, schema, allSchemas);
        if (cm.interfaces == null) {
            cm.interfaces = new ArrayList<String>();
        }

        Boolean hasDefaultValues = false;

        // for (de)serializing properties renamed for Apex (e.g. reserved words)
        List<Map<String, String>> propertyMappings = new ArrayList<>();
        for (CodegenProperty p : cm.allVars) {
            hasDefaultValues |= p.defaultValue != null;
            if (!p.baseName.equals(p.name)) {
                Map<String, String> mapping = new HashMap<>();
                mapping.put("externalName", p.baseName);
                mapping.put("internalName", p.name);
                propertyMappings.add(mapping);
            }
        }

        if (cm.vendorExtensions == null) {
            cm.vendorExtensions = new HashMap<>();
        }

        cm.vendorExtensions.put("hasPropertyMappings", !propertyMappings.isEmpty());
        cm.vendorExtensions.put("hasDefaultValues", hasDefaultValues);
        cm.vendorExtensions.put("propertyMappings", propertyMappings);

        if (!propertyMappings.isEmpty()) {
            cm.interfaces.add("Swagger.MappedProperties");
        }
        return cm;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        if (parameter.isBodyParam && parameter.isListContainer) {
            // items of array bodyParams are being nested an extra level too deep for some reason
            parameter.items = parameter.items.items;
            setParameterExampleValue(parameter);
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        Info info = openAPI.getInfo();
        String calloutLabel = info.getTitle();
        additionalProperties.put("calloutLabel", calloutLabel);
        String sanitized = sanitizeName(calloutLabel);
        additionalProperties.put("calloutName", sanitized);
        supportingFiles.add(new SupportingFile("namedCredential.mustache", srcPath + "/namedCredentials",
            sanitized + ".namedCredential"
        ));

        if (additionalProperties.get(BUILD_METHOD).equals("sfdx")) {
            generateSfdxSupportingFiles();
        } else if (additionalProperties.get(BUILD_METHOD).equals("ant")) {
            generateAntSupportingFiles();
        }

    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Schema> definitions, OpenAPI openAPI) {
        CodegenOperation op = super.fromOperation(
            path, httpMethod, operation, definitions, openAPI);
        if (op.getHasExamples()) {
            // prepare examples for Apex test classes
            ApiResponse responseProperty = findMethodResponse(operation.getResponses());
            String deserializedExample = toExampleValue(getSchemaFromResponse(responseProperty));
            for (Map<String, String> example : op.examples) {
                example.put("example", escapeText(example.get("example")));
                example.put("deserializedExample", deserializedExample);
            }
        }
        return op;
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("'", "\\'");
    }

    public void setBuildMethod(String buildMethod) {
        if (buildMethod.equals("ant")) {
            this.srcPath = "deploy/";
        } else {
            this.srcPath = "src/";
        }
        testFolder = sourceFolder = srcPath;
        this.buildMethod = buildMethod;
    }

    public void setNamedCredential(String namedCredential) {
        this.namedCredential = namedCredential;
    }

    public void setClassPrefix(String classPrefix) {
        // the best thing we can do without namespacing in Apex
        modelNamePrefix = classPrefix;
        this.classPrefix = classPrefix;
    }

    public void setApiVersion(String apiVersion) {
        this.apiVersion = apiVersion;
    }

    private String toApiVersion(String apiVersion) {
        if (apiVersion.matches("^\\d{2}(\\.0)?$")) {
            return apiVersion.substring(0, 2) + ".0";
        } else {
            LOGGER.warn(String.format("specified API version is invalid: %s - defaulting to %s", apiVersion, this.apiVersion));
            return this.apiVersion;
        }
    }

    private void postProcessOpts() {
        supportingFiles.add(
            new SupportingFile("client.mustache", srcPath + "classes", classPrefix + "Client.cls"));
        supportingFiles.add(new SupportingFile("cls-meta.mustache", srcPath + "classes",
            classPrefix + "Client.cls-meta.xml"
        ));
    }

    @Override
    public String escapeText(String input) {
        if (input == null) {
            return input;
        }

        return input.replace("'", "\\'").replace("\n", "\\n").replace("\r", "\\r");
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelName(name) + "Test";
    }

    @Override
    public String toExampleValue(Schema propertySchema) {
        if (propertySchema == null) {
            return StringUtils.EMPTY;
        }
        Object obj = propertySchema.getExample();
        String example = obj == null ? StringUtils.EMPTY : obj.toString();
        if (propertySchema instanceof ArraySchema) {
            example = String.format("new %s {%s}", getTypeDeclaration(propertySchema),  toExampleValue(((ArraySchema) propertySchema).getItems()));
        } else if (propertySchema instanceof BooleanSchema) {
            example = String.valueOf(!"false".equals(example));
        } else if (propertySchema instanceof ByteArraySchema) {
            if (example.isEmpty()) {
                example = "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wZWQgb3ZlciB0aGUgbGF6eSBkb2cu";
            }
            propertySchema.setExample(example);
            example = String.format("EncodingUtil.base64Decode('%s')", example);
        } else if (propertySchema instanceof DateSchema) {
            if (example.matches("^\\d{4}(-\\d{2}){2}")) {
                example = example.substring(0, 10).replaceAll("-0?", ", ");
            } else if (example.isEmpty()) {
                example = "2000, 1, 23";
            } else {
                LOGGER.warn(String.format("The example provided for property '%s' is not a valid RFC3339 date. Defaulting to '2000-01-23'. [%s]", propertySchema
                    .getName(), example));
                example = "2000, 1, 23";
            }
            example = String.format("Date.newInstance(%s)", example);
        } else if (propertySchema instanceof DateTimeSchema) {
            if (example.matches("^\\d{4}([-T:]\\d{2}){5}.+")) {
                example = example.substring(0, 19).replaceAll("[-T:]0?", ", ");
            } else if (example.isEmpty()) {
                example = "2000, 1, 23, 4, 56, 7";
            } else {
                LOGGER.warn(String.format("The example provided for property '%s' is not a valid RFC3339 datetime. Defaulting to '2000-01-23T04-56-07Z'. [%s]", propertySchema
                    .getName(), example));
                example = "2000, 1, 23, 4, 56, 7";
            }
            example = String.format("Datetime.newInstanceGmt(%s)", example);
        } else if (propertySchema instanceof NumberSchema) {
            example = example.replaceAll("[^-0-9.]", "");
            example = example.isEmpty() ? "1.3579" : example;
        } else if (propertySchema instanceof FileSchema) {
            if (example.isEmpty()) {
                example = "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wZWQgb3ZlciB0aGUgbGF6eSBkb2cu";
                propertySchema.setExample(example);
            }
            example = String.format("EncodingUtil.base64Decode(%s)", example);
        } else if (propertySchema instanceof EmailSchema) {
            if (example.isEmpty()) {
                example = "example@example.com";
                ((EmailSchema) propertySchema).setExample(example);
            }
            example = "'" + example + "'";
        } else if (propertySchema instanceof IntegerSchema && SchemaTypeUtil.INTEGER64_FORMAT.equals(propertySchema.getFormat())) {
            example = example.isEmpty() ? "123456789L" : example + "L";
        } else if (propertySchema instanceof MapSchema) {
            example = String.format("new %s {'key'=>%s}", getTypeDeclaration(propertySchema), toExampleValue(propertySchema.getAdditionalProperties()));
        } else if (propertySchema instanceof ObjectSchema) {
            example = example.isEmpty() ? "null" : example;
        } else if (propertySchema instanceof PasswordSchema) {
            example = example.isEmpty() ? "password123" : escapeText(example);
            propertySchema.setExample(example);
            example = String.format("'&s'", example);
        } else if (StringUtils.isNotBlank(propertySchema.get$ref()) ) {
            example = getTypeDeclaration(propertySchema) + ".getExample()";
        } else if (propertySchema instanceof StringSchema) {
            List<String> enums = propertySchema.getEnum();
            if (enums != null && example.isEmpty()) {
                example = enums.get(0);
                propertySchema.setExample(example);
            } else if (example.isEmpty()) {
                example = "aeiou";
            } else {
                example = escapeText(example);
                propertySchema.setExample(example);
            }
            example = "'" + example + "'";
        } else if (propertySchema instanceof UUIDSchema) {
            example = example.isEmpty()
                ? "'046b6c7f-0b8a-43b9-b35d-6489e6daee91'"
                : String.format("'&s'", escapeText(example));
        } else if (propertySchema instanceof IntegerSchema) {
            example = example.matches("^-?\\d+$") ? example : "123";
        }

        return example;
    }

    @Override
    public String toApiName(String name) {
        return camelize(classPrefix + super.toApiName(name));
    }

    public void updateCodegenPropertyEnum(CodegenProperty var) {
        ModelUtils.updateCodegenPropertyEnum(var);
        if (var.isEnum && var.example != null) {
            String example = var.example.replace("'", "");
            example = toEnumVarName(example, var.datatype);
            var.example = toEnumDefaultValue(example, var.datatypeWithEnum);
        }
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "apex";
    }

    @Override
    public String getHelp() {
        return "Generates an Apex API client library (beta).";
    }

    private void generateAntSupportingFiles() {

        supportingFiles.add(new SupportingFile("package.mustache", "deploy", "package.xml"));
        supportingFiles.add(new SupportingFile("package.mustache", "undeploy", "destructiveChanges.xml"));
        supportingFiles.add(new SupportingFile("build.mustache", "build.xml"));
        supportingFiles.add(new SupportingFile("build.properties", "build.properties"));
        supportingFiles.add(new SupportingFile("remove.package.mustache", "undeploy", "package.xml"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        writeOptional(outputFolder, new SupportingFile("README_ant.mustache", "README.md"));

    }

    private void generateSfdxSupportingFiles() {

        supportingFiles.add(new SupportingFile("sfdx.mustache", "", "sfdx-oss-manifest.json"));

        writeOptional(outputFolder, new SupportingFile("README_sfdx.mustache", "README.md"));

    }


}
