package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CliOption;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.regex.Matcher;

import org.apache.commons.lang.StringUtils;

public class PerlClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String MODULE_NAME = "moduleName";
    public static final String MODULE_VERSION = "moduleVersion";
    protected String moduleName = "WWW::SwaggerClient";
    protected String modulePathPart = moduleName.replaceAll("::", Matcher.quoteReplacement(File.separator));
    protected String moduleVersion = "1.0.0";

    protected static int emptyFunctionNameCounter = 0;

    public PerlClientCodegen() {
        super();
        modelPackage = File.separatorChar + "Object";
        outputFolder = "generated-code" + File.separatorChar + "perl";
        modelTemplateFiles.put("object.mustache", ".pm");
        apiTemplateFiles.put("api.mustache", ".pm");
        modelTestTemplateFiles.put("object_test.mustache", ".t");
        apiTestTemplateFiles.put("api_test.mustache", ".t");
        embeddedTemplateDir = templateDir = "perl";


        setReservedWordsLowerCase(
                Arrays.asList(
                        "else", "lock", "qw",
                        "__END__", "elsif", "lt", "qx",
                        "__FILE__", "eq", "m", "s",
                        "__LINE__", "exp", "ne", "sub",
                        "__PACKAGE__", "for", "no", "tr",
                        "and", "foreach", "or", "unless",
                        "cmp", "ge", "package", "until",
                        "continue", "gt", "q", "while",
                        "CORE", "if", "qq", "xor",
                        "do", "le", "qr", "y",
                        "return"
                )
        );

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("double");
        languageSpecificPrimitives.add("string");
        languageSpecificPrimitives.add("boolean");
        languageSpecificPrimitives.add("DateTime");
        languageSpecificPrimitives.add("ARRAY");
        languageSpecificPrimitives.add("HASH");
        languageSpecificPrimitives.add("object");

        typeMapping.clear();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");
        typeMapping.put("float", "double");
        typeMapping.put("double", "double");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("date", "DateTime");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("password", "string");
        typeMapping.put("array", "ARRAY");
        typeMapping.put("map", "HASH");
        typeMapping.put("object", "object");
        //TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "string");

        cliOptions.clear();
        cliOptions.add(new CliOption(MODULE_NAME, "Perl module name (convention: CamelCase or Long::Module).").defaultValue("SwaggerClient"));
        cliOptions.add(new CliOption(MODULE_VERSION, "Perl module version.").defaultValue("1.0.0"));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ENSURE_UNIQUE_PARAMS, CodegenConstants
                .ENSURE_UNIQUE_PARAMS_DESC).defaultValue(Boolean.TRUE.toString()));

    }


    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(MODULE_VERSION)) {
            setModuleVersion((String) additionalProperties.get(MODULE_VERSION));
        } else {
            additionalProperties.put(MODULE_VERSION, moduleVersion);
        }

        if (additionalProperties.containsKey(MODULE_NAME)) {
            setModuleName((String) additionalProperties.get(MODULE_NAME));
            setModulePathPart(moduleName.replaceAll("::", Matcher.quoteReplacement(File.separator)));
        } else {
            additionalProperties.put(MODULE_NAME, moduleName);
        }

        supportingFiles.add(new SupportingFile("ApiClient.mustache", ("lib/" + modulePathPart).replace('/', File.separatorChar), "ApiClient.pm"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", ("lib/" + modulePathPart).replace('/', File.separatorChar), "Configuration.pm"));
        supportingFiles.add(new SupportingFile("ApiFactory.mustache", ("lib/" + modulePathPart).replace('/', File.separatorChar), "ApiFactory.pm"));
        supportingFiles.add(new SupportingFile("Role.mustache", ("lib/" + modulePathPart).replace('/', File.separatorChar), "Role.pm"));
        supportingFiles.add(new SupportingFile("AutoDoc.mustache", ("lib/" + modulePathPart + "/Role").replace('/', File.separatorChar), "AutoDoc.pm"));
        supportingFiles.add(new SupportingFile("autodoc.script.mustache", "bin", "autodoc"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "perl";
    }

    @Override
    public String getHelp() {
        return "Generates a Perl client library.";
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + "/lib/" + modulePathPart + apiPackage()).replace('/', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + "/lib/" + modulePathPart + modelPackage()).replace('/', File.separatorChar);
    }


    @Override
    public String apiTestFileFolder() {
        return (outputFolder + "/t").replace('/', File.separatorChar);
    }

    @Override
    public String modelTestFileFolder() {
        return (outputFolder + "/t").replace('/', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "[string," + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        if (type == null) {
            return null;
        }
        return toModelName(type);
    }

    @Override
    public String toDefaultValue(Property p) {
        return "null";
    }


    @Override
    public String toVarName(String name) {
        // return the name in underscore style
        // PhoneNumber => phone_number
        name = underscore(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // parameter name starting with number won't compile
        // need to escape it by appending _ at the beginning
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }
        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // model name cannot use reserved keyword
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("object_" + name));
            name = "object_" + name;
        }

        // add prefix/suffic to model name
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelFilename(name) + "Test";
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiFilename(name) + "Test";
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. phone_number_api.rb => PhoneNumberApi.rb
        return camelize(name) + "Api";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return camelize(name) + "Api";
    }

    @Override
    public String toOperationId(String operationId) {
        //rename to empty_function_name_1 (e.g.) if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            operationId = underscore("empty_function_name_" + emptyFunctionNameCounter++);
            LOGGER.warn("Empty method name (operationId) found. Renamed to " + operationId);
            return operationId;
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + underscore("call_" + operationId));
            return underscore("call_" + operationId);
        }

        return underscore(operationId);
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public void setModulePathPart(String modulePathPart) {
        this.modulePathPart = modulePathPart;
    }

    public void setModuleVersion(String moduleVersion) {
        this.moduleVersion = moduleVersion;
    }
}
