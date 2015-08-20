package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.codegen.CliOption;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;

import org.apache.commons.lang.StringUtils;

public class PerlClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected String moduleName = "SwaggerClient";
    protected String moduleVersion = "1.0.0";

    public PerlClientCodegen() {
        super();
        modelPackage = File.separatorChar + "Object";
        outputFolder = "generated-code" + File.separatorChar + "perl";
        modelTemplateFiles.put("object.mustache", ".pm");
        apiTemplateFiles.put("api.mustache", ".pm");
        templateDir = "perl";


        reservedWords = new HashSet<String>(
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
                        "do", "le", "qr", "y"
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
        typeMapping.put("dateTime", "DateTime");
        typeMapping.put("password", "string");
        typeMapping.put("array", "ARRAY");
        typeMapping.put("map", "HASH");
        typeMapping.put("object", "object");

        cliOptions.clear();
        cliOptions.add(new CliOption("moduleName", "perl module name (convention: CamelCase), default: SwaggerClient"));
        cliOptions.add(new CliOption("moduleVersion", "perl module version, default: 1.0.0"));
    }


    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("moduleVersion")) {
            moduleVersion = (String) additionalProperties.get("moduleVersion");
        } else {
            additionalProperties.put("moduleVersion", moduleVersion);
        }

        if (additionalProperties.containsKey("moduleName")) {
            moduleName = (String) additionalProperties.get("moduleName");
        } else {
            additionalProperties.put("moduleName", moduleName);
        }

        supportingFiles.add(new SupportingFile("ApiClient.mustache", ("lib/WWW/" + moduleName).replace('/', File.separatorChar), "ApiClient.pm"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", ("lib/WWW/" + moduleName).replace('/', File.separatorChar), "Configuration.pm"));
        supportingFiles.add(new SupportingFile("BaseObject.mustache", ("lib/WWW/" + moduleName).replace('/', File.separatorChar), "Object/BaseObject.pm"));
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "perl";
    }

    public String getHelp() {
        return "Generates a Perl client library.";
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + "/lib/WWW/" + moduleName + apiPackage()).replace('/', File.separatorChar);
    }

    public String modelFileFolder() {
        return (outputFolder + "/lib/WWW/" + moduleName + modelPackage()).replace('/', File.separatorChar);
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
        return type;
    }

    public String toDefaultValue(Property p) {
        return "null";
    }


    @Override
    public String toVarName(String name) {
        // return the name in underscore style
        // PhoneNumber => phone_number
        name = underscore(name);

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
        // model name cannot use reserved keyword
        if (reservedWords.contains(name)) {
            escapeReservedWord(name); // e.g. return => _return
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
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

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
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return underscore(operationId);
    }


}
