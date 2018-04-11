package io.swagger.codegen.languages;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;

public class EiffelClientCodegen extends AbstractEiffelCodegen {
    static Logger LOGGER = LoggerFactory.getLogger(EiffelClientCodegen.class);

    protected String libraryTarget = "swagger_eiffel_client";
    protected String packageName = "Eiffel";
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs";
    protected String modelDocPath = "docs";
    protected String modelPath = "domain";

    protected UUID uuid;
    protected UUID uuidTest;

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "eiffel";
    }

    @Override
    public String getHelp() {
        return "Generates a Eiffel client library (beta).";
    }

    public EiffelClientCodegen() {
        super();
        uuid = UUID.randomUUID();
        uuidTest = UUID.randomUUID();
        outputFolder = "generated-code/Eiffel";
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        modelTemplateFiles.put("model_generic.mustache", ".e");
        apiTemplateFiles.put("api.mustache", ".e");
        apiTestTemplateFiles.put("test/api_test.mustache", ".e");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "Eiffel";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = true;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("swagger");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("uuid", uuid.toString());
        additionalProperties.put("uuidTest", uuidTest.toString());
        additionalProperties.put("libraryTarget", libraryTarget);
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        modelPackage = packageName;
        apiPackage = packageName;

        final String authFolder = ("src/framework/auth");
        final String serializerFolder = ("src/framework/serialization");
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("ecf.mustache", "", "api_client.ecf"));
        supportingFiles.add(new SupportingFile("test/ecf_test.mustache", "test", "api_test.ecf"));
        supportingFiles.add(new SupportingFile("test/application.mustache", "test", "application.e"));
        supportingFiles.add(new SupportingFile("api_client.mustache", "src", "api_client.e"));
        supportingFiles.add(new SupportingFile("framework/api_i.mustache", "src/framework", "api_i.e"));
        supportingFiles.add(
                new SupportingFile("framework/api_client_request.mustache", "src/framework", "api_client_request.e"));
        supportingFiles.add(
                new SupportingFile("framework/api_client_response.mustache", "src/framework", "api_client_response.e"));
        supportingFiles.add(new SupportingFile("framework/api_error.mustache", "src/framework", "api_error.e"));
        supportingFiles.add(new SupportingFile("framework/configuration.mustache", "src/framework", "configuration.e"));
        supportingFiles
                .add(new SupportingFile("framework/auth/authentication.mustache", authFolder, "authentication.e"));
        supportingFiles.add(new SupportingFile("framework/auth/api_key_auth.mustache", authFolder, "api_key_auth.e"));
        supportingFiles
                .add(new SupportingFile("framework/auth/http_basic_auth.mustache", authFolder, "http_basic_auth.e"));
        supportingFiles.add(new SupportingFile("framework/auth/oauth.mustache", authFolder, "oauth.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/api_deserializer.mustache", serializerFolder,
                "api_deserializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/api_json_deserializer.mustache",
                serializerFolder, "api_json_deserializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/api_json_serializer.mustache", serializerFolder,
                "api_json_serializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/api_serializer.mustache", serializerFolder,
                "api_serializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/json_basic_reflector_deserializer.mustache",
                serializerFolder, "json_basic_reflector_deserializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/json_type_utilities_ext.mustache",
                serializerFolder, "json_type_utilities_ext.e"));

    }


    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + "src" + File.separator + "api";
    }

    public String modelFileFolder() {
        return outputFolder + File.separator + "src" + File.separator + modelPath;
    }

    public String apiTestFileFolder() {
        return outputFolder + File.separator + "test" + File.separator + "apis";
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }
    

    @Override
    public String toEnumName(CodegenProperty property) {
        return sanitizeName(property.name).toUpperCase() + "_ENUM";
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (value.length() == 0) {
            return "EMPTY";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(value) != null) {
            return getSymbolName(value).toUpperCase();
        }

        // number
        if ("INTEGER_32".equals(datatype) || "INTEGER_64".equals(datatype) ||
            "REAL_32".equals(datatype) || "REAL_64".equals(datatype)) {
            String varName = "NUMBER_" + value;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String var = value.replaceAll("\\W+", "_").toLowerCase();
        if (var.matches("\\d.*")) {
            return "val_" + var;
        } else if (var.startsWith("_")){
            return "val" + var;
        } else {
            return "val_" + var;
        }
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("INTEGER_32".equals(datatype) || "INTEGER_64".equals(datatype) ||
            "REAL_32".equals(datatype) || "REAL_64".equals(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }    

}
