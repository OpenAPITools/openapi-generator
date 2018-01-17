package io.swagger.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.core.util.Yaml;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SinatraServerCodegen extends DefaultCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(SinatraServerCodegen.class);

    protected String gemName;
    protected String moduleName;
    protected String gemVersion = "1.0.0";
    protected String libFolder = "lib";

    public SinatraServerCodegen() {
        super();
        apiPackage = "lib";
        outputFolder = "generated-code" + File.separator + "sinatra";

        // no model
        modelTemplateFiles.clear();
        apiTemplateFiles.put("api.mustache", ".rb");
        embeddedTemplateDir = templateDir = "sinatra";

        typeMapping.clear();
        languageSpecificPrimitives.clear();

        setReservedWordsLowerCase(
                Arrays.asList(
                        "__FILE__", "and", "def", "end", "in", "or", "self", "unless", "__LINE__",
                        "begin", "defined?", "ensure", "module", "redo", "super", "until", "BEGIN",
                        "break", "do", "false", "next", "rescue", "then", "when", "END", "case",
                        "else", "for", "nil", "retry", "true", "while", "alias", "class", "elsif",
                        "if", "not", "return", "undef", "yield")
        );

        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("array");
        languageSpecificPrimitives.add("map");
        languageSpecificPrimitives.add("string");
        languageSpecificPrimitives.add("DateTime");

        typeMapping.put("long", "int");
        typeMapping.put("integer", "int");
        typeMapping.put("Array", "array");
        typeMapping.put("String", "string");
        typeMapping.put("List", "array");
        typeMapping.put("map", "map");
        //TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "string");
        typeMapping.put("UUID", "string");

        // remove modelPackage and apiPackage added by default
        cliOptions.clear();
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // use constant model/api package (folder path)
        //setModelPackage("models");
        setApiPackage("api");

        supportingFiles.add(new SupportingFile("my_app.mustache", "", "my_app.rb"));
        supportingFiles.add(new SupportingFile("Swaggering.rb", libFolder, "swaggering.rb"));
        supportingFiles.add(new SupportingFile("config.ru", "", "config.ru"));
        supportingFiles.add(new SupportingFile("Gemfile", "", "Gemfile"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
        supportingFiles.add(new SupportingFile("swagger.mustache","","swagger.yaml"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "sinatra";
    }

    @Override
    public String getHelp() {
        return "Generates a Sinatra server library.";
    }

    @Override
    public String escapeReservedWord(String name) {           
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage.replace("/", File.separator);
    }

    @Override
    public String getTypeDeclaration(Schema propertySchema) {
        if (propertySchema instanceof ArraySchema) {
            Schema inner = ((ArraySchema) propertySchema).getItems();
            return String.format("%s[%s]", getSchemaType(propertySchema), getTypeDeclaration(inner));
        } else if (propertySchema instanceof MapSchema && hasSchemaProperties(propertySchema)) {
            Schema inner = (Schema) propertySchema.getAdditionalProperties();
            return String.format("%s[string, %s]", getSchemaType(propertySchema), getTypeDeclaration(inner));
        }
        return super.getTypeDeclaration(propertySchema);
    }

    @Override
    public String getSchemaType(Schema propertySchema) {
        String schemaType = super.getSchemaType(propertySchema);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = schemaType;
        }
        if (type == null) {
            return null;
        }
        return type;
    }

    @Override
    public String toDefaultValue(Schema p) {
        return "null";
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all uppper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase();
        }

        // camelize (lower first character) the variable name
        // petId => pet_id
        name = underscore(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
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
        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model filename. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model filename. Renamed to " + underscore("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // underscore the model file name
        // PhoneNumber.rb => phone_number.rb
        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PhoneNumberApi.rb => phone_number_api.rb
        return underscore(name) + "_api";
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
        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = underscore("call_" + operationId);
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        return underscore(operationId);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        OpenAPI openAPI = (OpenAPI) objs.get("openapi");
        if(openAPI != null) {
            try {
                objs.put("swagger-yaml", Yaml.mapper().writeValueAsString(openAPI));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("=end", "=_end").replace("=begin", "=_begin");
    }
}
