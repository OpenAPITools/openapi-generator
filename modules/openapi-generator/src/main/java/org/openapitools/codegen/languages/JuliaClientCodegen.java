package org.openapitools.codegen.languages;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.ModelUtils;

import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class JuliaClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(JuliaClientCodegen.class);

    protected String packageName;

    // source folder where to write the files
    protected String sourceFolder = "src";
    protected String apiVersion = "1.0.0";

    /**
     * Configures the type of generator.
     *
     * @return  the CodegenType for this generator
     */
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -l flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "julia";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a Julia client library (beta).";
    }

    public JuliaClientCodegen() {
        super();

        supportsInheritance = true;
        supportsMixins = true;

        // set the output folder here
        outputFolder = "generated-code/julia";

        /**
         * Models.  You can write model files using the modelTemplateFiles map.
         * if you want to create one template for file, you can do so here.
         * for multiple files for model, just put another entry in the `modelTemplateFiles` with
         * a different extension
         */
        // models
        modelTemplateFiles.put("model.mustache", ".jl");

        /**
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        apiTemplateFiles.put("api.mustache", ".jl");

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("REQUIRE", "", "REQUIRE"));

        // Template Location: where templates will be read from. The generator will use the resource stream to attempt to read the templates.
        templateDir = "julia";

        // Reserved words.  Override this with reserved words specific to your language
        reservedWords = new HashSet<String> (
                Arrays.asList(
                        "if", "else", "elseif", "while", "for", "begin", "end", "quote",
                        "try", "catch", "return", "local", "abstract", "function", "macro",
                        "ccall", "finally", "typealias", "break", "continue", "type",
                        "global", "module", "using", "import", "export", "const", "let",
                        "bitstype", "do", "baremodule", "importall", "immutable",
                        "Type", "Enum", "Any", "DataType", "Base"
                )
        );

        // Additional Properties. These values can be passed to the templates and are available in models, apis, and supporting files
        additionalProperties.put("apiVersion", apiVersion);

        // Language Specific Primitives.  These types will not trigger imports by the client generator
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("Int", "Int32", "Int64", "Float32", "Float64", "Vector", "Array", "Bool", "String", "Nothing")
        );

        typeMapping.clear();
        typeMapping.put("integer", "Int32");
        typeMapping.put("long", "Int64");
        typeMapping.put("float", "Float32");
        typeMapping.put("double", "Float64");
        typeMapping.put("string", "String");
        typeMapping.put("byte", "UInt8");
        typeMapping.put("binary", "Vector{UInt8}");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("number", "Float32");
        typeMapping.put("array", "Vector");
        typeMapping.put("map", "Dict");
        typeMapping.put("date", "Date");
        typeMapping.put("Object", "Any");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("File", "String");
        typeMapping.put("UUID", "String");
        typeMapping.put("ByteArray", "Vector{UInt8}");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Julia package name.").defaultValue("OpenAPIClient"));
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }
        else {
            setPackageName("OpenAPIClient");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);

        supportingFiles.add(new SupportingFile("client.mustache", "src", packageName + ".jl"));
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reseved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if (reservedWords.contains(name)) {
            return "_" + name;  // add an underscore to the name
        } else {
            return name;
        }
    }

    /**
     * Location to write model files.
     */
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder;
    }

    /**
     * Location to write api files.
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder;
    }

    @Override
    public String toModelFilename(String name) {
        name = sanitizeName(name);
        name = name.replaceAll("$", "");
        return "model_" + dropDots(name);
    }

    private static String dropDots(String str) {
        return str.replaceAll("\\.", "_");
    }

    @Override
    public String toApiFilename(String name) {
        name = name.replaceAll("-", "_");
        return "api_" + org.openapitools.codegen.utils.StringUtils.camelize(name) + "Api";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return org.openapitools.codegen.utils.StringUtils.camelize(name) + "Api";
    }

    @Override
    public String toParamName(String name) {
        return escapeReservedWord(sanitizeName(name));
    }

    @Override
    public String toApiVarName(String name) {
        return escapeReservedWord(sanitizeName(name));
    }

    @Override
    public String toVarName(String name) {
        return escapeReservedWord(sanitizeName(name));
    }

    /**
     * Sanitize name (parameter, property, method, etc)
     *
     * @param name string to be sanitize
     * @return sanitized string
     */
    @Override
    @SuppressWarnings("static-method")
    public String sanitizeName(String name) {
        if (name == null) {
            LOGGER.error("String to be sanitized is null. Default to ERROR_UNKNOWN");
            return "ERROR_UNKNOWN";
        }

        // if the name is just '$', map it to 'value' for the time being.
        if ("$".equals(name)) {
            return "value";
        }

        name = name.replaceAll("\\[\\]", "");
        name = name.replaceAll("\\[", "_");
        name = name.replaceAll("\\]", "");
        name = name.replaceAll("\\(", "_");
        name = name.replaceAll("\\)", "");
        name = name.replaceAll("\\.", "_");
        name = name.replaceAll("-", "_");
        name = name.replaceAll(" ", "_");
        return name.replaceAll("[^a-zA-Z0-9_{}]", "");
    }

    /**
     * Sanitize tag
     *
     * @param tag Tag
     * @return Sanitized tag
     */
    public String sanitizeTag(String tag) {
        tag = org.openapitools.codegen.utils.StringUtils.camelize(sanitizeName(tag));

        // tag starts with numbers
        if (tag.matches("^\\d.*")) {
            tag = "Class" + tag;
        }

        return tag;
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // remove dollar sign
        name = name.replaceAll("$", "");

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

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

    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        if(ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "{" + getTypeDeclaration(inner) + "}";
        }
        else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return getSchemaType(p) + "{String, " + getTypeDeclaration(inner) + "}";
        }
        return super.getTypeDeclaration(p);
    }

    /**
     * Optional - OpenAPI type conversion.  This is used to map OpenAPI types in a `Property` into
     * either language specific types via `typeMapping` or into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */
    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if(typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if(languageSpecificPrimitives.contains(type))
                return toModelName(type);
        }
        else
            type = openAPIType;
        return toModelName(type);
    }

    /**
     * Return the default value of the property
     *
     * @param p schema object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            //StringProperty dp = (StringProperty) p;
            if (p.getDefault() != null) {
                return "\"" + p.getDefault() + "\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            //BooleanProperty dp = (BooleanProperty) p;
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isDateSchema(p)) {
            // TODO
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
        } else if (ModelUtils.isDoubleSchema(p)) {
            //DoubleProperty dp = (DoubleProperty) p;
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isFloatSchema(p)) {
            //FloatProperty dp = (FloatProperty) p;
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            //IntegerProperty dp = (IntegerProperty) p;
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isLongSchema(p)) {
            //LongProperty dp = (LongProperty) p;
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        }

        return "nothing";
    }

    public String escapeUnsafeCharacters(String input) {
        return input;
    }

    /**
     * Escape single and/or double quote to avoid code injection
     * @param input String to be cleaned up
     * @return string with quotation mark removed or escaped
     */
    public String escapeQuotationMark(String input) {
        return input.replace("\"", "\\\"");
    }

    private String escapeBaseName(String name) {
        name = name.replaceAll("\\$", "\\\\\\$");
        return name;
    }

    @Override
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
        CodegenParameter parameter = super.fromParameter(param, imports);
        parameter.baseName = escapeBaseName(parameter.baseName);
        return parameter;
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty property = super.fromProperty(name, p);
        property.baseName = escapeBaseName(property.baseName);
        return property;
    }
}