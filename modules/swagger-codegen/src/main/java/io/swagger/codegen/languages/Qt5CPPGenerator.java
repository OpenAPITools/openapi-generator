package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DecimalProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.BaseIntegerProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class Qt5CPPGenerator extends DefaultCodegen implements CodegenConfig {
    public static final String CPP_NAMESPACE = "cppNamespace";
    public static final String CPP_NAMESPACE_DESC = "C++ namespace (convention: name::space::for::api).";

    protected final String PREFIX = "SWG";
    protected Set<String> foundationClasses = new HashSet<String>();
    // source folder where to write the files
    protected String sourceFolder = "client";
    protected String apiVersion = "1.0.0";
    protected Map<String, String> namespaces = new HashMap<String, String>();
    protected Set<String> systemIncludes = new HashSet<String>();
    protected String cppNamespace = "Swagger";

    public Qt5CPPGenerator() {
        super();

        // set the output folder here
        outputFolder = "generated-code/qt5cpp";

        /*
         * Models.  You can write model files using the modelTemplateFiles map.
         * if you want to create one template for file, you can do so here.
         * for multiple files for model, just put another entry in the `modelTemplateFiles` with
         * a different extension
         */
        modelTemplateFiles.put(
                "model-header.mustache",
                ".h");

        modelTemplateFiles.put(
                "model-body.mustache",
                ".cpp");

        /*
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        apiTemplateFiles.put(
                "api-header.mustache",   // the template to use
                ".h");       // the extension for each file to write

        apiTemplateFiles.put(
                "api-body.mustache",   // the template to use
                ".cpp");       // the extension for each file to write

        /*
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "qt5cpp";

        // CLI options
        addOption(CPP_NAMESPACE, CPP_NAMESPACE_DESC, this.cppNamespace);

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        "sample1",  // replace with static values
                        "sample2")
        );

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties().put("prefix", PREFIX);

        // Write defaults namespace in properties so that it can be accessible in templates.
        // At this point command line has not been parsed so if value is given
        // in command line it will superseed this content
        additionalProperties.put("cppNamespace",cppNamespace);

        /*
         * Language Specific Primitives.  These types will not trigger imports by
         * the client generator
         */
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "bool",
                        "qint32",
                        "qint64",
                        "float",
                        "double")
        );

        supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder, PREFIX + "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder, PREFIX + "Helpers.cpp"));
        supportingFiles.add(new SupportingFile("HttpRequest.h.mustache", sourceFolder, PREFIX + "HttpRequest.h"));
        supportingFiles.add(new SupportingFile("HttpRequest.cpp.mustache", sourceFolder, PREFIX + "HttpRequest.cpp"));
        supportingFiles.add(new SupportingFile("modelFactory.mustache", sourceFolder, PREFIX + "ModelFactory.h"));
        supportingFiles.add(new SupportingFile("object.mustache", sourceFolder, PREFIX + "Object.h"));

        super.typeMapping = new HashMap<String, String>();

        typeMapping.put("date", "QDate");
        typeMapping.put("DateTime", "QDateTime");
        typeMapping.put("string", "QString");
        typeMapping.put("integer", "qint32");
        typeMapping.put("long", "qint64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "QList");
        typeMapping.put("map", "QMap");
        typeMapping.put("file", "SWGHttpRequestInputFileElement");
        typeMapping.put("object", PREFIX + "Object");
        //TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "QString");
        typeMapping.put("ByteArray", "QByteArray");
        // UUID support - possible enhancement : use QUuid instead of QString.
        //   beware though that Serialisation/deserialisation of QUuid does not
        //   come out of the box and will need to be sorted out (at least imply
        //   modifications on multiple templates)
        typeMapping.put("UUID", "QString");

        importMapping = new HashMap<String, String>();

        importMapping.put("SWGHttpRequestInputFileElement", "#include \"" + PREFIX + "HttpRequest.h\"");

        namespaces = new HashMap<String, String>();

        foundationClasses.add("QString");

        systemIncludes.add("QString");
        systemIncludes.add("QList");
        systemIncludes.add("QMap");
        systemIncludes.add("QDate");
        systemIncludes.add("QDateTime");
        systemIncludes.add("QByteArray");
    }

    protected void addOption(String key, String description, String defaultValue) {
        CliOption option = new CliOption(key, description);
        if (defaultValue != null)
            option.defaultValue(defaultValue);
        cliOptions.add(option);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("cppNamespace")){
            cppNamespace = (String) additionalProperties.get("cppNamespace");
        }

        additionalProperties.put("cppNamespaceDeclarations", cppNamespace.split("\\::"));
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see io.swagger.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -l flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "qt5cpp";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a qt5 C++ client library.";
    }

    @Override
    public String toModelImport(String name) {
        if (namespaces.containsKey(name)) {
            return "using " + namespaces.get(name) + ";";
        } else if (systemIncludes.contains(name)) {
            return "#include <" + name + ">";
        }

        String folder = modelPackage().replace("::", File.separator);
        if (!folder.isEmpty())
            folder += File.separator;

        return "#include \"" + folder + name + ".h\"";
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reseved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    /**
     * Location to write model files.  You can use the modelPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace("::", File.separator);
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace("::", File.separator);
    }

    @Override
    public String toModelFilename(String name) {
        return modelNamePrefix + initialCaps(name);
    }

    @Override
    public String toApiFilename(String name) {
        return PREFIX + initialCaps(name) + "Api";
    }

    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Property p) {
        String swaggerType = getSwaggerType(p);

        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">*";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "<QString, " + getTypeDeclaration(inner) + ">*";
        }
        if (foundationClasses.contains(swaggerType)) {
            return swaggerType + "*";
        } else if (languageSpecificPrimitives.contains(swaggerType)) {
            return toModelName(swaggerType);
        } else {
            return swaggerType + "*";
        }
    }


    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            return "new QString(\"\")";
        } else if (p instanceof BooleanProperty) {
            return "false";
        } else if (p instanceof DateProperty) {
            return "NULL";
        } else if (p instanceof DateTimeProperty) {
            return "NULL";
        } else if (p instanceof DoubleProperty) {
            return "0.0";
        } else if (p instanceof FloatProperty) {
            return "0.0f";
        } else if (p instanceof IntegerProperty) {
            return "0";
        } else if (p instanceof LongProperty) {
            return "0L";
        } else if (p instanceof BaseIntegerProperty) {
            // catchall for any other format of the swagger specifiction
            // integer type not explicitly handled above
            return "0";
        } else if (p instanceof DecimalProperty) {
            return "0.0";
        } else if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            String inner = getSwaggerType(ap.getAdditionalProperties());
            return "new QMap<QString, " + inner + ">()";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            if (!languageSpecificPrimitives.contains(inner)) {
                inner += "*";
            }
            return "new QList<" + inner + ">()";
        }
        // else
        if (p instanceof RefProperty) {
            RefProperty rp = (RefProperty) p;
            return "new " + toModelName(rp.getSimpleRef()) + "()";
        }
        return "NULL";
    }

    /**
     * Optional - swagger type conversion.  This is used to map swagger types in a `Property` into
     * either language specific types via `typeMapping` or into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     * @see io.swagger.models.properties.Property
     */
    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
            if (foundationClasses.contains(type)) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String toModelName(String type) {
        if (typeMapping.keySet().contains(type) ||
                typeMapping.values().contains(type) ||
                importMapping.values().contains(type) ||
                defaultIncludes.contains(type) ||
                languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return modelNamePrefix + Character.toUpperCase(type.charAt(0)) + type.substring(1);
        }
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

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
        return toVarName(name);
    }

    @Override
    public String toApiName(String type) {
        return PREFIX + Character.toUpperCase(type.charAt(0)) + type.substring(1) + "Api";
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
}
