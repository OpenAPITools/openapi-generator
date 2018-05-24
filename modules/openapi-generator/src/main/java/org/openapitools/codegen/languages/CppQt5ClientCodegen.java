/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;

import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class CppQt5ClientCodegen extends AbstractCppCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(CppQt5ClientCodegen.class);

    public static final String CPP_NAMESPACE = "cppNamespace";
    public static final String CPP_NAMESPACE_DESC = "C++ namespace (convention: name::space::for::api).";
    public static final String OPTIONAL_PROJECT_FILE_DESC = "Generate client.pri.";

    protected final String PREFIX = "OAI";
    protected Set<String> foundationClasses = new HashSet<String>();
    // source folder where to write the files
    protected String sourceFolder = "client";
    protected String apiVersion = "1.0.0";
    protected Map<String, String> namespaces = new HashMap<String, String>();
    protected Set<String> systemIncludes = new HashSet<String>();
    protected String cppNamespace = "OpenAPI";
    protected boolean optionalProjectFileFlag = true;

    public CppQt5ClientCodegen() {
        super();

        // set the output folder here
        outputFolder = "generated-code/qt5cpp";

        // set modelNamePrefix as default for QT5CPP
        if (StringUtils.isEmpty(modelNamePrefix)) {
            modelNamePrefix = PREFIX;
        }

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
        addSwitch(CodegenConstants.OPTIONAL_PROJECT_FILE, OPTIONAL_PROJECT_FILE_DESC, this.optionalProjectFileFlag);

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties().put("prefix", PREFIX);

        // Write defaults namespace in properties so that it can be accessible in templates.
        // At this point command line has not been parsed so if value is given
        // in command line it will superseed this content
        additionalProperties.put("cppNamespace", cppNamespace);

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
        supportingFiles.add(new SupportingFile("QObjectWrapper.h.mustache", sourceFolder, PREFIX + "QObjectWrapper.h"));
        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Project.mustache", sourceFolder, "client.pri"));
        }

        super.typeMapping = new HashMap<String, String>();

        typeMapping.put("date", "QDate");
        typeMapping.put("DateTime", "QDateTime");
        typeMapping.put("string", "QString");
        typeMapping.put("integer", "qint32");
        typeMapping.put("long", "qint64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "QList");
        typeMapping.put("map", "QMap");
        typeMapping.put("file", "OAIHttpRequestInputFileElement");
        typeMapping.put("object", PREFIX + "Object");
        // mapped as "file" type for OAS 3.0
        typeMapping.put("binary", "OAIHttpRequestInputFileElement");
        typeMapping.put("ByteArray", "QByteArray");
        // UUID support - possible enhancement : use QUuid instead of QString.
        //   beware though that Serialisation/deserialisation of QUuid does not
        //   come out of the box and will need to be sorted out (at least imply
        //   modifications on multiple templates)
        typeMapping.put("UUID", "QString");

        importMapping = new HashMap<String, String>();

        importMapping.put("OAIHttpRequestInputFileElement", "#include \"" + PREFIX + "HttpRequest.h\"");

        namespaces = new HashMap<String, String>();

        foundationClasses.add("QString");

        systemIncludes.add("QString");
        systemIncludes.add("QList");
        systemIncludes.add("QMap");
        systemIncludes.add("QDate");
        systemIncludes.add("QDateTime");
        systemIncludes.add("QByteArray");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("cppNamespace")) {
            cppNamespace = (String) additionalProperties.get("cppNamespace");
        }

        additionalProperties.put("cppNamespaceDeclarations", cppNamespace.split("\\::"));
        if (additionalProperties.containsKey("modelNamePrefix")) {
            supportingFiles.clear();
            supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder, modelNamePrefix + "Helpers.h"));
            supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder, modelNamePrefix + "Helpers.cpp"));
            supportingFiles.add(new SupportingFile("HttpRequest.h.mustache", sourceFolder, modelNamePrefix + "HttpRequest.h"));
            supportingFiles.add(new SupportingFile("HttpRequest.cpp.mustache", sourceFolder, modelNamePrefix + "HttpRequest.cpp"));
            supportingFiles.add(new SupportingFile("modelFactory.mustache", sourceFolder, modelNamePrefix + "ModelFactory.h"));
            supportingFiles.add(new SupportingFile("object.mustache", sourceFolder, modelNamePrefix + "Object.h"));
            supportingFiles.add(new SupportingFile("QObjectWrapper.h.mustache", sourceFolder, modelNamePrefix + "QObjectWrapper.h"));

            typeMapping.put("object", modelNamePrefix + "Object");
            typeMapping.put("file", modelNamePrefix + "HttpRequestInputFileElement");
            importMapping.put("SWGHttpRequestInputFileElement", "#include \"" + modelNamePrefix + "HttpRequest.h\"");
            additionalProperties().put("prefix", modelNamePrefix);
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_FILE)) {
            setOptionalProjectFileFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_PROJECT_FILE));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_PROJECT_FILE, optionalProjectFileFlag);
        }
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "cpp-qt5";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Qt5 C++ client library.";
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
     * those terms here.  This logic is only called if a variable matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
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
        return modelNamePrefix + initialCaps(name) + "Api";
    }

    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">*";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = (Schema) p.getAdditionalProperties();
            return getSchemaType(p) + "<QString, " + getTypeDeclaration(inner) + ">*";
        }
        if (foundationClasses.contains(openAPIType)) {
            return openAPIType + "*";
        } else if (languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        } else {
            return openAPIType + "*";
        }
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            return "false";
        } else if (ModelUtils.isDateSchema(p)) {
            return "NULL";
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return "NULL";
        } else if (ModelUtils.isNumberSchema(p)) {
            if (SchemaTypeUtil.FLOAT_FORMAT.equals(p.getFormat())) {
                return "0.0f";
            }
            return "0.0";
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (SchemaTypeUtil.INTEGER64_FORMAT.equals(p.getFormat())) {
                return "0L";
            }
            return "0";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = (Schema) p.getAdditionalProperties();
            return "new QMap<QString, " + getTypeDeclaration(inner) + ">()";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return "new QList<" + getTypeDeclaration(inner) + ">()";
        } else if (ModelUtils.isStringSchema(p)) {
            return "new QString(\"\")";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            return "new " + toModelName(ModelUtils.getSimpleRef(p.get$ref())) + "()";
        }
        return "NULL";
    }

    /**
     * Optional - OpenAPI type conversion.  This is used to map OpenAPI types in a `Schema` into
     * either language specific types via `typeMapping` or into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */
    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);

        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
            if (foundationClasses.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public String toModelName(String type) {
        if (type == null) {
            LOGGER.warn("Model name can't be null. Defaul to 'UnknownModel'.");
            type = "UnknownModel";
        }

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
        return modelNamePrefix + Character.toUpperCase(type.charAt(0)) + type.substring(1) + "Api";
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

    public void setOptionalProjectFileFlag(boolean flag) {
        this.optionalProjectFileFlag = flag;
    }
}
