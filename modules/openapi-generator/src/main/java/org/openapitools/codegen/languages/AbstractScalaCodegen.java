/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.samskivert.mustache.Escapers;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static org.openapitools.codegen.languages.AbstractJavaCodegen.DATE_LIBRARY;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public abstract class AbstractScalaCodegen extends DefaultCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(AbstractScalaCodegen.class);

    protected String modelPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase.name();
    protected String invokerPackage = "org.openapitools.client";
    protected String sourceFolder = "src/main/scala";
    protected boolean stripPackageName = true;
    protected String dateLibrary = DateLibraries.java8.name();

    protected enum DateLibraries {
        java8("Java 8 native JSR310 (preferred for JDK 1.8+)"),
        joda( "Joda (for legacy app)"),
        legacy( "Backport to http-client (deprecated)");

        private final String description;

        DateLibraries(String description) {
            this.description = description;
        }
    }

    public AbstractScalaCodegen() {
        super();

        languageSpecificPrimitives.addAll(Arrays.asList(
                "String",
                "boolean",
                "Boolean",
                "Double",
                "Int",
                "Long",
                "Float",
                "Object",
                "Any",
                "List",
                "Seq",
                "Map",
                "Array",
                "Byte"));

        reservedWords.addAll(Arrays.asList(
                "abstract",
                "case",
                "catch",
                "class",
                "clone",
                "def",
                "do",
                "else",
                "extends",
                "false",
                "final",
                "finally",
                "for",
                "forSome",
                "if",
                "implicit",
                "import",
                "lazy",
                "match",
                "new",
                "null",
                "object",
                "override",
                "package",
                "private",
                "protected",
                "return",
                "sealed",
                "super",
                "this",
                "throw",
                "trait",
                "try",
                "true",
                "type",
                "val",
                "var",
                "while",
                "with",
                "yield"
        ));

        // Scala specific openApi types mapping
        typeMapping.put("ByteArray", "Array[Byte]");


        importMapping = new HashMap<String, String>();
        importMapping.put("ListBuffer", "scala.collection.mutable.ListBuffer");
        // although Seq is a predef, before Scala 2.13, it _could_ refer to a mutable Seq in some cases.
        importMapping.put("Seq", "scala.collection.immutable.Seq");
        importMapping.put("Set", "scala.collection.immutable.Set");
        importMapping.put("ListSet", "scala.collection.immutable.ListSet");
        // fallback to java types
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("URI", "java.net.URI");
        importMapping.put("File", "java.io.File");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("HashMap", "java.util.HashMap");
        importMapping.put("Array", "java.util.List");
        importMapping.put("ArrayList", "java.util.ArrayList");
        // todo remove legacy date types
        importMapping.put("Date", "java.util.Date");
        importMapping.put("DateTime", "org.joda.time.*");
        importMapping.put("LocalDateTime", "org.joda.time.*");
        importMapping.put("LocalDate", "org.joda.time.*");
        importMapping.put("LocalTime", "org.joda.time.*");


        instantiationTypes.put("set", "Set");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC));
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue(modelPropertyNaming));

        CliOption dateLibrary = new CliOption(DATE_LIBRARY, "Option. Date library to use").defaultValue(this.dateLibrary);
        Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(DateLibraries.java8.name(), DateLibraries.java8.description);
        dateOptions.put(DateLibraries.joda.name(), DateLibraries.joda.description);
        dateLibrary.setEnum(dateOptions);
        cliOptions.add(dateLibrary);

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("SCALA_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable SCALA_POST_PROCESS_FILE not defined so the Scala code may not be properly formatted. To define it, try 'export SCALA_POST_PROCESS_FILE=/usr/local/bin/scalafmt' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }
        if (additionalProperties.containsKey(CodegenConstants.STRIP_PACKAGE_NAME) &&
                "false".equalsIgnoreCase(additionalProperties.get(CodegenConstants.STRIP_PACKAGE_NAME).toString())) {
            this.stripPackageName = false;
            additionalProperties.put(CodegenConstants.STRIP_PACKAGE_NAME, false);
            LOGGER.warn("stripPackageName=false. Compilation errors may occur if API type names clash with types " +
                    "in the default imports");
        }
        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming(
                    (String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }

        if (additionalProperties.containsKey(DATE_LIBRARY)) {
            this.setDateLibrary(additionalProperties.get(DATE_LIBRARY).toString(), false);
        }
        if (DateLibraries.java8.name().equals(dateLibrary)) {
            this.importMapping.put("LocalDate", "java.time.LocalDate");
            this.importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
            this.typeMapping.put("date", "LocalDate");
            this.typeMapping.put("DateTime", "OffsetDateTime");
            additionalProperties.put("java8", "true");
        } else if (DateLibraries.joda.name().equals(dateLibrary)) {
            this.importMapping.put("LocalDate", "org.joda.time.LocalDate");
            this.importMapping.put("DateTime", "org.joda.time.DateTime");
            this.importMapping.put("LocalDateTime", "org.joda.time.LocalDateTime");
            this.importMapping.put("LocalTime", "org.joda.time.LocalTime");
            this.typeMapping.put("date", "LocalDate");
            this.typeMapping.put("DateTime", "DateTime");
            additionalProperties.put("joda", "true");
        }
    }

    public void setDateLibrary(String dateLibrary, boolean withLegacy) {
        if (withLegacy && dateLibrary.equals(DateLibraries.legacy.name())) {
            this.dateLibrary = dateLibrary;
            return;
        }
        for ( DateLibraries dateLib : DateLibraries.values()) {
            if (dateLib.name().equals(dateLibrary)) {
                this.dateLibrary = dateLibrary;
                return;
            }
        }
        throw new IllegalArgumentException("Invalid dateLibrary. Must be 'java8' or 'joda'");
    }

    public String getDateLibrary() {
        return this.dateLibrary;
    }

    public void setModelPropertyNaming(String naming) {
        try {
            this.modelPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.valueOf(naming).name();
        } catch (IllegalArgumentException ex) {
            throw new IllegalArgumentException("Invalid model property naming '" +
                    naming + "'. Must be 'original', 'camelCase', " +
                    "'PascalCase' or 'snake_case'");
        }
    }

    public String getModelPropertyNaming() {
        return this.modelPropertyNaming;
    }


    @Override
    public String toVarName(String name) {
        String varName = sanitizeName(name);

        if ("_".equals(varName)) {
            varName = "_u";
        }

        // if it's all upper case, do nothing
        if (!varName.matches("^[A-Z_0-9]*$")) {
            varName = getNameUsingModelPropertyNaming(varName);
        }

        if (isReservedWord(varName) || varName.matches("^\\d.*")) {
            varName = escapeReservedWord(varName);
        }

        return varName;
    }

    public String getNameUsingModelPropertyNaming(String name) {
        switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
            case original:
                return name;
            case camelCase:
                return camelize(name, true);
            case PascalCase:
                return camelize(name);
            case snake_case:
                return underscore(name);
            default:
                throw new IllegalArgumentException("Invalid model property naming '" +
                        name + "'. Must be 'original', 'camelCase', " +
                        "'PascalCase' or 'snake_case'");
        }
    }

    public String getSourceFolder() {
        return sourceFolder;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        // Reserved words will be further escaped at the mustache compiler level.
        // Scala escaping done here (via `, without compiler escaping) would otherwise be HTML encoded.
        return "`" + name + "`";
    }

    @Override
    public Mustache.Compiler processCompiler(Mustache.Compiler compiler) {
        Mustache.Escaper SCALA = new Mustache.Escaper() {
            @Override
            public String escape(String text) {
                // Fix included as suggested by akkie in #6393
                // The given text is a reserved word which is escaped by enclosing it with grave accents. If we would
                // escape that with the default Mustache `HTML` escaper, then the escaper would also escape our grave
                // accents. So we remove the grave accents before the escaping and add it back after the escaping.
                if (text.startsWith("`") && text.endsWith("`")) {
                    String unescaped = text.substring(1, text.length() - 1);
                    return "`" + Escapers.HTML.escape(unescaped) + "`";
                }

                // All none reserved words will be escaped with the default Mustache `HTML` escaper
                return Escapers.HTML.escape(text);
            }
        };

        return compiler.withEscaper(SCALA);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);

            return getSchemaType(p) + "[String, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        if (ModelUtils.isSet(p)) {
            openAPIType = "set";
        }
        // don't apply renaming on types from the typeMapping
        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }
        return toModelName(openAPIType);
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(getAdditionalProperties(p));
            return instantiationTypes.get("map") + "[String, " + inner + "]";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            return ( ModelUtils.isSet(ap) ? instantiationTypes.get("set") : instantiationTypes.get("array") ) + "[" + inner + "]";
        } else {
            return null;
        }
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getDefault() != null) {
            return p.getDefault().toString();
        }

        // comment out the following as the default value is no handled differently
        if (ModelUtils.isBooleanSchema(p)) {
            return null;
        } else if (ModelUtils.isDateSchema(p)) {
            return null;
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return null;
        } else if (ModelUtils.isNumberSchema(p)) {
            return null;
        } else if (ModelUtils.isIntegerSchema(p)) {
            return null;
        } else if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(getAdditionalProperties(p));
            return "new HashMap[String, " + inner + "]() ";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            String genericType;
            if (ModelUtils.isSet(ap)) {
                genericType = instantiationTypes.get("set");
            } else {
                genericType = instantiationTypes.get("array");
            }

            // test for immutable Monoids with .empty method for idiomatic defaults
            if ("List".equals(genericType) ||
                "Set".equals(genericType) ||
                "Seq".equals(genericType) ||
                "Array".equals(genericType) ||
                "Vector".equals(genericType) ||
                "IndexedSeq".equals(genericType) ||
                "Iterable".equals(genericType) ||
                "ListSet".equals(genericType)
            ) {
                return genericType + ".empty[" + inner + "] ";
            }

            // Assume that any other generic types can be new'd up.
            return "new " + genericType + "[" + inner + "]() ";
        } else if (ModelUtils.isStringSchema(p)) {
            return null;
        } else {
            return null;
        }
    }

    /**
     * Convert OAS Property object to Codegen Property object
     *
     * @param name name of the property
     * @param p    OAS property object
     * @return Codegen Property object
     */
    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty prop = super.fromProperty(name, p);
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema as = (ArraySchema) p;
            if (ModelUtils.isSet(as)) {
                prop.containerType = "set";
            }
        }
        return prop;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // remove model imports to avoid warnings for importing class in the same package in Scala
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        final String prefix = modelPackage() + ".";
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(prefix)) iterator.remove();
        }
        return objs;
    }

    @Override
    public String toModelName(final String name) {
        final String sanitizedName = sanitizeName(modelNamePrefix + this.stripPackageName(name) + modelNameSuffix);

        // camelize the model name
        // phone_number => PhoneNumber
        final String camelizedName = camelize(sanitizedName);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            final String modelName = "Model" + camelizedName;
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", camelizedName, modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            final String modelName = "Model" + camelizedName; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    modelName);
            return modelName;
        }

        return camelizedName;
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    protected String formatIdentifier(String name, boolean capitalized) {
        String identifier = camelize(sanitizeName(name), true);
        if (capitalized) {
            identifier = StringUtils.capitalize(identifier);
        }
        if (identifier.matches("[a-zA-Z_$][\\w_$]+") && !isReservedWord(identifier)) {
            return identifier;
        }
        return escapeReservedWord(identifier);
    }

    protected String stripPackageName(String input) {
        if (!stripPackageName || StringUtils.isEmpty(input) || input.lastIndexOf(".") < 0)
            return input;

        int lastIndexOfDot = input.lastIndexOf(".");
        return input.substring(lastIndexOfDot + 1);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        String scalaPostProcessFile = System.getenv("SCALA_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(scalaPostProcessFile)) {
            return; // skip if SCALA_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with scala extension
        if ("scala".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = scalaPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");
        }

        operationId = camelize(sanitizeName(operationId), true);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize("call_" + operationId, true);
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn(operationId + " (starting with a number) cannot be used as method sname. Renamed to " + camelize("call_" + operationId), true);
            operationId = camelize("call_" + operationId, true);
        }

        return operationId;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.SCALA; }
}
