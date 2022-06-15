/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class CppUE4ClientCodegen extends AbstractCppCodegen {
    public static final String CPP_NAMESPACE = "cppNamespace";
    public static final String CPP_NAMESPACE_DESC = "C++ namespace (convention: name::space::for::api).";
    public static final String UNREAL_MODULE_NAME = "unrealModuleName";
    public static final String UNREAL_MODULE_NAME_DESC = "Name of the generated unreal module (optional)";
    public static final String OPTIONAL_PROJECT_FILE_DESC = "Generate Build.cs";

    protected String unrealModuleName = "OpenAPI";
    // Will be treated as pointer
    protected Set<String> pointerClasses = new HashSet<>();
    // source folder where to write the files
    protected String privateFolder = "Private";
    protected String publicFolder = "Public";
    protected String apiVersion = "1.0.0";
    protected Map<String, String> namespaces = new HashMap<>();
    // Will be included using the <> syntax, not used in Unreal's coding convention
    protected Set<String> systemIncludes = new HashSet<>();
    protected String cppNamespace = unrealModuleName;
    protected boolean optionalProjectFileFlag = true;

    public CppUE4ClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        // set the output folder here
        outputFolder = "generated-code/cpp-ue4";

        // set modelNamePrefix as default for cpp-ue4
        if ("".equals(modelNamePrefix)) {
            modelNamePrefix = unrealModuleName;
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
                "model-source.mustache",
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
                "api-source.mustache",   // the template to use
                ".cpp");       // the extension for each file to write

        apiTemplateFiles.put(
                "api-operations-header.mustache",   // the template to use
                ".h");       // the extension for each file to write

        apiTemplateFiles.put(
                "api-operations-source.mustache",   // the template to use
                ".cpp");       // the extension for each file to write

        /*
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "cpp-ue4";

        // CLI options
        addOption(CPP_NAMESPACE, CPP_NAMESPACE_DESC, this.cppNamespace);
        addOption(UNREAL_MODULE_NAME, UNREAL_MODULE_NAME_DESC, this.unrealModuleName);
        addSwitch(CodegenConstants.OPTIONAL_PROJECT_FILE, OPTIONAL_PROJECT_FILE_DESC, this.optionalProjectFileFlag);

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties().put("modelNamePrefix", modelNamePrefix);
        additionalProperties().put("modelPackage", modelPackage);
        additionalProperties().put("apiPackage", apiPackage);
        additionalProperties().put("dllapi", unrealModuleName.toUpperCase(Locale.ROOT) + "_API");
        additionalProperties().put("unrealModuleName", unrealModuleName);

        // Write defaults namespace in properties so that it can be accessible in templates.
        // At this point command line has not been parsed so if value is given
        // in command line it will supersede this content
        additionalProperties.put("cppNamespace", cppNamespace);
        additionalProperties.put("unrealModuleName", unrealModuleName);

        /*
         * Language Specific Primitives.  These types will not trigger imports by
         * the client generator
         */
        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "bool",
                        "int32",
                        "int64",
                        "float",
                        "double",
                        "FString",
                        "FDateTime",
                        "FGuid",
                        "TArray",
                        "TArray<uint8>",  // For byte arrays
                        "TMap",
                        "TSharedPtr<FJsonObject>",
                        "TSharedPtr<FJsonValue>")
        );

        supportingFiles.add(new SupportingFile("model-base-header.mustache", publicFolder, modelNamePrefix + "BaseModel.h"));
        supportingFiles.add(new SupportingFile("model-base-source.mustache", privateFolder, modelNamePrefix + "BaseModel.cpp"));
        supportingFiles.add(new SupportingFile("helpers-header.mustache", publicFolder, modelNamePrefix + "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-source.mustache", privateFolder, modelNamePrefix + "Helpers.cpp"));
        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Build.cs.mustache", unrealModuleName + ".Build.cs"));
            supportingFiles.add(new SupportingFile("module-header.mustache", privateFolder, unrealModuleName + "Module.h"));
            supportingFiles.add(new SupportingFile("module-source.mustache", privateFolder, unrealModuleName + "Module.cpp"));
        }

        super.typeMapping = new HashMap<>();

        // Maps C++ types during call to getSchemaType, see DefaultCodegen.getSchemaType and not the types/formats
        // defined in openapi specification "array" is also used explicitly in the generator for containers
        typeMapping.clear();
        typeMapping.put("integer", "int32");
        typeMapping.put("long", "int64");
        typeMapping.put("float", "float");
        typeMapping.put("number", "double");
        typeMapping.put("double", "double");
        typeMapping.put("string", "FString");
        typeMapping.put("byte", "uint8");
        typeMapping.put("binary", "TArray<uint8>");
        typeMapping.put("ByteArray", "TArray<uint8>");
        typeMapping.put("password", "FString");
        typeMapping.put("boolean", "bool");
        typeMapping.put("date", "FDateTime");
        typeMapping.put("Date", "FDateTime");
        typeMapping.put("date-time", "FDateTime");
        typeMapping.put("DateTime", "FDateTime");
        typeMapping.put("array", "TArray");
        typeMapping.put("list", "TArray");
        typeMapping.put("map", "TMap");
        typeMapping.put("object", "TSharedPtr<FJsonObject>");
        typeMapping.put("Object", "TSharedPtr<FJsonObject>");
        typeMapping.put("file", "HttpFileInput");
        typeMapping.put("UUID", "FGuid");
        typeMapping.put("AnyType", "TSharedPtr<FJsonValue>");

        importMapping = new HashMap<>();
        importMapping.put("HttpFileInput", "#include \"" + modelNamePrefix + "Helpers.h\"");

        namespaces = new HashMap<>();
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("cppNamespace")) {
            cppNamespace = (String) additionalProperties.get("cppNamespace");
        }

        additionalProperties.put("cppNamespaceDeclarations", cppNamespace.split("\\::"));

        boolean updateSupportingFiles = false;
        if (additionalProperties.containsKey("unrealModuleName")) {
            unrealModuleName = (String) additionalProperties.get("unrealModuleName");
            additionalProperties().put("dllapi", unrealModuleName.toUpperCase(Locale.ROOT) + "_API");
            modelNamePrefix = unrealModuleName;
            updateSupportingFiles = true;
        }

        if (additionalProperties.containsKey("modelNamePrefix")) {
            modelNamePrefix = (String) additionalProperties.get("modelNamePrefix");
            updateSupportingFiles = true;
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_FILE)) {
            setOptionalProjectFileFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_PROJECT_FILE));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_PROJECT_FILE, optionalProjectFileFlag);
        }

        if (updateSupportingFiles) {
            supportingFiles.clear();

            supportingFiles.add(new SupportingFile("model-base-header.mustache", publicFolder, modelNamePrefix + "BaseModel.h"));
            supportingFiles.add(new SupportingFile("model-base-source.mustache", privateFolder, modelNamePrefix + "BaseModel.cpp"));
            supportingFiles.add(new SupportingFile("helpers-header.mustache", publicFolder, modelNamePrefix + "Helpers.h"));
            supportingFiles.add(new SupportingFile("helpers-source.mustache", privateFolder, modelNamePrefix + "Helpers.cpp"));
            if (optionalProjectFileFlag) {
                supportingFiles.add(new SupportingFile("Build.cs.mustache", unrealModuleName + ".Build.cs"));
                supportingFiles.add(new SupportingFile("module-header.mustache", privateFolder, unrealModuleName + "Module.h"));
                supportingFiles.add(new SupportingFile("module-source.mustache", privateFolder, unrealModuleName + "Module.cpp"));
            }

            importMapping.put("HttpFileInput", "#include \"" + modelNamePrefix + "Helpers.h\"");
        }
    }

    public void setOptionalProjectFileFlag(boolean flag) {
        this.optionalProjectFileFlag = flag;
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
     * to select the library with the -l flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "cpp-ue4";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Unreal Engine 4 C++ Module (beta).";
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

    @Override
    protected boolean needToImport(String type) {
        boolean shouldImport = super.needToImport(type);
        if (shouldImport)
            return !languageSpecificPrimitives.contains(type);
        else
            return false;
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
        return outputFolder + File.separator + modelPackage().replace("::", File.separator);
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace("::", File.separator);
    }

    @Override
    public String modelFilename(String templateName, String modelName) {
        String suffix = modelTemplateFiles().get(templateName);
        String folder = privateFolder;
        if (".h".equals(suffix)) {
            folder = publicFolder;
        }

        return modelFileFolder() + File.separator + folder + File.separator + toModelFilename(modelName) + suffix;
    }

    @Override
    public String toModelFilename(String name) {
        name = sanitizeName(name);
        return modelNamePrefix + camelize(name);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        String folder = privateFolder;
        if (".h".equals(suffix)) {
            folder = publicFolder;
        }

        if (templateName.startsWith("api-operations")) {
            return apiFileFolder() + File.separator + folder + File.separator + toApiFilename(tag) + "Operations" + suffix;
        } else {
            return apiFileFolder() + File.separator + folder + File.separator + toApiFilename(tag) + suffix;
        }
    }

    @Override
    public String toApiFilename(String name) {
        name = sanitizeName(name);
        return modelNamePrefix + camelize(name) + "Api";
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
            String inner = getSchemaType(ap.getItems());
            return getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(getAdditionalProperties(p));
            return getSchemaType(p) + "<FString, " + getTypeDeclaration(inner) + ">";
        }

        if (pointerClasses.contains(openAPIType)) {
            return openAPIType + "*";
        } else if (languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        } else {
            return openAPIType;
        }
    }

    @Override
    public String getTypeDeclaration(String name) {
        return name;
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "TEXT(\"" + p.getDefault().toString() + "\")";
            } else {
                return null;
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "false";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            return "FDateTime(0)";
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return "FDateTime(0)";
        } else if (ModelUtils.isDoubleSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "0.0";
            }
        } else if (ModelUtils.isFloatSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "0.0f";
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "0";
            }
        } else if (ModelUtils.isLongSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "0";
            }
        }

        return null;
    }

    /**
     * Optional - OpenAPI type conversion.  This is used to map OpenAPI types in a `Property` into
     * either language specific types via `typeMapping` or into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     * @see io.swagger.v3.oas.models.media.Schema
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
            if (pointerClasses.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
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
            return modelNamePrefix + camelize(sanitizeName(type), false);
        }
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all upper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase(Locale.ROOT);
        }

        //Unreal variable names are CamelCase
        String camelCaseName = camelize(name, false);

        //Avoid empty variable name at all costs
        if(!camelCaseName.isEmpty()) {
            name = camelCaseName;
        }

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        return toVarName(name);
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toApiName(String type) {
        return modelNamePrefix + camelize(type, false) + "Api";
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

    @Override
    public String toBooleanGetter(String name) {
        return "Is" + getterAndSetterCapitalize(name);
    }

    @Override
    public String toGetter(String name) {
        return "Get" + getterAndSetterCapitalize(name);
    }

    @Override
    public String toSetter(String name) {
        return "Set" + getterAndSetterCapitalize(name);
    }
}
