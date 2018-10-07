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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;


public class CppQt5QHttpEngineServerCodegen extends AbstractCppCodegen implements CodegenConfig {
    @SuppressWarnings("unused")
    private static final Logger LOGGER = LoggerFactory.getLogger(CppQt5QHttpEngineServerCodegen.class);

    public static final String CPP_NAMESPACE = "cppNamespace";
    public static final String CPP_NAMESPACE_DESC = "C++ namespace (convention: name::space::for::api).";

    protected final String PREFIX = "OAI";
    protected final String SRC_DIR = "/src";
    protected final String MODEL_DIR = "/src/models";
    protected final String APIHANDLER_DIR = "/src/handlers";
    protected final String APIREQUEST_DIR = "/src/requests";
    protected Set<String> foundationClasses = new HashSet<String>();
    // source folder where to write the files
    protected String sourceFolder = "server";
    protected String apiVersion = "1.0.0";
    protected Map<String, String> namespaces = new HashMap<String, String>();
    protected Set<String> systemIncludes = new HashSet<String>();
    protected String cppNamespace = "OpenAPI";
    protected Set<String> nonFrameworkPrimitives = new HashSet<String>();
    public CppQt5QHttpEngineServerCodegen() {
        super();

        // set the output folder here
        outputFolder = "generated-code/cpp-qt5-qhttpengine-server";

        // set modelNamePrefix as default for QHttpEngine Server
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
                "apihandler.h.mustache",   // the template to use
                ".h");       // the extension for each file to write

        apiTemplateFiles.put(
                "apihandler.cpp.mustache",   // the template to use
                ".cpp");       // the extension for each file to write

        apiTemplateFiles.put(
                "apirequest.h.mustache",   // the template to use
                ".h");       // the extension for each file to write
    
        apiTemplateFiles.put(
                "apirequest.cpp.mustache",   // the template to use
                ".cpp");       // the extension for each file to write

        /*
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "cpp-qt5-qhttpengine-server";

        // CLI options
        addOption(CPP_NAMESPACE, CPP_NAMESPACE_DESC, this.cppNamespace);

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties().put("prefix", PREFIX);

        // Write defaults namespace in properties so that it can be accessible in templates.
        // At this point command line has not been parsed so if value is given
        // in command line it will supersede this content
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
        nonFrameworkPrimitives.addAll(languageSpecificPrimitives);
        
        foundationClasses.addAll(
                Arrays.asList(
                        "QString",                          
                        "QDate",
                        "QDateTime",
                        "QByteArray")
        );
        languageSpecificPrimitives.addAll(foundationClasses);
        
        supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder + MODEL_DIR, PREFIX + "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder + MODEL_DIR, PREFIX + "Helpers.cpp"));
        supportingFiles.add(new SupportingFile("object.mustache", sourceFolder + MODEL_DIR, PREFIX + "Object.h"));        
        supportingFiles.add(new SupportingFile("apirouter.h.mustache", sourceFolder + APIHANDLER_DIR, PREFIX + "ApiRouter.h"));
        supportingFiles.add(new SupportingFile("apirouter.cpp.mustache", sourceFolder + APIHANDLER_DIR, PREFIX + "ApiRouter.cpp"));


        supportingFiles.add(new SupportingFile("main.cpp.mustache", sourceFolder + SRC_DIR, "main.cpp"));
        supportingFiles.add(new SupportingFile("src-CMakeLists.txt.mustache", sourceFolder + SRC_DIR, "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("README.md.mustache", sourceFolder, "README.MD"));
        supportingFiles.add(new SupportingFile("Makefile.mustache", sourceFolder, "Makefile"));
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", sourceFolder, "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", sourceFolder, "Dockerfile"));
        supportingFiles.add(new SupportingFile("LICENSE.txt.mustache", sourceFolder, "LICENSE.txt"));

        super.typeMapping = new HashMap<String, String>();

        typeMapping.put("date", "QDate");
        typeMapping.put("DateTime", "QDateTime");
        typeMapping.put("string", "QString");
        typeMapping.put("integer", "qint32");
        typeMapping.put("long", "qint64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "QList");
        typeMapping.put("map", "QMap");
        typeMapping.put("object", PREFIX + "Object");
        // mapped as "file" type for OAS 3.0
        typeMapping.put("ByteArray", "QByteArray");
        //   UUID support - possible enhancement : use QUuid instead of QString.
        //   beware though that Serialization/deserialization of QUuid does not
        //   come out of the box and will need to be sorted out (at least imply
        //   modifications on multiple templates)
        typeMapping.put("UUID", "QString");
        typeMapping.put("file", "QIODevice");
        typeMapping.put("binary", "QIODevice");
        importMapping = new HashMap<String, String>();
        namespaces = new HashMap<String, String>();



        systemIncludes.add("QString");
        systemIncludes.add("QList");
        systemIncludes.add("QMap");
        systemIncludes.add("QDate");
        systemIncludes.add("QDateTime");
        systemIncludes.add("QByteArray");
        systemIncludes.add("QIODevice");
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
            supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder + MODEL_DIR, modelNamePrefix + "Helpers.h"));
            supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder + MODEL_DIR, modelNamePrefix + "Helpers.cpp"));
            supportingFiles.add(new SupportingFile("object.mustache", sourceFolder + MODEL_DIR, modelNamePrefix + "Object.h"));
            supportingFiles.add(new SupportingFile("apirouter.h.mustache", sourceFolder + APIHANDLER_DIR, modelNamePrefix + "ApiRouter.h"));
            supportingFiles.add(new SupportingFile("apirouter.cpp.mustache", sourceFolder + APIHANDLER_DIR, modelNamePrefix + "ApiRouter.cpp"));            
                      
            supportingFiles.add(new SupportingFile("main.cpp.mustache", sourceFolder + SRC_DIR, "main.cpp"));
            supportingFiles.add(new SupportingFile("src-CMakeLists.txt.mustache", sourceFolder + SRC_DIR, "CMakeLists.txt"));
            supportingFiles.add(new SupportingFile("README.md.mustache", sourceFolder, "README.MD"));
            supportingFiles.add(new SupportingFile("Makefile.mustache", sourceFolder, "Makefile"));
            supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", sourceFolder, "CMakeLists.txt"));
            supportingFiles.add(new SupportingFile("Dockerfile.mustache", sourceFolder, "Dockerfile"));
            supportingFiles.add(new SupportingFile("LICENSE.txt.mustache", sourceFolder, "LICENSE.txt"));

            typeMapping.put("object", modelNamePrefix + "Object");
            additionalProperties().put("prefix", modelNamePrefix);
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
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "cpp-qt5-qhttpengine-server";
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Qt5 C++ Server using the QHTTPEngine HTTP Library.";
    }

    @Override
    public String toModelImport(String name) {
        if( name.isEmpty() ) {
            return null;
        }

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
     * Location to write model files.  You can use the modelPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + MODEL_DIR + "/" + modelPackage().replace("::", File.separator);
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + APIHANDLER_DIR + "/" + apiPackage().replace("::", File.separator);
    }

    private String requestFileFolder() {
        return outputFolder + "/" + sourceFolder + APIREQUEST_DIR + "/" + apiPackage().replace("::", File.separator);
    }
    
    @Override
    public String toModelFilename(String name) {
        return modelNamePrefix + initialCaps(name);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if (templateName.contains("apirequest")) {
            result = result.replace("ApiHandler", "ApiRequest");
            result = result.replace(apiFileFolder(), requestFileFolder());
        }
        return result;
    }

    @Override
    public String toApiFilename(String name) {
        return modelNamePrefix + initialCaps(name) + "ApiHandler";
    }

    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
    @Override
    @SuppressWarnings("rawtypes")
    public String getTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return getSchemaType(p) + "<QString, " + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isBinarySchema(p)) {
            return getSchemaType(p) + "*";
        } else if (ModelUtils.isFileSchema(p)) {
            return getSchemaType(p) + "*";
        }
        if (foundationClasses.contains(openAPIType)) {
            return openAPIType;
        } else if (languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        } else {
            return openAPIType;
        }
    }

    @Override
    @SuppressWarnings("rawtypes")    
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
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return "QMap<QString, " + getTypeDeclaration(inner) + ">()";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return "QList<" + getTypeDeclaration(inner) + ">()";
        } else if (ModelUtils.isStringSchema(p)) {
            return "QString(\"\")";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            return toModelName(ModelUtils.getSimpleRef(p.get$ref())) + "()";
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
    @SuppressWarnings("rawtypes")    
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
    public String toVarName(String name) {
        // sanitize name
        String varName = name;
        varName = sanitizeName(name); 

        // if it's all uppper case, convert to lower case
        if (varName.matches("^[A-Z_]*$")) {
            varName = varName.toLowerCase(Locale.ROOT);
        }

        // camelize (lower first character) the variable name
        // petId => pet_id
        varName = org.openapitools.codegen.utils.StringUtils.underscore(varName);

        // for reserved word or word starting with number, append _
        if (isReservedWord(varName) || varName.matches("^\\d.*")) {
            varName = escapeReservedWord(varName);
        }

        return varName;
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String getTypeDeclaration(String str) {
        return str;
    }
      
    @Override
    protected boolean needToImport(String type) {
        return StringUtils.isNotBlank(type) && !defaultIncludes.contains(type)
                && !nonFrameworkPrimitives.contains(type);
    }
   
    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        for (CodegenOperation operation : operations) {
            // Check all return parameter baseType if there is a necessity to include, include it if not 
            // already done
            if (operation.returnBaseType != null && needToImport(operation.returnBaseType)) {
                if(!isIncluded(operation.returnBaseType, imports)) {
                    imports.add(createMapping("import", operation.returnBaseType));
                }
            }
            List<CodegenParameter> params = new ArrayList<CodegenParameter>();
            if (operation.allParams != null)params.addAll(operation.allParams);

            // Check all parameter baseType if there is a necessity to include, include it if not 
            // already done
            for(CodegenParameter param : params) {
                if(param.isPrimitiveType && needToImport(param.baseType)) {
                    if(!isIncluded(param.baseType, imports)) {
                        imports.add(createMapping("import", param.baseType));
                    }
                }
            }
            if (operation.pathParams != null) {
                // We use QString to pass path params, add it to include
                if(!isIncluded("QString", imports)) {
                    imports.add(createMapping("import", "QString"));
                }               
            }
        }
        if(isIncluded("QMap", imports)) {
            // Maps uses QString as key
            if(!isIncluded("QString", imports)) {
                imports.add(createMapping("import", "QString"));
            }
        }
        return objs;
    }
        
    public Map<String, String> createMapping(String key, String value) {
        Map<String, String> customImport = new HashMap<String, String>();
        customImport.put(key, toModelImport(value));
        return customImport;
    }
    
    private boolean isIncluded(String type, List<Map<String, String>> imports) {
        boolean included = false;
        String inclStr = toModelImport(type);
        for (Map<String, String> importItem : imports) {
            if(importItem.containsValue(inclStr)) {
                included = true;
                break;
            }
        }
        return included;
    }
}
