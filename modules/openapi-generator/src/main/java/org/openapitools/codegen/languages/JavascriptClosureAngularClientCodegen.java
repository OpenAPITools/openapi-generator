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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class JavascriptClosureAngularClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(JavascriptClosureAngularClientCodegen.class);

    public static final String USE_ES6 = "useEs6";

    protected boolean useEs6;

    public JavascriptClosureAngularClientCodegen() {
        super();
        outputFolder = "generated-code/javascript-closure-angular";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        supportsInheritance = false;
        setReservedWordsLowerCase(Arrays.asList("abstract",
            "continue", "for", "new", "switch", "assert", "default", "if",
            "package", "synchronized", "do", "goto", "private",
            "this", "break", "double", "implements", "protected", "throw",
            "byte", "else", "import", "public", "throws", "case", "enum",
            "instanceof", "return", "transient", "catch", "extends", "int",
            "short", "try", "char", "final", "interface", "static", "void",
            "class", "finally", "const", "super", "while"));

        languageSpecificPrimitives = new HashSet<String>(Arrays.asList(
            "string",
            "boolean",
            "number",
            "Object",
            "Blob",
            "Date"));
        instantiationTypes.put("array", "Array");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("Array", "Array");
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "number");
        typeMapping.put("float", "number");
        typeMapping.put("number", "number");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("char", "string");
        typeMapping.put("double", "number");
        typeMapping.put("object", "Object");
        typeMapping.put("Object", "Object");
        typeMapping.put("File", "Blob");
        typeMapping.put("file", "Blob");
        typeMapping.put("integer", "number");
        typeMapping.put("Map", "Object");
        typeMapping.put("map", "Object");
        typeMapping.put("DateTime", "Date");

        importMapping = new HashMap<String, String>();
        defaultIncludes = new HashSet<String>(Arrays.asList(
            "Object",
            "Array",
            "Blob"
        ));

        typeMapping.put("binary", "string");

        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(USE_ES6,
                "use ES6 templates")
                .defaultValue(Boolean.FALSE.toString()));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(USE_ES6)) {
            setUseEs6(convertPropertyToBooleanAndWriteBack(USE_ES6));
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        if (useEs6) {
            embeddedTemplateDir = templateDir = "Javascript-Closure-Angular/es6";
            apiPackage = "resources";
            apiTemplateFiles.put("api.mustache", ".js");
            supportingFiles.add(new SupportingFile("module.mustache", "", "module.js"));
        } else {
            modelTemplateFiles.put("model.mustache", ".js");
            apiTemplateFiles.put("api.mustache", ".js");
            embeddedTemplateDir = templateDir = "Javascript-Closure-Angular";
            apiPackage = "API.Client";
            modelPackage = "API.Client";
        }
    }

    @Override
    public String getName() {
        return "javascript-closure-angular";
    }

    @Override
    public String getHelp() {
        return "Generates a Javascript AngularJS client library (beta) annotated with Google Closure Compiler annotations" +
            "(https://developers.google.com/closure/compiler/docs/js-for-compiler?hl=en)";
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
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
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    public String modelFileFolder() {
        return outputFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        // camelize the variable name
        // pet_id => PetId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*"))
            name = escapeReservedWord(name);

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
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
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "<!" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return "Object<!string, "+ getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isFileSchema(p)) {
            return "Object";
        }
        String type = super.getTypeDeclaration(p);
        if (type.equals("boolean") ||
                type.equals("Date") ||
                type.equals("number") ||
                type.equals("string")) {
            return type;
                }
        return apiPackage + "." + type;
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else
            type = schemaType;
        return type;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {

        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
            for (CodegenProperty var : cm.vars) {
                // handle default value for enum, e.g. available => StatusEnum.available
                if (var.isEnum && var.defaultValue != null && !"null".equals(var.defaultValue)) {
                    var.defaultValue = var.datatypeWithEnum + "." + var.defaultValue;
                }
            }
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        if (objs.get("imports") instanceof List) {
            List<Map<String, String>> imports = (ArrayList<Map<String, String>>)objs.get("imports");
            Collections.sort(imports, new Comparator<Map<String, String>>() {
                public int compare(Map<String, String> o1, Map<String, String> o2) {
                    return o1.get("import").compareTo(o2.get("import"));
                }
            });
            objs.put("imports", imports);
        }
        return objs;
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
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        return operationId;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ', " to avoid code injection
        return input.replace("\"", "").replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    public void setUseEs6(boolean useEs6) {
        this.useEs6 = useEs6;
    }
}
