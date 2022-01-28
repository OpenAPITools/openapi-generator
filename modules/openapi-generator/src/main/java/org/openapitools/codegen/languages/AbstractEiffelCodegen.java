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

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public abstract class AbstractEiffelCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(AbstractEiffelCodegen.class);

    private final Set<String> parentModels = new HashSet<>();
    private final Multimap<String, CodegenModel> childrenByParent = ArrayListMultimap.create();

    public AbstractEiffelCodegen() {
        super();

        hideGenerationTimestamp = Boolean.FALSE;

        setReservedWordsLowerCase(Arrays.asList(
                // language reserved words
                "across", "agent", "alias", "all", "and", "as", "assign", "attached", "attribute", "check", "class", "convert",
                "create", "Current", "debug", "deferred", "detachable", "do", "else", "elseif", "end", "ensure", "expanded", "export",
                "external", "False", "feature", "from", "frozen", "if", "implies", "inherit", "inspect", "invariant",
                "like", "local", "loop", "not", "note", "obsolete", "old", "once", "only", "or", "Precursor",
                "redefine", "rename", "require", "rescue", "Result", "retry", "select", "separate", "then", "True",
                "TUPLE", "undefine", "until", "variant", "Void", "when", "xor"));

        defaultIncludes = new HashSet<>(Arrays.asList("map", "array"));

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("BOOLEAN", "INTEGER_8", "INTEGER_16", "INTEGER_32", "INTEGER_64", "NATURAL_8",
                        "NATURAL_16", "NATURAL_32", "NATURAL_64", "REAL_32", "REAL_64"));

        instantiationTypes.clear();

        typeMapping.clear();
        typeMapping.put("integer", "INTEGER_32");
        typeMapping.put("long", "INTEGER_64");
        typeMapping.put("number", "REAL_32");
        typeMapping.put("float", "REAL_32");
        typeMapping.put("decimal", "REAL_64");
        typeMapping.put("double", "REAL_64");
        typeMapping.put("boolean", "BOOLEAN");
        typeMapping.put("string", "STRING_32");
        typeMapping.put("UUID", "UUID"); //
        typeMapping.put("URI", "STRING"); //
        typeMapping.put("date", "DATE");
        typeMapping.put("DateTime", "DATE_TIME");
        typeMapping.put("date-time", "DATE_TIME");
        typeMapping.put("password", "STRING");
        typeMapping.put("File", "FILE");
        typeMapping.put("file", "FILE");
        typeMapping.put("binary", "FILE");
        typeMapping.put("ByteArray", "ARRAY [NATURAL_8]");
        typeMapping.put("object", "ANY");
        typeMapping.put("map", "STRING_TABLE");
        typeMapping.put("array", "LIST");
        typeMapping.put("list", "LIST");
        typeMapping.put("AnyType", "ANY");

        instantiationTypes.put("array", "ARRAYED_LIST");
        instantiationTypes.put("list", "ARRAYED_LIST");
        instantiationTypes.put("map", "STRING_TABLE");

        importMapping.put("List", "LIST");
        importMapping.put("Set", "SET");
        importMapping.put("file", "FILE");
        importMapping.put("File", "FILE");
        importMapping.put("Map", "STRING_TABLE");


        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Eiffel Cluster name (convention: lowercase).")
                .defaultValue("openapi"));
        cliOptions
                .add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Eiffel package version.").defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC).defaultValue(Boolean.TRUE.toString()));
    }

    @Override
    public String escapeReservedWord(String name) {
        // Can't start with an underscore, as our fields need to start with an
        // UppercaseLetter so that Go treats them as public/visible.

        // Options?
        // - MyName
        // - AName
        // - TheName
        // - XName
        // - X_Name
        // ... or maybe a suffix?
        // - Name_ ... think this will work.
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        if (name.matches("^\\d.*")) {// prepend var_
            return "var_" + name;
        }
        return "var_" + name;
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // pet_id
        // petId  => pet_id
        name = unCamelize(name);

        if (name.startsWith("_")) {
            name = "var" + name;
        }

        // for reserved word
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // for reserved word or word starting with number, append
        if (name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // params should be lowercase. E.g. "person: PERSON"
        return toVarName(name).toLowerCase(Locale.ROOT);
    }

    @Override
    public String toModelName(String name) {
        // phone_number => PHONE_NUMBER
        return toModelFilename(name).toUpperCase(Locale.ROOT);
    }

    @Override
    public String toModelFilename(String name) {
        // We need to check if import-mapping has a different model for this class, so we use it
        // instead of the auto-generated one.
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, "model_" + name);
            name = "model_" + name; // e.g. return => ModelReturn (after
            // camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    "model_" + name);
            name = "model_" + name; // e.g. 200Response => Model200Response
            // (after camelize)
        }
        // model name starts with _
        if (name.startsWith("_")) {
            LOGGER.warn("{} (model name starts with _) cannot be used as model name. Renamed to {}", name,
                    "model" + name);
            name = "model" + name; // e.g. 200Response => Model200Response
            // (after camelize)
        }

        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be
        // assigned. Also declare the
        // methods parameters as 'final'.

        // e.g. PetApi.go => pet_api.go
        return underscore(name) + "_api";
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiName(name).toLowerCase(Locale.ROOT) + "_test";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DEFAULT_API";
        }
        return name.toUpperCase(Locale.ROOT) + "_API";
    }

    /**
     * Overrides postProcessParameter to add a vendor extension
     * "x-exportParamName". This is useful when paramName starts with a
     * lowercase letter, but we need that param to be exportable (starts with an
     * Uppercase letter).
     *
     * @param parameter CodegenParameter object to be processed.
     */
    @Override
    public void postProcessParameter(CodegenParameter parameter) {

        // Give the base class a chance to process
        super.postProcessParameter(parameter);

        char firstChar = parameter.paramName.charAt(0);

        if (Character.isUpperCase(firstChar)) {
            // First char is already uppercase, just use paramName.
            parameter.vendorExtensions.put("x-export-param-name", parameter.paramName);
        }

        // It's a lowercase first char, let's convert it to uppercase
        StringBuilder sb = new StringBuilder(parameter.paramName);
        sb.setCharAt(0, Character.toUpperCase(firstChar));
        parameter.vendorExtensions.put("x-export-param-name", sb.toString());
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if (StringUtils.isNotBlank(model.parent)) {
            parentModels.add(model.parent);
            if (!childrenByParent.containsEntry(model.parent, model)) {
                childrenByParent.put(model.parent, model);
            }
        }
        if (StringUtils.isNotBlank(model.parentSchema)) {
            model.parentSchema = model.parentSchema.toLowerCase(Locale.ROOT);
        }
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return "LIST [" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);

            return getSchemaType(p) + " [" + getTypeDeclaration(inner) + "]";
        }
        // return super.getTypeDeclaration(p);

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String schemaType = getSchemaType(p);
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }

        if (typeMapping.containsValue(schemaType)) {
            return schemaType;
        }

        if (languageSpecificPrimitives.contains(schemaType)) {
            return schemaType;
        }

        return toModelName(schemaType);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type))
                return (type);
        } else
            type = schemaType;
        return type;
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");
        }

        String sanitizedOperationId = camelize(sanitizeName(operationId), true);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(sanitizedOperationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, camelize("call_" + operationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn(operationId + " (starting with a number) cannot be used as method sname. Renamed to " + camelize("call_" + operationId), true);
            sanitizedOperationId = camelize("call_" + sanitizedOperationId, true);
        }

        // method name from updateSomething to update_Something.
        sanitizedOperationId = unCamelize(sanitizedOperationId);

        return toEiffelFeatureStyle(sanitizedOperationId);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            // http method verb conversion (e.g. PUT => Put)

            operation.httpMethod = camelize(operation.httpMethod.toLowerCase(Locale.ROOT));
        }

        // remove model imports to avoid error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        if (imports == null)
            return objs;

        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(apiPackage()))
                iterator.remove();
        }
        // if the return type is not primitive, import encoding/json
        for (CodegenOperation operation : operations) {
            if (operation.returnBaseType != null && needToImport(operation.returnBaseType)) {
                imports.add(createMapping("import", "encoding/json"));
                break; // just need to import once
            }
        }

        // this will only import "fmt" if there are items in pathParams
        for (CodegenOperation operation : operations) {
            if (operation.pathParams != null && operation.pathParams.size() > 0) {
                imports.add(createMapping("import", "fmt"));
                break; // just need to import once
            }
        }

        // recursively add import for mapping one type to multiple imports
        List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
        if (recursiveImports == null)
            return objs;

        ListIterator<Map<String, String>> listIterator = imports.listIterator();
        while (listIterator.hasNext()) {
            String _import = listIterator.next().get("import");
            // if the import package happens to be found in the importMapping
            // (key)
            // add the corresponding import package to the list
            if (importMapping.containsKey(_import)) {
                listIterator.add(createMapping("import", importMapping.get(_import)));
            }
        }

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // remove model imports to avoid error
//        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
//        final String prefix = modelPackage();
//        Iterator<Map<String, String>> iterator = imports.iterator();
//        while (iterator.hasNext()) {
//            String _import = iterator.next().get("import");
//            if (_import.startsWith(prefix))
//                iterator.remove();
//        }
//
//        // recursively add import for mapping one type to multiple imports
//        List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
//        if (recursiveImports == null)
//            return objs;
//
//        ListIterator<Map<String, String>> listIterator = imports.listIterator();
//        while (listIterator.hasNext()) {
//            String _import = listIterator.next().get("import");
//            // if the import package happens to be found in the importMapping
//            // (key)
//            // add the corresponding import package to the list
//            if (importMapping.containsKey(_import)) {
//                listIterator.add(createMapping("import", importMapping.get(_import)));
//            }
//        }
//
//        return objs;
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessAllModels(final Map<String, Object> models) {

        final Map<String, Object> processed = super.postProcessAllModels(models);
        postProcessParentModels(models);
        return processed;
    }

    private void postProcessParentModels(final Map<String, Object> models) {
        for (final String parent : parentModels) {
            final CodegenModel parentModel = ModelUtils.getModelByName(parent, models);
            final Collection<CodegenModel> childrenModels = childrenByParent.get(parent);
            for (final CodegenModel child : childrenModels) {
                processParentPropertiesInChildModel(parentModel, child);
            }
        }
    }

    /**
     * Sets the child property's isInherited flag to true if it is an inherited
     * property
     */
    private void processParentPropertiesInChildModel(final CodegenModel parent, final CodegenModel child) {
        final Map<String, CodegenProperty> childPropertiesByName = new HashMap<>(child.vars.size());
        for (final CodegenProperty childProperty : child.vars) {
            childPropertiesByName.put(childProperty.name, childProperty);
        }
        if (parent != null) {
            for (final CodegenProperty parentProperty : parent.vars) {
                final CodegenProperty duplicatedByParent = childPropertiesByName.get(parentProperty.name);
                if (duplicatedByParent != null) {
                    duplicatedByParent.isInherited = true;
                }
            }
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        CodegenModel codegenModel = super.fromModel(name, model);
        if (allDefinitions != null && codegenModel.parentSchema != null && codegenModel.hasEnums) {
            final Schema parentModel = allDefinitions.get(codegenModel.parentSchema);
            final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent, parentModel);
            codegenModel = AbstractEiffelCodegen.reconcileInlineEnums(codegenModel, parentCodegenModel);
        }
        return codegenModel;
    }

    private static CodegenModel reconcileInlineEnums(CodegenModel codegenModel, CodegenModel parentCodegenModel) {
        // This generator uses inline classes to define enums, which breaks when
        // dealing with models that have subTypes. To clean this up, we will analyze
        // the parent and child models, look for enums that match, and remove
        // them from the child models and leave them in the parent.
        // Because the child models extend the parents, the enums will be available via the parent.

        // Only bother with reconciliation if the parent model has enums.
        if (parentCodegenModel == null || !parentCodegenModel.hasEnums) {
            return codegenModel;
        }

        // Get the properties for the parent and child models
        final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
        List<CodegenProperty> codegenProperties = codegenModel.vars;

        // Iterate over all of the parent model properties
        boolean removedChildEnum = false;
        for (CodegenProperty parentModelCodegenProperty : parentModelCodegenProperties) {
            // Look for enums
            if (parentModelCodegenProperty.isEnum) {
                // Now that we have found an enum in the parent class,
                // and search the child class for the same enum.
                Iterator<CodegenProperty> iterator = codegenProperties.iterator();
                while (iterator.hasNext()) {
                    CodegenProperty codegenProperty = iterator.next();
                    if (codegenProperty.isEnum && codegenProperty.equals(parentModelCodegenProperty)) {
                        // We found an enum in the child class that is
                        // a duplicate of the one in the parent, so remove it.
                        iterator.remove();
                        removedChildEnum = true;
                    }
                }
            }
        }

        if (removedChildEnum) {
            codegenModel.vars = codegenProperties;
        }
        return codegenModel;
    }


    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type) && !languageSpecificPrimitives.contains(type);
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

    public Map<String, String> createMapping(String key, String value) {
        Map<String, String> customImport = new HashMap<>();
        customImport.put(key, value);

        return customImport;
    }

    @Override
    public String toInstantiationType(Schema p) {
        return getTypeDeclaration(p);
//        if (ModelUtils.isMapSchema(p)) {
//            Schema additionalProperties2 = getAdditionalProperties(p);
//            String type = additionalProperties2.getType();
//            if (null == type) {
//                LOGGER.error("No Type defined for Additional Schema " + additionalProperties2 + "\n" //
//                        + "\tIn Schema: " + p);
//            }
//            String inner = toModelName(getSchemaType(additionalProperties2));
//            return instantiationTypes.get("map") + " [" + inner + "]";
//        } else if (ModelUtils.isArraySchema(p)) {
//            ArraySchema ap = (ArraySchema) p;
//            String inner = toModelName(getSchemaType(ap.getItems()));
//            return instantiationTypes.get("array") + " [" + inner + "]";
//        } else {
//            return null;
//        }
    }

    public String unCamelize(String name) {
        return name.replaceAll("(.)(\\p{Upper})", "$1_$2").toLowerCase(Locale.ROOT);
    }

    public String toEiffelFeatureStyle(String operationId) {
        if (operationId.startsWith("get_")) {
            return operationId.substring(4);
        } else {
            return operationId;
        }
    }

    /**
     * Update property for array(list) container
     *
     * @param property      Codegen property
     * @param innerProperty Codegen inner property of map or list
     */
    @Override
    protected void updatePropertyForArray(CodegenProperty property, CodegenProperty innerProperty) {
        if (innerProperty == null) {
            LOGGER.warn("skipping invalid array property {}", Json.pretty(property));
            return;
        }
        property.dataFormat = innerProperty.dataFormat;
        if (!languageSpecificPrimitives.contains(innerProperty.baseType)) {
            property.complexType = innerProperty.baseType;
        } else {
            property.isPrimitiveType = true;
        }
        property.items = innerProperty;
        // inner item is Enum
        if (isPropertyInnerMostEnum(property)) {
            // We use the data type instead of the Enum class.
            // at the moment is not supported.

            // isEnum is set to true when the type is an enum
            // or the inner type of an array/map is an enum
            //property.isEnum = true;
            // update datatypeWithEnum and default value for array
            // e.g. List<string> => List<StatusEnum>
            //updateDataTypeWithEnumForArray(property);
            // set allowable values to enum values (including array/map of enum)
            //property.allowableValues = getInnerEnumAllowableValues(property);
        }

    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.EIFFEL; }
}
