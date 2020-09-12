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

import com.google.common.base.Strings;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static org.openapitools.codegen.utils.OnceLogger.once;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public abstract class AbstractApexCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractApexCodegen.class);

    protected Boolean serializableModel = false;

    public AbstractApexCodegen() {
        super();
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "apex";
    }

    @Override
    public String getHelp() {
        return "Generates an Apex API client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String sanitizeName(String name) {
        name = super.sanitizeName(name);
        if (name.contains("__")) { // Preventing namespacing
            name = name.replaceAll("__", "_");
        }
        if (name.matches("^\\d.*")) {  // Prevent named credentials with leading number
            name = name.replaceAll("^\\d.*", "");
        }
        return name;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        if (name.toLowerCase(Locale.ROOT).matches("^_*class$")) {
            return "propertyClass";
        }

        if ("_".equals(name)) {
            name = "_u";
        }

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            if (isReservedWord(name)) {
                name = escapeReservedWord(name);
            }
            return name;
        }

        if (startsWithTwoUppercaseLetters(name)) {
            name = name.substring(0, 2).toLowerCase(Locale.ROOT) + name.substring(2);
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    private boolean startsWithTwoUppercaseLetters(String name) {
        boolean startsWithTwoUppercaseLetters = false;
        if (name.length() > 1) {
            startsWithTwoUppercaseLetters = name.substring(0, 2).equals(name.substring(0, 2).toUpperCase(Locale.ROOT));
        }
        return startsWithTwoUppercaseLetters;
    }

    @Override
    public String toParamName(String name) {
        // to avoid conflicts with 'callback' parameter for async call
        if ("callback".equals(name)) {
            return "paramCallback";
        }

        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(final String name) {

        final String sanitizedName = sanitizeName(name);

        String nameWithPrefixSuffix = sanitizedName;
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            // add '_' so that model name can be camelized correctly
            nameWithPrefixSuffix = modelNamePrefix + "_" + nameWithPrefixSuffix;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            // add '_' so that model name can be camelized correctly
            nameWithPrefixSuffix = nameWithPrefixSuffix + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        final String camelizedName = camelize(nameWithPrefixSuffix);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            final String modelName = "Model" + camelizedName;
            LOGGER.warn(camelizedName + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (camelizedName.matches("^\\d.*")) {
            final String modelName = "Model" + camelizedName; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
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
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            if (inner == null) {
                LOGGER.warn(ap.getName() + "(array property) does not have a proper inner type defined");
                // TODO maybe better defaulting to StringProperty than returning null
                return null;
            }
            return getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);

            if (inner == null) {
                LOGGER.warn(p.getName() + "(map property) does not have a proper inner type defined");
                // TODO maybe better defaulting to StringProperty than returning null
                return null;
            }
            return getSchemaType(p) + "<String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getAlias(String name) {
        if (typeAliases != null && typeAliases.containsKey(name)) {
            return typeAliases.get(name);
        }
        return name;
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            final ArraySchema ap = (ArraySchema) p;
            final String pattern = "new ArrayList<%s>()";
            if (ap.getItems() == null) {
                return null;
            }

            return String.format(Locale.ROOT, pattern, getTypeDeclaration(ap.getItems()));
        } else if (ModelUtils.isMapSchema(p)) {
            final MapSchema ap = (MapSchema) p;
            final String pattern = "new HashMap<%s>()";
            if (getAdditionalProperties(ap) == null) {
                return null;
            }

            return String.format(Locale.ROOT, pattern, String.format(Locale.ROOT, "String, %s", getTypeDeclaration(getAdditionalProperties(ap))));
        } else if (ModelUtils.isLongSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString() + "l";
            }
            return "null";
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            return "null";
        } else if (ModelUtils.isFloatSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString() + "f";
            }
            return "null";
        } else if (ModelUtils.isDoubleSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString() + "d";
            }
            return "null";
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            return "null";
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                String _default = (String) p.getDefault();
                if (p.getEnum() == null) {
                    return "\"" + escapeText(_default) + "\"";
                } else {
                    // convert to enum var name later in postProcessModels
                    return _default;
                }
            }
            return "null";
        }
        return super.toDefaultValue(p);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {

        if (Boolean.TRUE.equals(p.isLong)) {
            p.example = "2147483648L";
        } else if (Boolean.TRUE.equals(p.isFile)) {
            p.example = "Blob.valueOf('Sample text file\\nContents')";
        } else if (Boolean.TRUE.equals(p.isDate)) {
            p.example = "Date.newInstance(1960, 2, 17)";
        } else if (Boolean.TRUE.equals(p.isDateTime)) {
            p.example = "Datetime.newInstanceGmt(2013, 11, 12, 3, 3, 3)";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            if (p.items != null && p.items.example != null) {
                p.example = "new " + p.dataType + "{" + p.items.example + "}";
            }
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            if (p.items != null && p.items.example != null) {
                p.example = "new " + p.dataType + "{" + p.items.example + "}";
            }
        } else if (Boolean.TRUE.equals(p.isString)) {
            p.example = "'" + p.example + "'";
        } else if ("".equals(p.example) || p.example == null && "Object".equals(p.dataType)) {
            // Get an example object from the generated model
            if (!isReservedWord(p.dataType.toLowerCase(Locale.ROOT))) {
                p.example = p.dataType + ".getExample()";
            }
        } else {
            p.example = "''";
        }

    }

    @Override
    public String toExampleValue(Schema p) {
        if (p == null) {
            return "";
        }

        Object obj = p.getExample();
        String example = obj == null ? "" : obj.toString();

        if (ModelUtils.isArraySchema(p)) {
            example = "new " + getTypeDeclaration(p) + "{" + toExampleValue(
                    ((ArraySchema) p).getItems()) + "}";
        } else if (ModelUtils.isBooleanSchema(p)) {
            example = String.valueOf(!"false".equals(example));
        } else if (ModelUtils.isByteArraySchema(p)) {
            if (example.isEmpty()) {
                example = "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wZWQgb3ZlciB0aGUgbGF6eSBkb2cu";
            }
            p.setExample(example);
            example = "EncodingUtil.base64Decode('" + example + "')";
        } else if (ModelUtils.isDateSchema(p)) {
            if (example.matches("^\\d{4}(-\\d{2}){2}")) {
                example = example.substring(0, 10).replaceAll("-0?", ", ");
            } else if (example.isEmpty()) {
                example = "2000, 1, 23";
            } else {
                LOGGER.warn(String.format(Locale.ROOT, "The example provided for property '%s' is not a valid RFC3339 date. Defaulting to '2000-01-23'. [%s]", p
                        .getName(), example));
                example = "2000, 1, 23";
            }
            example = "Date.newInstance(" + example + ")";
        } else if (ModelUtils.isDateTimeSchema(p)) {
            if (example.matches("^\\d{4}([-T:]\\d{2}){5}.+")) {
                example = example.substring(0, 19).replaceAll("[-T:]0?", ", ");
            } else if (example.isEmpty()) {
                example = "2000, 1, 23, 4, 56, 7";
            } else {
                LOGGER.warn(String.format(Locale.ROOT, "The example provided for property '%s' is not a valid RFC3339 datetime. Defaulting to '2000-01-23T04-56-07Z'. [%s]", p
                        .getName(), example));
                example = "2000, 1, 23, 4, 56, 7";
            }
            example = "Datetime.newInstanceGmt(" + example + ")";
        } else if (ModelUtils.isNumberSchema(p)) {
            example = example.replaceAll("[^-0-9.]", "");
            example = example.isEmpty() ? "1.3579" : example;
        } else if (ModelUtils.isFileSchema(p)) {
            if (example.isEmpty()) {
                example = "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wZWQgb3ZlciB0aGUgbGF6eSBkb2cu";
                p.setExample(example);
            }
            example = "EncodingUtil.base64Decode(" + example + ")";
        } else if (ModelUtils.isEmailSchema(p)) {
            if (example.isEmpty()) {
                example = "example@example.com";
                p.setExample(example);
            }
            example = "'" + example + "'";
        } else if (ModelUtils.isLongSchema(p)) {
            example = example.isEmpty() ? "123456789L" : example + "L";
        } else if (ModelUtils.isMapSchema(p)) {
            example = "new " + getTypeDeclaration(p) + "{'key'=>" + toExampleValue(getAdditionalProperties(p)) + "}";

        } else if (ModelUtils.isPasswordSchema(p)) {
            example = example.isEmpty() ? "password123" : escapeText(example);
            p.setExample(example);
            example = "'" + example + "'";
        } else if (ModelUtils.isStringSchema(p)) {
            List<String> enums = p.getEnum();
            if (enums != null && example.isEmpty()) {
                example = enums.get(0);
                p.setExample(example);
            } else if (example.isEmpty()) {
                example = "";
            } else {
                example = escapeText(example);
                p.setExample(example);
            }
            example = "'" + example + "'";
        } else if (ModelUtils.isUUIDSchema(p)) {
            example = example.isEmpty()
                    ? "'046b6c7f-0b8a-43b9-b35d-6489e6daee91'"
                    : "'" + escapeText(example) + "'";
        } else if (ModelUtils.isIntegerSchema(p)) {
            example = example.matches("^-?\\d+$") ? example : "0";
        } else if (ModelUtils.isObjectSchema(p)) {
            example = example.isEmpty() ? "null" : example;
        } else {
            example = getTypeDeclaration(p) + ".getExample()";
        }
        return example;
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);

        schemaType = getAlias(schemaType);

        // don't apply renaming on types from the typeMapping
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }

        if (null == schemaType) {
            LOGGER.error("No Type defined for Property " + p);
        }
        return toModelName(schemaType);
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
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel cm = super.fromModel(name, model);

        // TODO Check enum model handling
        if (cm.interfaces == null) {
            cm.interfaces = new ArrayList<String>();
        }

        Boolean hasDefaultValues = false;

        // for (de)serializing properties renamed for Apex (e.g. reserved words)
        List<Map<String, String>> propertyMappings = new ArrayList<>();
        for (CodegenProperty p : cm.allVars) {
            hasDefaultValues |= p.defaultValue != null;
            if (!p.baseName.equals(p.name)) {
                Map<String, String> mapping = new HashMap<>();
                mapping.put("externalName", p.baseName);
                mapping.put("internalName", p.name);
                propertyMappings.add(mapping);
            }
        }

        cm.vendorExtensions.put("x-has-property-mappings", !propertyMappings.isEmpty());
        cm.vendorExtensions.put("x-has-default-values", hasDefaultValues);
        cm.vendorExtensions.put("x-property-mappings", propertyMappings);

        if (!propertyMappings.isEmpty()) {
            cm.interfaces.add("OAS.MappedProperties");
        }
        return cm;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        if (parameter.isBodyParam && parameter.isListContainer) {
            // items of array bodyParams are being nested an extra level too deep for some reason
            parameter.items = parameter.items.items;
            setParameterExampleValue(parameter);
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(objs);
    }

    /* the following function is not used anywhere in this class so we'll remove it later
    private static String getAccept(Operation operation) {
        String accepts = null;
        String defaultContentType = "application/json";
        if (operation.getProduces() != null && !operation.getProduces().isEmpty()) {
            StringBuilder sb = new StringBuilder();
            for (String produces : operation.getProduces()) {
                if (defaultContentType.equalsIgnoreCase(produces)) {
                    accepts = defaultContentType;
                    break;
                } else {
                    if (sb.length() > 0) {
                        sb.append(",");
                    }
                    sb.append(produces);
                }
            }
            if (accepts == null) {
                accepts = sb.toString();
            }
        } else {
            accepts = defaultContentType;
        }

        return accepts;
    }*/

    @Override
    protected boolean needToImport(String type) {
        return super.needToImport(type) && type.indexOf(".") < 0;
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return sanitizeName(camelize(property.name)) + "Enum";
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (value.length() == 0) {
            return "EMPTY";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(value) != null) {
            return getSymbolName(value).toUpperCase(Locale.ROOT);
        }

        // number
        if ("Integer".equals(datatype) || "Long".equals(datatype) ||
                "Float".equals(datatype) || "Double".equals(datatype)) {
            String varName = "NUMBER_" + value;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String var = value.replaceAll("\\W+", "_").toUpperCase(Locale.ROOT);
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("Integer".equals(datatype) || "Double".equals(datatype)) {
            return value;
        } else if ("Long".equals(datatype)) {
            // add l to number, e.g. 2048 => 2048l
            return value + "l";
        } else if ("Float".equals(datatype)) {
            // add f to number, e.g. 3.14 => 3.14f
            return value + "f";
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {

        CodegenOperation op = super.fromOperation(
                path, httpMethod, operation, null);

        if (op.getHasExamples()) {
            // prepare examples for Apex test classes
            ApiResponse apiResponse = findMethodResponse(operation.getResponses());
            final Schema responseSchema = ModelUtils.getSchemaFromResponse(apiResponse);
            String deserializedExample = toExampleValue(responseSchema);
            for (Map<String, String> example : op.examples) {
                example.put("example", escapeText(example.get("example")));
                example.put("deserializedExample", deserializedExample);
            }
        }

        return op;
    }

    private static CodegenModel reconcileInlineEnums(CodegenModel codegenModel, CodegenModel parentCodegenModel) {
        // This generator uses inline classes to define enums, which breaks when
        // dealing with models that have subTypes. To clean this up, we will analyze
        // the parent and child models, look for enums that match, and remove
        // them from the child models and leave them in the parent.
        // Because the child models extend the parents, the enums will be available via the parent.

        // Only bother with reconciliation if the parent model has enums.
        if (!parentCodegenModel.hasEnums) {
            return codegenModel;
        }

        // Get the properties for the parent and child models
        final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
        List<CodegenProperty> codegenProperties = codegenModel.vars;

        // Iterate over all of the parent model properties
        boolean removedChildEnum = false;
        for (CodegenProperty parentModelCodegenPropery : parentModelCodegenProperties) {
            // Look for enums
            if (parentModelCodegenPropery.isEnum) {
                // Now that we have found an enum in the parent class,
                // and search the child class for the same enum.
                Iterator<CodegenProperty> iterator = codegenProperties.iterator();
                while (iterator.hasNext()) {
                    CodegenProperty codegenProperty = iterator.next();
                    if (codegenProperty.isEnum && codegenProperty.equals(parentModelCodegenPropery)) {
                        // We found an enum in the child class that is
                        // a duplicate of the one in the parent, so remove it.
                        iterator.remove();
                        removedChildEnum = true;
                    }
                }
            }
        }

        if (removedChildEnum) {
            // If we removed an entry from this model's vars, we need to ensure hasMore is updated
            int count = 0, numVars = codegenProperties.size();
            for (CodegenProperty codegenProperty : codegenProperties) {
                count += 1;
                codegenProperty.hasMore = (count < numVars) ? true : false;
            }
            codegenModel.vars = codegenProperties;
        }
        return codegenModel;
    }

    private static String sanitizePackageName(String packageName) {
        packageName = packageName.trim(); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        packageName = packageName.replaceAll("[^a-zA-Z0-9_\\.]", "_");
        if (Strings.isNullOrEmpty(packageName)) {
            return "invalidPackageName";
        }
        return packageName;
    }


    public void setSerializableModel(Boolean serializableModel) {
        this.serializableModel = serializableModel;
    }

    private String sanitizePath(String p) {
        //prefer replace a ", instead of a fuLL URL encode for readability
        return p.replaceAll("\"", "%22");
    }

    public String toRegularExpression(String pattern) {
        return escapeText(pattern);
    }

    @Override
    public String sanitizeTag(String tag) {
        return camelize(sanitizeName(tag));
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelName(name) + "Test";
    }

}
