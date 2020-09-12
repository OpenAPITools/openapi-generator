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

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Mustache.Lambda;
import com.samskivert.mustache.Template;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ElmClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ElmClientCodegen.class);

    protected String packageName = "openapi";
    protected String packageVersion = "1.0.0";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "elm";
    }

    public String getHelp() {
        return "Generates an Elm client library.";
    }

    public ElmClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.of(SecurityFeature.BearerToken))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath
                )
        );

        templateDir = "elm";
        apiPackage = "Api.Request";
        modelPackage = "Api";

        supportsInheritance = true;

        reservedWords = new HashSet<>(
                Arrays.asList(
                        "if", "then", "else",
                        "case", "of",
                        "let", "in",
                        "type",
                        "module", "where",
                        "import", "exposing",
                        "as",
                        "port")
        );

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "Order",
                        "Never",
                        "List",
                        "Maybe",
                        "Result",
                        "Program",
                        "Cmd",
                        "Sub")
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "Bool",
                        "Dict",
                        "Float",
                        "Int",
                        "List",
                        "String")
        );

        instantiationTypes.clear();
        instantiationTypes.put("array", "List");
        instantiationTypes.put("map", "Dict");

        typeMapping.clear();
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Int");
        typeMapping.put("number", "Float");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Float");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "String");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Dict");
        typeMapping.put("date", "Posix");
        typeMapping.put("DateTime", "Posix");
        typeMapping.put("password", "String");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("file", "String");
        typeMapping.put("binary", "String");
        typeMapping.put("UUID", "Uuid");
        typeMapping.put("URI", "String");

        importMapping.clear();

        cliOptions.clear();

        apiTemplateFiles.put("operation.mustache", ".elm");
        modelTemplateFiles.put("model.mustache", ".elm");
        supportingFiles.add(new SupportingFile("Api.mustache", "", "src" + File.separator + "Api.elm"));
        supportingFiles.add(new SupportingFile("Time.mustache", "", "src" + File.separator + "Api" + File.separator + "Time.elm"));
        supportingFiles.add(new SupportingFile("elm.mustache", "", "elm.json"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
            .put("removeWhitespace", new RemoveWhitespaceLambda());
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("\"", "");
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "Default";
        }
        return camelize(name);
    }

    @Override
    public String toModelName(String name) {
        final String modelName = camelize(name);
        return defaultIncludes.contains(modelName) ? modelName + "_" : modelName;
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return toModelName(property.name);
    }

    @Override
    public String toVarName(String name) {
        final String varName = camelize(name.replaceAll("[^a-zA-Z0-9_]", ""), true);
        return isReservedWord(varName) ? escapeReservedWord(name) : varName;
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        String camelized = camelize(value.replace(" ", "_").replace("(", "_").replace(")", "")); // TODO FIXME escape properly
        if (camelized.length() == 0) {
            LOGGER.error("Unable to determine enum variable name (name: {}, datatype: {}) from empty string. Default to UnknownEnumVariableName", value, datatype);
            camelized = "UnknownEnumVariableName";
        }
        return camelized;
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            return instantiationTypes.get("array") + " " + inner;
        } else {
            return null;
        }
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + "src" + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + "src" + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String escapeReservedWord(String name) {
        return name + "_";
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if (property.getAllowableValues() != null && !property.getAllowableValues().isEmpty()) {
            property.isModel = true;
        }
    }

    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessAllModels(final Map<String, Object> orgObjs) {
        final Map<String, Object> objs = super.postProcessAllModels(orgObjs);

        // put all models in one file
        final Map<String, Object> objects = new HashMap<>();
        final Map<String, Object> dataObj = objs.values().stream()
            .map(obj -> (Map<String, Object>) obj)
            .findFirst()
            .orElse(new HashMap<>());
        final List<Map<String, Object>> models = objs.values().stream()
            .map(obj -> (Map<String, Object>) obj)
            .flatMap(obj -> ((List<Map<String, Object>>) obj.get("models")).stream())
            .flatMap(obj -> {
                final CodegenModel model = (CodegenModel) obj.get("model");
                // circular references
                model.vars.forEach(var -> {
                    var.isCircularReference = model.allVars.stream()
                        .filter(v -> var.baseName.equals(v.baseName))
                        .map(v -> v.isCircularReference)
                        .findAny()
                        .orElse(false);
                    CodegenProperty items = var.items;
                    while (items != null) {
                        items.isCircularReference = var.isCircularReference;
                        items.required = true;
                        items = items.items;
                    }
                });
                // discriminators
                if (model.discriminator != null && model.getChildren() != null) {
                    model.getChildren().forEach(child -> {
                        child.allOf = child.allOf.stream()
                            .map(v -> model.classname.equals(v) ? "Base" + v : v)
                            .collect(Collectors.toSet());
                    });
                }
                // remove *AllOf
                if (model.classname.endsWith("AllOf")) {
                    return Stream.empty();
                } else {
                    model.allOf.removeIf(name -> name.endsWith("AllOf"));
                    return Stream.of(obj);
                }
            })
            .collect(Collectors.toList());

        final boolean includeTime = anyVarMatches(models, prop -> prop.isDate || prop.isDateTime);
        final boolean includeUuid = anyVarMatches(models, prop -> prop.isUuid);

        dataObj.put("models", models);
        dataObj.put("includeTime", includeTime);
        dataObj.put("includeUuid", includeUuid);
        objects.put("Data", dataObj);
        return objects;
    }

    private boolean anyVarMatches(final List<Map<String, Object>> models, final Predicate<CodegenProperty> predicate) {
        return models.stream()
            .map(obj -> (CodegenModel) obj.get("model"))
            .flatMap(model -> model.vars.stream())
            .filter(var -> {
                CodegenProperty prop = var;
                while (prop != null) {
                    if (predicate.test(prop)) {
                        return true;
                    }
                    prop = prop.items;
                }
                return false;
            })
            .count() > 0;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(objs);
    }

    private static boolean anyOperationParam(final List<CodegenOperation> operations, final Predicate<CodegenParameter> predicate) {
        return operations.stream()
                .flatMap(operation -> operation.allParams.stream())
                .filter(predicate)
                .findAny()
                .isPresent();
    }

    private static boolean anyOperationResponse(final List<CodegenOperation> operations, final Predicate<CodegenResponse> predicate) {
        return operations.stream()
                .flatMap(operation -> operation.responses.stream())
                .filter(predicate)
                .findAny()
                .isPresent();
    }

    @Override
    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> allModels) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>) objs.get("operation");
        ops.forEach(op -> {
            op.allParams = op.allParams.stream().sorted(new ParameterSorter()).collect(Collectors.toList());
            op.responses.forEach(response -> {
                if (response.isDefault) {
                    response.isModel = !response.primitiveType;
                }
            });
        });

        final boolean includeTime =
            anyOperationResponse(ops, response -> response.isDate || response.isDateTime) ||
            anyOperationParam(ops, param -> param.isDate || param.isDateTime);
        final boolean includeUuid =
            anyOperationResponse(ops, response -> response.isUuid) ||
            anyOperationParam(ops, param -> param.isUuid);
        operations.put("includeTime", includeTime);
        operations.put("includeUuid", includeUuid);

        return operations;
    }

    static class ParameterSorter implements Comparator<CodegenParameter> {
        public int compare(final CodegenParameter p1, final CodegenParameter p2) { 
            return index(p1) - index(p2);
        }

        private int index(final CodegenParameter p) {
            if (p.isPathParam) {
                return 1;
            }
            if (p.isQueryParam) {
                return 2;
            }
            if (p.isHeaderParam) {
                return 3;
            }
            if (p.isBodyParam) {
                return 4;
            }
            return 5;
        }
    }   
    
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return Boolean.valueOf(p.getDefault().toString()) ? "True" : "False";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        }
        return null;
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else
            type = openAPIType;
        return toModelName(type);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getTypeDeclaration(inner);
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getTypeDeclaration(inner);
        }
        return super.getTypeDeclaration(p);
    }

    private static class RemoveWhitespaceLambda implements Mustache.Lambda {
        @Override
        public void execute(final Template.Fragment fragment, final Writer writer) throws IOException {
            writer.write(fragment.execute().replaceAll("\\s+", ""));
        }
    }
}
