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

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

public class PlantumlDocumentationCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String ALL_OF_SUFFIX = "AllOf";

    static Logger LOGGER = LoggerFactory.getLogger(PlantumlDocumentationCodegen.class);

    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    public String getName() {
        return "plantuml";
    }

    public String getHelp() {
        return "Generates a plantuml documentation.";
    }

    public PlantumlDocumentationCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "plantuml";
        embeddedTemplateDir = templateDir = "plantuml";
        supportingFiles.add(new SupportingFile("schemas.mustache", "", "schemas.plantuml"));
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Object models = objs.get("models");
        List<Object> modelsList = (List<Object>) models;
        List<CodegenModel> codegenModelList = modelsList.stream()
                .filter(listItem -> listItem instanceof HashMap<?, ?>)
                .map(listItem -> (CodegenModel) ((HashMap<?, ?>) listItem).get("model"))
                .collect(Collectors.toList());

        List<CodegenModel> inlineAllOfCodegenModelList = codegenModelList.stream()
                .filter(codegenModel -> codegenModel.getClassname().endsWith(ALL_OF_SUFFIX))
                .collect(Collectors.toList());

        List<CodegenModel> nonInlineAllOfCodegenModelList = codegenModelList.stream()
                .filter(codegenModel -> !codegenModel.getClassname().endsWith(ALL_OF_SUFFIX))
                .collect(Collectors.toList());

        List<CodegenModel> subtypeCodegenModelList = codegenModelList.stream()
                .filter(codegenModel -> !codegenModel.allOf.isEmpty())
                .collect(Collectors.toList());


        List<Map<String, Object>> entities = calculateEntities(nonInlineAllOfCodegenModelList, inlineAllOfCodegenModelList);
        objs.put("entities", entities);

        List<Map<String, Object>> relationships = calculateCompositionRelationshipsFrom(entities);
        objs.put("relationships", relationships);

        List<Object> inheritances = calculateInheritanceRelationships(subtypeCodegenModelList);
        objs.put("inheritances", inheritances);

        return super.postProcessSupportingFileData(objs);
    }

    private List<Map<String, Object>> calculateEntities(List<CodegenModel> nonInlineAllOfCodegenModelList, List<CodegenModel> inlineAllOfCodegenModelList) {
        Map<String, CodegenModel> inlineAllOfCodegenModelMap = inlineAllOfCodegenModelList.stream().collect(Collectors.toMap(cm -> cm.getClassname(), cm -> cm));

        return nonInlineAllOfCodegenModelList.stream().map(codegenModel -> createEntityFor(codegenModel, inlineAllOfCodegenModelMap)).collect(Collectors.toList());
    }

    private Map<String, Object> createEntityFor(CodegenModel nonInlineAllOfCodegenModel, Map<String, CodegenModel> inlineAllOfCodegenModelMap) {
        if (nonInlineAllOfCodegenModel.allOf.isEmpty()) {
            return createEntityFor(nonInlineAllOfCodegenModel, nonInlineAllOfCodegenModel.getAllVars());
        } else {
            Optional<String> inheritingInlineAllOfName = nonInlineAllOfCodegenModel.allOf.stream().filter(allOfName -> allOfName.endsWith(ALL_OF_SUFFIX)).findFirst();
            if (inheritingInlineAllOfName.isPresent()) {
                if (inlineAllOfCodegenModelMap.containsKey(inheritingInlineAllOfName.get())) {
                    CodegenModel inheritingInlineAllOfModel = inlineAllOfCodegenModelMap.get(inheritingInlineAllOfName.get());
                    return createEntityFor(nonInlineAllOfCodegenModel, inheritingInlineAllOfModel.getAllVars());
                }
            }

            return createEntityFor(nonInlineAllOfCodegenModel, Collections.emptyList());
        }
    }

    private Map<String, Object> createEntityFor(CodegenModel codegenModel, List<CodegenProperty> properties) {
        Map<String, Object> entity = new HashMap<>();
        entity.put("name", removeSuffix(codegenModel.getClassname(), ALL_OF_SUFFIX));

        List<Object> fields = properties.stream()
                .map(var -> createFieldFor(var))
                .collect(Collectors.toList());
        entity.put("fields", fields);

        return entity;
    }

    private Object createFieldFor(CodegenProperty codegenProperty) {
        Map<String, Object> field = new HashMap<>();
        field.put("name", codegenProperty.getBaseName());
        field.put("isRequired", codegenProperty.getRequired());
        field.put("isList", codegenProperty.isListContainer);
        field.put("complexDataType", getComplexDataTypeFor(codegenProperty));

        String dataType = codegenProperty.isListContainer && codegenProperty.getItems() != null ? "List<" + toModelName(codegenProperty.getItems().getDataType()) + ">" : toModelName(codegenProperty.getDataType());
        field.put("dataType", dataType);

        return field;
    }

    @SuppressWarnings("unchecked")
    private List<Map<String, Object>> calculateCompositionRelationshipsFrom(List<Map<String, Object>> entities) {
        Map<String, List<Map<String, Object>>> entityFieldsMap = entities.stream()
                .collect(Collectors.toMap(entity -> (String) entity.get("name"), entity -> (List<Map<String, Object>>) entity.get("fields")));

        return entityFieldsMap.entrySet().stream()
                .map(entry -> createRelationshipsFor(entry.getKey(), entry.getValue()))
                .flatMap(relationshipList -> relationshipList.stream())
                .collect(Collectors.toList());
    }

    private List<Map<String, Object>> createRelationshipsFor(String entityName, List<Map<String, Object>> fields) {
        return fields.stream()
                .filter(field -> field.get("complexDataType") != null)
                .map(complexField -> createRelationshipFor(complexField, entityName))
                .collect(Collectors.toList());
    }

    private Map<String, Object> createRelationshipFor(Map<String, Object> complexField, String entityName) {
        Map<String, Object> relationship = new HashMap<>();
        relationship.put("parent", entityName);
        relationship.put("child", complexField.get("complexDataType"));
        relationship.put("name", complexField.get("name"));
        relationship.put("isList", complexField.get("isList"));

        return relationship;
    }

    private String getComplexDataTypeFor(CodegenProperty codegenProperty) {
        if (codegenProperty.isModel) {
            return toModelName(codegenProperty.getDataType());
        } else if (codegenProperty.isListContainer && codegenProperty.getItems().isModel) {
            return toModelName((codegenProperty.getItems().getDataType()));
        }

        return null;
    }

    private List<Object> calculateInheritanceRelationships(List<CodegenModel> subtypeCodegenModelList) {
        return subtypeCodegenModelList.stream()
                .map(subtypeModel -> getInterfacesForSubtype(subtypeModel))
                .flatMap(subTypeInterfaces -> subTypeInterfaces.getValue().stream().map(parent -> createInheritance(parent, subTypeInterfaces.getKey())))
                .collect(Collectors.toList());
    }

    private Map.Entry<String, List<String>> getInterfacesForSubtype(CodegenModel subtypeModel) {
        List<String> interfaceList = subtypeModel.getInterfaces().stream()
                .filter(inf -> !inf.endsWith(ALL_OF_SUFFIX))
                .collect(Collectors.toList());
        return new AbstractMap.SimpleEntry<>(subtypeModel.getClassname(), interfaceList);
    }

    private Object createInheritance(String parentName, String childName) {
        Map<String, Object> inheritance = new HashMap<>();
        inheritance.put("parent", parentName);
        inheritance.put("child", childName);
        return inheritance;
    }

    private String removeSuffix(final String value, final String suffix) {
        if (value != null && suffix != null && value.endsWith(suffix)) {
            return value.substring(0, value.length() - suffix.length());
        }

        return value;
    }
}
