package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

        outputFolder = "generated-code" + File.separator + "plantuml";
        embeddedTemplateDir = templateDir = "plantuml-documentation";
        supportingFiles.add(new SupportingFile("schemas.mustache", "", "schemas.plantuml"));
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Object models = objs.get("models");
        List<Object> modelsList = (List<Object>) models;
        List<CodegenModel> codegenModelList = modelsList.stream()
                .filter(listItem -> listItem instanceof HashMap<?, ?>)
                .map(listItem -> (CodegenModel)((HashMap<?, ?>)listItem).get("model"))
                .collect(Collectors.toList());

        List<CodegenModel> subtypeCodegenModelList = codegenModelList.stream()
                .filter(codegenModel -> !codegenModel.allOf.isEmpty())
                .collect(Collectors.toList());

        List<CodegenModel> supertypeCodegenModelList = codegenModelList.stream()
                .filter(codegenModel -> codegenModel.allOf.isEmpty())
                .collect(Collectors.toList());

        List<Object> entities = supertypeCodegenModelList.stream().map(cm -> createEntityFor(cm)).collect(Collectors.toList());
        objs.put("entities", entities);

        List<Object> relationships = calculateCompositionRelationships(supertypeCodegenModelList);
        objs.put("relationships", relationships);

        List<Object> inheritances = calculateInheritanceRelationships(subtypeCodegenModelList);
        objs.put("inheritances", inheritances);

        return super.postProcessSupportingFileData(objs);
    }

    private List<Object> calculateCompositionRelationships(List<CodegenModel> supertypeCodegenModelList) {
        return supertypeCodegenModelList.stream()
                .map(superType -> calculateCompositionRelationshipsFor(superType))
                .flatMap(relationshipList -> relationshipList.stream())
                .collect(Collectors.toList());
    }

    private List<Object> calculateCompositionRelationshipsFor(CodegenModel codegenModel) {
        String parentName = removeSuffix(codegenModel.getClassname(), ALL_OF_SUFFIX);
        return codegenModel.getAllVars().stream()
                .filter(var -> getComplexDataTypeFor(var) != null)
                .map(var -> createRelationshipFor(var,parentName))
                .collect(Collectors.toList());
    }

    private Object createRelationshipFor(CodegenProperty codegenProperty, String parent) {
        Map<String, Object> field = new HashMap<>();
        String childModelName = toModelName(getComplexDataTypeFor(codegenProperty));
        field.put("parent", parent);
        field.put("child", childModelName);
        field.put("name", codegenProperty.getName());
        field.put("isList", codegenProperty.isListContainer);

        return field;
    }

    private String getComplexDataTypeFor(CodegenProperty codegenProperty) {
        if (codegenProperty.isModel) {
            return codegenProperty.getDataType();
        } else if (codegenProperty.isListContainer && codegenProperty.getItems().isModel) {
            return codegenProperty.getItems().getDataType();
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

    private Object createEntityFor(CodegenModel codegenModel) {
        Map<String, Object> entity = new HashMap<>();
        entity.put("name", removeSuffix(codegenModel.getClassname(), ALL_OF_SUFFIX));

        List<Object> fields = codegenModel.getAllVars().stream()
                .map(var -> createFieldFor(var))
                .collect(Collectors.toList());
        entity.put("fields", fields);

        return entity;
    }

    private String removeSuffix(final String value, final String suffix) {
        if (value != null && suffix != null && value.endsWith(suffix)) {
            return value.substring(0, value.length() - suffix.length());
        }

        return value;
    }

    private Object createFieldFor(CodegenProperty codegenProperty) {
        Map<String, Object> field = new HashMap<>();
        field.put("name", codegenProperty.getBaseName());
        field.put("isRequired", codegenProperty.getRequired());

        String dataType = codegenProperty.isListContainer && codegenProperty.getItems() != null ? "List<" + codegenProperty.getItems().getDataType() + ">" : codegenProperty.getDataType();
        field.put("dataType", dataType);

        return field;
    }
}
