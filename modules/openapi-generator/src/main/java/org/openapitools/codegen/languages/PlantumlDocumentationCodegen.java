package org.openapitools.codegen.languages;

import javafx.util.Pair;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PlantumlDocumentationCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

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

//    @Override
//    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
//        List<Map<String, Object>> models = (List<Map<String, Object>>) objs.get("models");
//        for (Map<String, Object> model : models) {
//            Object v = model.get("model");
//            if (v instanceof CodegenModel) {
//                CodegenModel m = (CodegenModel) v;
//                if (m.interfaces != null) {
//                    m.interfaces.removeIf(interfaceName -> interfaceName.endsWith("AllOf"));
//                }
//            }
//        }
//        return objs;
//    }

//    @Override
//    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
//        List<String> keysToRemove = objs.keySet().stream().filter(key -> key.endsWith("_allOf")).collect(Collectors.toList());
//        keysToRemove.forEach(key -> objs.remove(key));
//
//        return super.postProcessAllModels(objs);
//    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Object models = objs.get("models");
        List<Object> modelsList = (List<Object>) models;
        List<CodegenModel> codegenModelList = modelsList.stream()
                .filter(listItem -> listItem instanceof HashMap<?, ?>)
                .map(listItem -> (CodegenModel)((HashMap<?, ?>)listItem).get("model"))
                .collect(Collectors.toList());

        List<CodegenModel> composedCodegenModelList = codegenModelList.stream()
                .filter(codegenModel -> !codegenModel.allOf.isEmpty())
                .collect(Collectors.toList());

        List<CodegenModel> notComposedCodegenModelList = codegenModelList.stream()
                .filter(codegenModel -> codegenModel.allOf.isEmpty())
                .collect(Collectors.toList());

//        List<CodegenModel> allOfInlineCodegenModelList = notComposedCodegenModelList.stream()
//                .filter(codegenModel -> codegenModel.getName().endsWith("_allOf"))
//                .collect(Collectors.toList());
//
//        List<CodegenModel> simpleCodegenModelList = notComposedCodegenModelList.stream()
//                .filter(codegenModel -> !codegenModel.getName().endsWith("_allOf"))
//                .collect(Collectors.toList());

        List<Object> entities = notComposedCodegenModelList.stream().map(cm -> createEntityFor(cm)).collect(Collectors.toList());
        objs.put("entities", entities);

        List<Object> inheritances = calculateInheritanceRelationships(composedCodegenModelList);
        objs.put("inheritances", inheritances);

        return super.postProcessSupportingFileData(objs);
    }

    private List<Object> calculateInheritanceRelationships(List<CodegenModel> composedCodegenModelList) {
        return composedCodegenModelList.stream()
                .map(composedModel -> new Pair<>(composedModel.getClassname(), composedModel.getInterfaces().stream()
                        .filter(inf -> !inf.endsWith("AllOf"))
                        .collect(Collectors.toList())))
                .flatMap(stringListPair -> stringListPair.getValue().stream().map(parent -> createInheritance(parent, stringListPair.getKey())))
                .collect(Collectors.toList());
    }

    private Object createInheritance(String parentName, String childName) {
        Map<String, Object> inheritance = new HashMap<>();
        inheritance.put("parent", parentName);
        inheritance.put("child", childName);
        return inheritance;
    }

    private Object createEntityFor(CodegenModel codegenModel) {
        Map<String, Object> entity = new HashMap<>();
        entity.put("name", removeSuffix(codegenModel.getClassname(), "AllOf"));

        List<Object> fields = codegenModel.getAllVars().stream()
                .map(var -> createFieldFor(var))
                .collect(Collectors.toList());
        entity.put("fields", fields);

        return entity;
    }

    private String removeSuffix(final String value, final String suffix) {
        if (value != null && suffix != null && value.endsWith(suffix)) {
            return value.substring(0, value.length() - 5);
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
