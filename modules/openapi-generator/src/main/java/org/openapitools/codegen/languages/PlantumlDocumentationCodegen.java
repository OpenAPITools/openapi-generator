package org.openapitools.codegen.languages;

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

        List<Object> entities = codegenModelList.stream().map(cm -> createEntityFor(cm)).collect(Collectors.toList());
        objs.put("entities", entities);

        return super.postProcessSupportingFileData(objs);
    }

    private Object createEntityFor(CodegenModel codegenModel) {
        Map<String, Object> entity = new HashMap<>();
        entity.put("name", codegenModel.getClassname());

        List<Object> fields = codegenModel.getAllVars().stream()
                .map(var -> createFieldFor(var))
                .collect(Collectors.toList());
        entity.put("fields", fields);

        return entity;
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
