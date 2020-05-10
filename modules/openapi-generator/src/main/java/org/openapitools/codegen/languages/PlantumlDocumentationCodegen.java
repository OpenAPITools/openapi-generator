package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

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

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Map<String, Object>> models = (List<Map<String, Object>>) objs.get("models");
        for (Map<String, Object> model : models) {
            Object v = model.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel m = (CodegenModel) v;
                if (m.interfaces != null) {
                    m.interfaces.removeIf(interfaceName -> interfaceName.endsWith("AllOf"));
                }
            }
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        List<String> keysToRemove = objs.keySet().stream().filter(key -> key.endsWith("_allOf")).collect(Collectors.toList());
        keysToRemove.forEach(key -> objs.remove(key));

        return super.postProcessAllModels(objs);
    }
}
