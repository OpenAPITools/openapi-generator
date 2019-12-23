package org.openapitools.codegen.utils;

import org.openapitools.codegen.CodegenDiscriminator;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.ParentInterfaceModel;

import java.util.*;
import java.util.stream.Collectors;

public class OneOfModelPostProcessor {

    private Map<String, CodegenModel> codegenModelMapping;

    public OneOfModelPostProcessor(Map<String, Object> modelMapping) {
        this.codegenModelMapping = extractModelMapping(modelMapping);
    }

    public void postProcessOneOfModels() {
        getOneOfModels().forEach(this::oneOfModelsActions);
    }

    private void oneOfModelsActions(CodegenModel codegenModel) {
        cleanUpChildren(codegenModel);
        markAsInterface(codegenModel);
        addChildren(codegenModel);
        addDiscriminatorDetails(codegenModel);
        addInterfacesToChildren(codegenModel);
    }

    private List<CodegenModel> getOneOfModels() {
        return codegenModelMapping.entrySet()
                .stream()
                .map(Map.Entry::getValue)
                .filter(this::isOneOfOnlyModel)
                .collect(Collectors.toList());
    }

    private void addDiscriminatorDetails(CodegenModel codegenModel) {

        if (codegenModel.discriminator == null) {
            throw new RuntimeException("Error, oneOf model must have discriminator");
        }

        if (codegenModel.discriminator.getMappedModels() == null) {
            codegenModel.discriminator.setMappedModels(new HashSet<>());
        }

        codegenModel.oneOf.stream()
                .map(codegenModelMapping::get)
                .map(childModel -> new CodegenDiscriminator.MappedModel(childModel.name, childModel.classname))
                .forEach(codegenModel.discriminator.getMappedModels()::add);

    }

    private void addInterfacesToChildren(CodegenModel codegenModel) {
        codegenModel.oneOf.stream()
                .map(codegenModelMapping::get)
                .forEach(childModel -> addChildren(codegenModel, childModel));
    }

    private void addChildren(CodegenModel codegenModel, CodegenModel childModel) {
        childModel.hasParentInterfaces = true;
        childModel.parentInterfaceModels.forEach(parentInterfaceModel -> parentInterfaceModel.hasMore = true);
        childModel.parentInterfaceModels.add(new ParentInterfaceModel(codegenModel));
    }

    private void addChildren(CodegenModel codegenModel) {
        codegenModel.oneOf.stream()
                .map(codegenModelMapping::get)
                .filter(Objects::nonNull)
                .forEach(childModel -> addChild(codegenModel, childModel));
    }

    private void addChild(CodegenModel codegenModel, CodegenModel childModel) {
        codegenModel.children.add(childModel);
    }

    private void markAsInterface(CodegenModel codegenModel) {
        codegenModel.isInterface = true;
    }

    private boolean isOneOfOnlyModel(CodegenModel codegenModel) {
        boolean modelHasOneOfTag = !codegenModel.oneOf.isEmpty();
        boolean modelHasNoAllOf = codegenModel.allOf.isEmpty();
        boolean modelHasNoAnyOf = codegenModel.anyOf.isEmpty();
        boolean modelHasNoProperties = !codegenModel.modelJson.contains("properties") ;
        return modelHasOneOfTag && modelHasNoProperties && modelHasNoAnyOf && modelHasNoAllOf;
    }

    private void cleanUpChildren(CodegenModel codegenModel) {
        codegenModel.children = new LinkedList<>();
    }

    private Map<String, CodegenModel> extractModelMapping(Map<String, Object> objs) {
        return objs.entrySet().stream().map(Map.Entry::getValue)
                .map(Map.class::cast)
                .map(map -> map.get("models"))
                .map(List.class::cast)
                .map(list -> list.get(0))
                .map(Map.class::cast)
                .map(map -> map.get("model"))
                .map(CodegenModel.class::cast)
                .collect(Collectors.toMap(codegenModel -> codegenModel.getClassname(), codegenModel -> codegenModel));
    }
}
