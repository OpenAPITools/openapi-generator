package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;

import java.io.File;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProtobufVersion2SchemaCodegen extends ProtobufSchemaCodegen implements CodegenConfig {

    @Override
    public String getName() {
        return "protobuf-version-2-schema";
    }

    @Override
    public String getHelp() {
        return "Generates gRPC and protocol buffer 2 schema files (beta)";
    }

    public ProtobufVersion2SchemaCodegen() {
        super();

        this.protoVersion = "proto2";
        outputFolder = "generated-code" + File.separator + "protobuf-version-2-schema";
        embeddedTemplateDir = templateDir = "protobuf-version-2-schema";
    }

    @Override
    public void postProcessAllCustomOptions(List<CodegenProperty> customOptions, String customOptionsFileName) {
        super.postProcessAllCustomOptions(customOptions, customOptionsFileName);

        addProtobufType(customOptions);
    }

    @Override
    public CodegenModel postProcessCustomOptionCategory(CodegenModel optionCategory) {
        optionCategory = super.postProcessCustomOptionCategory(optionCategory);

        addProtobufType(optionCategory.vars);

        return optionCategory;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);

        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            
            addProtobufType(cm.vars);
        }

        return objs;
    }

    private void addProtobufType(List<CodegenProperty> vars) {
        for (CodegenProperty var : vars) {
            // add x-protobuf-type unless already set
            if (var.getRequired()) {
                var.vendorExtensions.putIfAbsent("x-protobuf-type", "required");
            }
            else {
                var.vendorExtensions.putIfAbsent("x-protobuf-type", "optional");
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();
        for (CodegenOperation op : operationList) {
            for (CodegenParameter p : op.allParams) {
                // add x-protobuf-type unless already set
                if (p.required) {
                    p.vendorExtensions.putIfAbsent("x-protobuf-type", "required");
                }
                else {
                    p.vendorExtensions.putIfAbsent("x-protobuf-type", "optional");
                }                
            }

            // add 'optional' before the type if not an array (used 'repeated')
            if (!"array".equals(op.returnContainer)) {
                op.vendorExtensions.put("x-grpc-response-type", "optional " + op.vendorExtensions.get("x-grpc-response-type"));                
            }
        }

        return objs;
    }
}
