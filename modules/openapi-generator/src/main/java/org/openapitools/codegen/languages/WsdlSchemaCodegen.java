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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

import java.io.File;
import java.text.Normalizer;
import java.util.*;

public class WsdlSchemaCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    public String getName() {
        return "wsdl-schema";
    }

    public String getHelp() {
        return "Generates WSDL files.";
    }

    public WsdlSchemaCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "wsdl-schema";
        embeddedTemplateDir = templateDir = "wsdl-schema";
        apiPackage = "Apis";
        modelPackage = "Models";

        cliOptions.add(new CliOption("hostname", "the hostname of the service"));
        cliOptions.add(new CliOption("soapPath", "basepath of the soap services"));
        cliOptions.add(new CliOption("serviceName", "service name for the wsdl"));

        additionalProperties.put("hostname", "localhost");
        additionalProperties.put("soapPath", "soap");
        additionalProperties.put("serviceName", "ServiceV1");

        supportingFiles.add(new SupportingFile("wsdl-converter.mustache", "", "service.wsdl"));
        supportingFiles.add(new SupportingFile("jaxb-customization.mustache", "",
                "jaxb-customization.xml"));
    }

    public void preprocessOpenAPI(OpenAPI openAPI) {
        Info info = openAPI.getInfo();

        String title = info.getTitle();
        String description = info.getDescription();

        info.setDescription(this.processOpenapiSpecDescription(description));
        info.setTitle(this.escapeTitle(title));
    }

    private String escapeTitle(String title) {
        // strip umlauts etc.
        final String normalizedTitle = Normalizer.normalize(title, Normalizer.Form.NFD)
                .replaceAll("[^\\p{ASCII}]", "");
        return super.escapeUnsafeCharacters(normalizedTitle);
    }

    public String processOpenapiSpecDescription(String description) {
        if (description != null) {
            return description.replaceAll("\\s+", " ");
        } else {
            return "No description provided";
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs,
                                                               List<Object> allModels) {

        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            op.operationId = this.generateOperationId(op);

            // for xml compliant primitives, lowercase dataType of openapi
            for (CodegenParameter param : op.allParams) {
                Map<String, Object> paramVendorExtensions = param.vendorExtensions;

                normalizeDataType(param);

                // prevent default="null" in wsdl-tag if no default was specified for a param
                if ("null".equals(param.defaultValue) || param.defaultValue == null) {
                    paramVendorExtensions.put("x-param-has-defaultvalue", false);
                } else {
                    paramVendorExtensions.put("x-param-has-defaultvalue", true);
                }

                // check if param has a minimum or maximum number or length
                if (param.minimum != null
                        || param.maximum != null
                        || param.minLength != null
                        || param.maxLength != null) {
                    paramVendorExtensions.put("x-param-has-minormax", true);
                } else {
                    paramVendorExtensions.put("x-param-has-minormax", false);
                }

                // if param is enum, uppercase 'baseName' to have a reference to wsdl simpletype
                if (param.isEnum) {
                    param.baseName = param.baseName.substring(0, 1).toUpperCase(Locale.getDefault())
                            + param.baseName.substring(1);
                }
            }

            // handle case lowercase schema-name in openapi to have reference to wsdl complextype
            for (CodegenResponse response : op.responses) {
                if (response.isModel) {
                    response.dataType = response.dataType.substring(0, 1).toUpperCase(Locale.getDefault())
                            + response.dataType.substring(1);
                }

                if (response.isArray) {
                    response.baseType = response.baseType.substring(0, 1).toUpperCase(Locale.getDefault())
                            + response.baseType.substring(1);
                }
            }

            for (CodegenParameter param : op.bodyParams) {
                normalizeDataType(param);
            }
            for (CodegenParameter param : op.pathParams) {
                normalizeDataType(param);
            }
            for (CodegenParameter param : op.queryParams) {
                normalizeDataType(param);
            }
            for (CodegenParameter param : op.formParams) {
                normalizeDataType(param);
            }
        }

        return objs;
    }

    private void normalizeDataType(CodegenParameter param) {
        if (param.isPrimitiveType) {
            param.dataType = param.dataType.toLowerCase(Locale.getDefault());
        }
        if (param.dataFormat != null && param.dataFormat.equalsIgnoreCase("date")) {
            param.dataType = "date";
        }
        if (param.dataFormat != null && param.dataFormat.equalsIgnoreCase("date-time")) {
            param.dataType = "dateTime";
        }
        if (param.dataFormat != null && param.dataFormat.equalsIgnoreCase("uuid")) {
            param.dataType = "string";
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");

        for (Object mo : models) {
            Map<String, Object> mod = (Map<String, Object>) mo;
            CodegenModel model = (CodegenModel) mod.get("model");
            Map<String, Object> modelVendorExtensions = model.getVendorExtensions();

            for (CodegenProperty var : model.vars) {
                Map<String, Object> propertyVendorExtensions = var.getVendorExtensions();

                // lowercase basetypes if openapitype is string
                if ("string".equals(var.openApiType)) {
                    var.baseType = var.baseType.substring(0, 1).toLowerCase(Locale.getDefault())
                            + var.baseType.substring(1);
                }
                // if string enum, uppercase 'name' to have a reference to wsdl simpletype
                if (var.isEnum) {
                    var.name = var.name.substring(0, 1).toUpperCase(Locale.getDefault()) + var.name.substring(1);
                }
                // prevent default="null" in wsdl-tag if no default was specified for a property
                if ("null".equals(var.defaultValue) || var.defaultValue == null) {
                    propertyVendorExtensions.put("x-prop-has-defaultvalue", false);
                } else {
                    propertyVendorExtensions.put("x-prop-has-defaultvalue", true);
                }

                // check if model property has a minimum or maximum number or length
                if (var.minimum != null
                        || var.maximum != null
                        || var.minLength != null
                        || var.maxLength != null) {
                    propertyVendorExtensions.put("x-prop-has-minormax", true);
                } else {
                    propertyVendorExtensions.put("x-prop-has-minormax", false);
                }

                // specify appearing schema names in case of openapi array with oneOf elements
                if ("array".equals(var.openApiType) && var.items.dataType.startsWith("oneOf<")) {
                    // get only comma separated names of schemas from oneOf<name1,name2...>
                    String schemaNamesString =
                            var.items.dataType.substring(6, var.items.dataType.length() - 1);
                    List<String> oneofSchemas =
                            new ArrayList<String>(Arrays.asList(schemaNamesString.split("\\s*,\\s*")));

                    for (int i = 0; i < oneofSchemas.size(); i++) {
                        oneofSchemas.set(i, lowerCaseStringExceptFirstLetter(oneofSchemas.get(i)));
                    }

                    propertyVendorExtensions.put("x-oneof-schemas", oneofSchemas);
                }
            }
        }
        return super.postProcessModelsEnum(objs);
    }

    public String generateOperationId(CodegenOperation op) {
        String newOperationid = this.lowerCaseStringExceptFirstLetter(op.httpMethod);
        String[] pathElements = op.path.split("/");
        List<String> pathParameters = new ArrayList();

        for (int i = 0; i < pathElements.length; i++) {
            if (pathElements[i].contains("{")) {
                pathParameters.add(pathElements[i]);
                pathElements[i] = "";
            }
            if (pathElements[i].length() > 0) {
                newOperationid = newOperationid
                        + this.lowerCaseStringExceptFirstLetter(pathElements[i]);
            }
        }

        if (pathParameters.size() > 0) {
            for (int i = 0; i < pathParameters.size(); i++) {
                String pathParameter = pathParameters.get(i);
                pathParameter = this.lowerCaseStringExceptFirstLetter(pathParameter
                        .substring(1, pathParameter.length() - 1));
                if (i == 0) {
                    newOperationid = newOperationid + "By" + pathParameter;
                } else {
                    newOperationid = newOperationid + "And" + pathParameter;
                }
            }
        }
        return newOperationid;
    }

    public String lowerCaseStringExceptFirstLetter(String value) {
        String newOperationid = value.toLowerCase(Locale.getDefault());
        return newOperationid.substring(0, 1).toUpperCase(Locale.getDefault())
                + newOperationid.substring(1);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // just return the original string
        return input;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // just return the original string
        return input;
    }
}
