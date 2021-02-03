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

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static org.openapitools.codegen.utils.OnceLogger.once;


public class Apache2ConfigCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String USER_INFO_PATH = "userInfoPath";
    private static final Logger LOGGER = LoggerFactory.getLogger(Apache2ConfigCodegen.class);

    protected String userInfoPath = "/var/www/html/";

    @Override
    public CodegenType getTag() {
        return CodegenType.CONFIG;
    }

    @Override
    public String getName() {
        return "apache2";
    }

    @Override
    public String getHelp() {
        return "Generates an Apache2 Config file with the permissions";
    }

    public Apache2ConfigCodegen() {
        super();

        // TODO: Apache2 maintainer review.
        modifyFeatureSet(features -> features
                .parameterFeatures(EnumSet.of(ParameterFeature.Path))
                .securityFeatures(EnumSet.of(SecurityFeature.BasicAuth))
                .dataTypeFeatures(EnumSet.noneOf(DataTypeFeature.class))
                .wireFormatFeatures(EnumSet.noneOf(WireFormatFeature.class))
                .documentationFeatures(EnumSet.noneOf(DocumentationFeature.class))
                .globalFeatures(EnumSet.noneOf(GlobalFeature.class))
                .schemaSupportFeatures(EnumSet.noneOf(SchemaSupportFeature.class))
                .clientModificationFeatures(EnumSet.noneOf(ClientModificationFeature.class))
        );

        apiTemplateFiles.put("apache-config.mustache", ".conf");

        embeddedTemplateDir = templateDir = "apache2";

        cliOptions.add(new CliOption(USER_INFO_PATH, "Path to the user and group files"));
    }


    @Override
    public void processOpts() {
        if (additionalProperties.containsKey(USER_INFO_PATH)) {
            userInfoPath = additionalProperties.get(USER_INFO_PATH).toString();
        }
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        List<CodegenOperation> newOpList = new ArrayList<CodegenOperation>();

        for (CodegenOperation op : operationList) {
            String path = op.path;

            String[] items = path.split("/", -1);
            List<String> splitPath = new ArrayList<String>();
            for (String item : items) {
                if (item.matches("^\\{(.*)\\}$")) {
                    item = "*";
                }
                splitPath.add(item);
                op.path += item + "/";
            }
            op.vendorExtensions.put("x-codegen-user-info-path", userInfoPath);
            boolean foundInNewList = false;
            for (CodegenOperation op1 : newOpList) {
                if (!foundInNewList) {
                    if (op1.path.equals(op.path)) {
                        foundInNewList = true;
                        List<CodegenOperation> currentOtherMethodList = (List<CodegenOperation>) op1.vendorExtensions.get("x-codegen-otherMethods");
                        if (currentOtherMethodList == null) {
                            currentOtherMethodList = new ArrayList<CodegenOperation>();
                        }
                        op.operationIdCamelCase = op1.operationIdCamelCase;
                        currentOtherMethodList.add(op);
                        op1.vendorExtensions.put("x-codegen-other-methods", currentOtherMethodList);
                    }
                }
            }
            if (!foundInNewList) {
                newOpList.add(op);
            }
        }
        operations.put("operation", newOpList);
        return objs;
    }
}
