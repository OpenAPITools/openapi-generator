/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;

import java.io.File;

public class DukeScriptClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected String sourceFolder = "src";
    protected String apiVersion = "1.0.0";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "dukescript";
    }

    public String getHelp() {
        return "Generates a DukeScript @Model client library (beta).";
    }

    public DukeScriptClientCodegen() {
        super();
        outputFolder = "generated-code/dukescript";
        modelTemplateFiles.put(
                "model.mustache",
                "VMD.java");
        apiTemplateFiles.put(
                "api.mustache",
                "Connector.java");
        templateDir = "dukescript";
        apiPackage = "org.openapitools.client.api";
        modelPackage = "org.openapitools.client.model";
        additionalProperties.put("apiVersion", apiVersion);
        typeMapping.put("string", "String");
        typeMapping.put("integer", "int");
        typeMapping.put("long", "long");
        typeMapping.put("short", "short");
        typeMapping.put("short", "short");
        typeMapping.put("Boolean", "boolean");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("Date", "long");
        typeMapping.put("DateTime", "long");
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;  // add an underscore to the name
    }

    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        return type;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

}
