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

import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.*;

public class TypeScriptAureliaClientCodegen extends AbstractTypeScriptClientCodegen {

    public TypeScriptAureliaClientCodegen() {
        super();

        apiTemplateFiles.put("api.mustache", ".ts");

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "generated-code/typescript-aurelia";
        embeddedTemplateDir = templateDir = "typescript-aurelia";
    }

    @Override
    public String getName() {
        return "typescript-aurelia";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library for the Aurelia framework (beta).";
    }

    public String getNpmName() {
        return npmName;
    }

    public void setNpmName(String npmName) {
        this.npmName = npmName;
    }

    public String getNpmVersion() {
        return npmVersion;
    }

    public void setNpmVersion(String npmVersion) {
        this.npmVersion = npmVersion;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Set supporting files
        supportingFiles.add(new SupportingFile("models.mustache", "", "models.ts"));
        supportingFiles.add(new SupportingFile("index.ts.mustache", "", "index.ts"));
        supportingFiles.add(new SupportingFile("Api.ts.mustache", "", "Api.ts"));
        supportingFiles.add(new SupportingFile("AuthStorage.ts.mustache", "", "AuthStorage.ts"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
        supportingFiles.add(new SupportingFile("package.json.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("tsconfig.json.mustache", "", "tsconfig.json"));
        supportingFiles.add(new SupportingFile("tslint.json.mustache", "", "tslint.json"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        HashSet<String> modelImports = new HashSet<>();
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            // Aurelia uses "asGet", "asPost", ... methods; change the method format
            op.httpMethod = camelize(op.httpMethod.toLowerCase(Locale.ROOT));

            // Collect models to be imported
            for (CodegenParameter param : op.allParams) {
                if (!param.isPrimitiveType && !param.isListContainer && !param.dataType.equals("any")) {
                    modelImports.add(param.dataType);
                }
            }
            if (op.returnBaseType != null && !op.returnTypeIsPrimitive) {
                modelImports.add(op.returnBaseType);
            }
        }

        objs.put("modelImports", modelImports);

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
            for (CodegenProperty var : cm.vars) {
                // name enum with model name, e.g. StatuEnum => PetStatusEnum
                if (Boolean.TRUE.equals(var.isEnum)) {
                    var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
                    var.enumName = cm.classname + var.enumName;
                }
            }
        }

        return objs;
    }

}
