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

import com.google.common.collect.ImmutableMap.Builder;
import com.samskivert.mustache.Mustache.Lambda;

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.templating.mustache.CaseFormatLambda;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static com.google.common.base.CaseFormat.LOWER_CAMEL;
import static com.google.common.base.CaseFormat.LOWER_UNDERSCORE;

import java.util.*;

public class TypeScriptAxiosClientCodegen extends AbstractTypeScriptClientCodegen {
    public TypeScriptAxiosClientCodegen() {
        super();

        this.outputFolder = "generated-code/typescript-axios";
        embeddedTemplateDir = templateDir = "typescript-axios";

        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");

        modelPackage = "models";
        apiPackage = "resources";
    }

    @Override
    public String getName() {
        return "typescript-axios";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library using Axios for HTTP requests.";
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toModelImport(String name) {
        return modelPackage() + "/" + toModelFilename(name);
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "Empty";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return camelize(getSymbolName(name));
        }

        // number
        if ("number".equals(datatype)) {
            String varName = "NUMBER_" + name;

            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        String enumName = name;
        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name);

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    protected Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas().put("snakecase_param", new CaseFormatLambda(LOWER_CAMEL, LOWER_UNDERSCORE));
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessModels(objs);

        // Add additional filename information for imports
        List<Map<String, Object>> models = (List<Map<String, Object>>) postProcessModelsEnum(result).get("models");
        for (Map<String, Object> mo : models) {
            CodegenModel cm = (CodegenModel) mo.get("model");
            mo.put("tsImports", toTsImports(cm.imports));
        }

        return result;
    }

    private List<Map<String, String>> toTsImports(Set<String> imports) {
        List<Map<String, String>> tsImports = new ArrayList<>();
        for (String im : imports) {
            HashMap<String, String> tsImport = new HashMap<>();
            tsImport.put("classname", im);
            tsImport.put("filename", toModelFilename(im));
            tsImports.add(tsImport);
        }
        return tsImports;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> allModels) {
        // Add additional filename information for model imports in the services
        List<Map<String, Object>> imports = (List<Map<String, Object>>) operations.get("imports");
        for (Map<String, Object> im : imports) {
            im.put("filename", im.get("import"));
            im.put("classname", getModelnameFromModelFilename(im.get("filename").toString()));
        }

        // if there are any form params, we'll need to import stringify
        Map<String, Object> opsObj = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>) opsObj.get("operation");
        boolean anyHasFormParams = false;
        for (CodegenOperation op : ops) {
            if (op.getHasFormParams()) {
                anyHasFormParams = true;
            }
        }
        if (anyHasFormParams) {
            operations.put("hasFormParams", true);
        }

        return operations;
    }

    private String getModelnameFromModelFilename(String filename) {
        return filename.substring((modelPackage() + "/").length());
    }
}