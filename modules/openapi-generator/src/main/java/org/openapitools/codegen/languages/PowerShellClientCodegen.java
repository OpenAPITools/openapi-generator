/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static java.util.UUID.randomUUID;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class PowerShellClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(PowerShellClientCodegen.class);

    private String packageGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";

    protected String sourceFolder = "src";
    protected String packageName = "Org.OpenAPITools";
    protected String csharpClientPath = "$ScriptDir\\csharp\\OpenAPIClient";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    /**
     * Constructs an instance of `PowerShellClientCodegen`.
     */
    public PowerShellClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "powershell";
        modelTemplateFiles.put("model.mustache", ".ps1");
        apiTemplateFiles.put("api.mustache", ".ps1");
        modelTestTemplateFiles.put("model_test.mustache", ".ps1");
        apiTestTemplateFiles.put("api_test.mustache", ".ps1");
        modelDocTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        embeddedTemplateDir = templateDir = "powershell";
        apiPackage = packageName + File.separator + "API";
        modelPackage = packageName + File.separator + "Model";

        // https://blogs.msdn.microsoft.com/powershell/2010/01/07/how-objects-are-sent-to-and-from-remote-sessions/
        languageSpecificPrimitives = new HashSet<String>(Arrays.asList(
                "Byte",
                "SByte",
                "Byte[]",
                "Int16",
                "Int32",
                "Int64",
                "UInt16",
                "UInt32",
                "UInt64",
                "Decimal",
                "Single",
                "Double",
                "TimeSpan",
                "System.DateTime",
                "ProgressRecord",
                "Char",
                "String",
                "XmlDocument",
                "SecureString",
                "Boolean",
                "Guid",
                "Uri",
                "Version"
        ));

        // https://richardspowershellblog.wordpress.com/2009/05/02/powershell-reserved-words/
        reservedWords = new HashSet<String>(Arrays.asList(
                "Begin",
                "Break",
                "Catch",
                "Continue",
                "Data",
                "Do",
                "Dynamicparam",
                "Else",
                "Elseif",
                "End",
                "Exit",
                "Filter",
                "Finally",
                "For",
                "Foreach",
                "From",
                "Function",
                "If",
                "In",
                "Param",
                "Process",
                "Return",
                "Switch",
                "Throw",
                "Trap",
                "Try",
                "Until",
                "While",
                "Local",
                "Private",
                "Where"
        ));


        defaultIncludes = new HashSet<String>(Arrays.asList(
                "Byte",
                "SByte",
                "Byte[]",
                "Int16",
                "Int32",
                "Int64",
                "UInt16",
                "UInt32",
                "UInt64",
                "Decimal",
                "Single",
                "Double",
                "TimeSpan",
                "System.DateTime",
                "ProgressRecord",
                "Char",
                "String",
                "XmlDocument",
                "SecureString",
                "Boolean",
                "Guid",
                "Uri",
                "Version"
        ));

        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "String");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("integer", "Int32");
        typeMapping.put("float", "Double");
        typeMapping.put("long", "Int64");
        typeMapping.put("double", "Double");
        typeMapping.put("number", "Decimal");
        typeMapping.put("date-time", "System.DateTime");
        typeMapping.put("date", "System.DateTime");
        typeMapping.put("file", "String");
        typeMapping.put("object", "String");
        typeMapping.put("binary", "String");
        typeMapping.put("Date", "System.DateTime");
        typeMapping.put("DateTime", "System.DateTime");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Client package name (e.g. org.openapitools.client).").defaultValue(this.packageName));
        cliOptions.add(new CliOption(CodegenConstants.OPTIONAL_PROJECT_GUID, "GUID for PowerShell module (e.g. a27b908d-2a20-467f-bc32-af6f3a654ac5). A random GUID will be generated by default."));
        cliOptions.add(new CliOption("csharpClientPath", "Path to the C# API client generated by OpenAPI Generator, e.g. $ScriptDir\\..\\csharp\\OpenAPIClient where $ScriptDir is the current directory. NOTE: you will need to generate the C# API client separately.").defaultValue(this.csharpClientPath));

    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "powershell";
    }

    public String getHelp() {
        return "Generates a PowerShell API client (beta). (The dependency C# API client needs to be generated separately.";
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setCsharpClientPath(String csharpClientPath) {
        this.csharpClientPath = csharpClientPath;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_GUID)) {
            setPackageGuid((String) additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_GUID));
        }
        additionalProperties.put("packageGuid", packageGuid);

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            this.setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey("csharpClientPath")) {
            this.setCsharpClientPath((String) additionalProperties.get("csharpClientPath"));
        } else {
            additionalProperties.put("csharpClientPath", csharpClientPath);
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            LOGGER.warn(CodegenConstants.MODEL_PACKAGE + " with " + this.getName() + " generator is ignored. Setting this value independently of " + CodegenConstants.PACKAGE_NAME + " is not currently supported.");
        }

        if (additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            LOGGER.warn(CodegenConstants.API_PACKAGE + " with " + this.getName() + " generator is ignored. Setting this value independently of " + CodegenConstants.PACKAGE_NAME + " is not currently supported.");
        }

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Build.ps1.mustache", "", "Build.ps1"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator);

        supportingFiles.add(new SupportingFile("Org.OpenAPITools.psm1.mustache", infrastructureFolder, packageName + ".psm1"));

        // private
        supportingFiles.add(new SupportingFile("Get-CommonParameters.ps1", infrastructureFolder + File.separator + "Private" + File.separator, "Get-CommonParameters.ps1"));
        supportingFiles.add(new SupportingFile("Out-DebugParameter.ps1", infrastructureFolder + File.separator + "Private" + File.separator, "Out-DebugParameter.ps1"));

        // en-US
        supportingFiles.add(new SupportingFile("about_Org.OpenAPITools.help.txt.mustache", infrastructureFolder + File.separator + "en-US" + File.separator + "about_" + packageName + ".help.txt"));

    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("#>", "#_>").replace("<#", "<_#");
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String apiTestFileFolder() {
        return (outputFolder + "/test").replace('/', File.separatorChar);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage();
    }

    @Override
    public String modelTestFileFolder() {
        return (outputFolder + "/test").replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }


    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage();
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    /**
     * Output the proper model name (capitalized).
     * In case the name belongs to the TypeSystem it won't be renamed.
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    @Override
    public String toModelName(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return "New-" + toModelName(name);
    }

    /**
     * returns the OpenAPI type for the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the type
     **/
    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;

        // This maps, for example, long -> Long based on hashes in this type's constructor
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }

        // model/object
        return toModelName(type);
    }

    /**
     * Output the type declaration of the property
     *
     * @param p OpenAPI Schema object
     * @return a string presentation of the property type
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getTypeDeclaration(inner) + "[]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            // TODO not sure if the following map/hash declaration is correct
            return "{String, " + getTypeDeclaration(inner) + "}";
        } else if (!languageSpecificPrimitives.contains(getSchemaType(p))) {
            return packageName + ".Model." + super.getTypeDeclaration(p);
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty (should not occur as an auto-generated method name will be used)
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return camelize(sanitizeName(operationId));
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            int index = 0;
            for (CodegenParameter p : op.allParams) {
                p.vendorExtensions.put("x-index", index);
                index++;
            }
        }

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        // add x-index to properties
        ProcessUtils.addIndexToProperties(models);
        return objs;
    }

}
