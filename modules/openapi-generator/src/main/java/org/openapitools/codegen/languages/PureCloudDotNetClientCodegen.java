package org.openapitools.codegen.languages;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class PureCloudDotNetClientCodegen extends CSharpClientCodegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudDotNetClientCodegen.class);

    public PureCloudDotNetClientCodegen() {
        super();

        // Use C# templates
        embeddedTemplateDir = templateDir = "purecloud" + File.separator + "csharp";

        // Custom mappings for swagger type -> .NET type
        typeMapping.put("date", "String" );
        typeMapping.put("LocalDateTime", "DateTime?");
        typeMapping.put("LocalTime", "DateTime?");
        typeMapping.put("Dictionary<string, Object>", "Object");
        typeMapping.put("decimal", "double");
        typeMapping.put("decimal?", "double?");

        apiDocTemplateFiles.put("api_json.mustache", ".json");
        operationTemplateFiles.put("operation_example.mustache", "-example.txt");

        reservedWords.addAll(
                Arrays.asList(
                        // set "client" as a reserved word to avoid conflicts with Org.OpenAPITools.Client
                        // this is a workaround and can be removed if c# api client is updated to use
                        // fully qualified name
                        "Client", "client", "parameter",
                        // local variable names in API methods (endpoints)
                        "localVarPath", "localVarPathParams", "localVarQueryParams", "localVarHeaderParams",
                        "localVarFormParams", "localVarFileParams", "localVarStatusCode", "localVarResponse",
                        "localVarPostBody", "localVarHttpHeaderAccepts", "localVarHttpHeaderAccept",
                        "localVarHttpContentTypes", "localVarHttpContentType",
                        "localVarStatusCode",
                        // C# reserved words
                        "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked",
                        "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else",
                        "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for",
                        "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock",
                        "long", "namespace", "new", "null", "object", "operator", "out", "override", "params", "Params",
                        "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed",
                        "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw",
                        "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using",
                        "virtual", "void", "volatile", "while")
        );

    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Prevents collision between System.Attribute and ININ.PureCloudApi.Model.Attribute
        typeMapping.put("Attribute", this.packageName + ".Model.Attribute");

        // `Configuration' is an ambiguous reference between `PureCloudPlatform.Client.V2.Client.Configuration' and `PureCloudPlatform.Client.V2.Model.Configuration'
        typeMapping.put("Configuration", this.packageName + ".Model.Configuration");

        // Prevent collision between base namespace and model PureCloud
        typeMapping.put("PureCloud", this.packageName + ".Model.PureCloud");

        typeMapping.put("Action", this.packageName + ".Model.Action");

        // Tests
        supportingFiles.add(new SupportingFile("test-packages.mustache", "", "src/" + this.packageName + ".Tests/packages.config"));
        supportingFiles.add(new SupportingFile("test-AssemblyInfo.mustache", "", "src/" + this.packageName + ".Tests/Properties/AssemblyInfo.cs"));
        supportingFiles.add(new SupportingFile("test-csproj.mustache", "", "src/" + this.packageName + ".Tests/" + this.packageName + ".Tests.csproj"));
        supportingFiles.add(new SupportingFile("test-SdkTests.mustache", "", "src/" + this.packageName + ".Tests/SdkTests.cs"));
        supportingFiles.add(new SupportingFile("test-ApiClientTests.mustache", "", "src/" + this.packageName + ".Tests/ApiClientTests.cs"));
    }

    @Override
    public String getName() {
        return "pureclouddotnet";
    }

    @Override
    /**
     * Get the operation ID or use default behavior if blank.
     *
     * @param operation the operation object
     * @param path the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        if (operation.getExtensions().containsKey(OPERATION_ID_PROPERTY_NAME)) {
            String operationId = operation.getExtensions().get(OPERATION_ID_PROPERTY_NAME).toString();
            if (!StringUtils.isBlank(operationId)) {
                return operationId;
            }
        }

        return super.getOrGenerateOperationId(operation, path, httpMethod);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objMap) {
        return super.postProcessModels(objMap);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        if (objs != null) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    for (CodegenParameter p: operation.allParams) {
                        if (p.dataType.equals("Dictionary<string, Object>")) {
                            p.dataType = "Object";
                        }
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        property.name = property.name.replaceAll("^_", "");
    }

    @Override
    public String toVarName(String name) {
        boolean isReserved = false;
        if (isReservedWord(name)) {
            isReserved = true;
        }
        String returnValue = super.toVarName(name);
        return isReserved && !returnValue.startsWith("_") ? "_" + returnValue : returnValue;//.replaceAll("^_", "");
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        // Execute super method
        CodegenModel codegenModel = super.fromModel(name, model);

        codegenModel.isPagedResource = true;

        // Check to see if it has all of the interface properties
        for (String s : Arrays.asList("Entities","PageSize","PageNumber","Total","FirstUri","SelfUri","LastUri","NextUri","PreviousUri","PageCount")) {
            if (!codegenModel.allVars.stream().anyMatch(var -> var.name.equals(s))) {
                codegenModel.isPagedResource = false;
                break;
            }
        }

        // Check for other disqualifying conditions
        if (codegenModel.isPagedResource) {
            // Get reference to entities property
            Optional<CodegenProperty> entitiesProperty = codegenModel.allVars.stream().filter(var -> var.name.equals("Entities")).findFirst();

            if (!entitiesProperty.isPresent()) {
                codegenModel.isPagedResource = false;
                return codegenModel;
            }

            // datatypeWithEnum has the correct type including generics. complexType drops them.
            // E.g. datatypeWithEnum=Map<Object, String> and complexType=Map
            codegenModel.pagedResourceType = entitiesProperty.get().datatypeWithEnum;
            if (codegenModel.pagedResourceType.startsWith("List<")) {
                codegenModel.pagedResourceType = codegenModel.pagedResourceType.substring(5,codegenModel.pagedResourceType.length() - 1);
            }
            codegenModel.imports.add("PagedResource");
        }

        // Use our own values for hasMore
        boolean foundLastValidProperty = false;
        for (int i = codegenModel.vars.size() -1; i >= 0; i--) {
            CodegenProperty cp = codegenModel.vars.get(i);

            CodegenProperty items = cp.getItems();
            if (items != null && (items.isArray || items.isContainer)) {
                items.isEnum = false;
            }
            cp.setItems(items);

            if (cp.isArray || cp.isContainer)
                cp.isEnum = false;

            if (cp.datatypeWithEnum.contains("Dictionary<string, InnerEnum>")) {
                cp.datatypeWithEnum = "Dictionary<string, string>";
            }

            // If we've found the last property already, set it and move on
            if (foundLastValidProperty) {
                cp.hasMore = true;

                // Prevent trailing commas from readonly props
                if (cp.isReadOnly) {
                    cp.hasMoreNonReadOnly = false;
                } else {
                    cp.hasMoreNonReadOnly = true;
                }

                continue;
            }

            // If the property isn't readonly, we've found the last valid property
            if (!cp.isReadOnly){
                foundLastValidProperty = true;
                cp.hasMore = false;
                cp.hasMoreNonReadOnly = false;
                continue;
            }
        }

        // Make sure last property in list doesn't think there's more
        if (codegenModel.vars.size() > 0) {
            codegenModel.vars.get(codegenModel.vars.size()-1).hasMore = false;
        }

        // Set hasRequired only if non-readonly properties are required
        codegenModel.hasRequired = false;
        for (CodegenProperty cp : codegenModel.vars) {
            if (!cp.isReadOnly && cp.required) {
                codegenModel.hasRequired = true;
                break;
            }
        }

        return codegenModel;
    }

    @Override
    public String toModelName(String name) {
        if (name.startsWith("PureCloud"))
            return name;
        return super.toModelName(name);
    }
}
