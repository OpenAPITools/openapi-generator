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

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class TerraformProviderCodegen extends AbstractGoCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(TerraformProviderCodegen.class);

    public static final String PROVIDER_NAME = "providerName";
    public static final String PROVIDER_ADDRESS = "providerAddress";
    public static final String PROVIDER_VERSION = "providerVersion";

    protected String providerName = "example";
    protected String providerAddress = "registry.terraform.io/example/example";
    protected String providerVersion = "0.1.0";

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "terraform-provider";
    }

    @Override
    public String getHelp() {
        return "Generates a Terraform provider (Go, using HashiCorp Plugin Framework).";
    }

    public TerraformProviderCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
        );

        outputFolder = "generated-code/terraform-provider";
        embeddedTemplateDir = templateDir = "terraform-provider";

        // API templates: generate per-tag resource, data source, and model files
        apiTemplateFiles.put("resource.mustache", "_resource.go");
        apiTemplateFiles.put("data_source.mustache", "_data_source.go");
        apiTemplateFiles.put("resource_model.mustache", "_model.go");

        // Model templates: generate per-schema Go structs for the client package
        modelTemplateFiles.put("model.mustache", ".go");

        // No doc templates
        apiDocTemplateFiles.clear();
        modelDocTemplateFiles.clear();

        hideGenerationTimestamp = Boolean.TRUE;

        // Override type mappings for Terraform (no time.Time or *os.File)
        typeMapping.put("DateTime", "string");
        typeMapping.put("date", "string");
        typeMapping.put("File", "string");
        typeMapping.put("file", "string");
        typeMapping.put("binary", "string");

        cliOptions.add(new CliOption(PROVIDER_NAME, "Terraform provider name (e.g. 'petstore')")
                .defaultValue(providerName));
        cliOptions.add(new CliOption(PROVIDER_ADDRESS, "Terraform provider registry address")
                .defaultValue(providerAddress));
        cliOptions.add(new CliOption(PROVIDER_VERSION, "Terraform provider version")
                .defaultValue(providerVersion));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(PROVIDER_NAME)) {
            providerName = additionalProperties.get(PROVIDER_NAME).toString();
        }
        additionalProperties.put(PROVIDER_NAME, providerName);

        if (additionalProperties.containsKey(PROVIDER_ADDRESS)) {
            providerAddress = additionalProperties.get(PROVIDER_ADDRESS).toString();
        }
        additionalProperties.put(PROVIDER_ADDRESS, providerAddress);

        if (additionalProperties.containsKey(PROVIDER_VERSION)) {
            providerVersion = additionalProperties.get(PROVIDER_VERSION).toString();
        }
        additionalProperties.put(PROVIDER_VERSION, providerVersion);

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("provider");
        }
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);

        apiPackage = "internal" + File.separator + "provider";
        modelPackage = "internal" + File.separator + "client";

        // Supporting files
        supportingFiles.add(new SupportingFile("main.mustache", "", "main.go"));
        supportingFiles.add(new SupportingFile("provider.mustache", "internal" + File.separator + "provider", "provider.go"));
        supportingFiles.add(new SupportingFile("client.mustache", "internal" + File.separator + "client", "client.go"));
        supportingFiles.add(new SupportingFile("go.mod.mustache", "", "go.mod"));
        supportingFiles.add(new SupportingFile("GNUmakefile.mustache", "", "GNUmakefile"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("provider_example.mustache", "examples" + File.separator + "provider", "provider.tf"));
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + "internal" + File.separator + "provider";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + "internal" + File.separator + "client";
    }

    @Override
    public String toApiFilename(String name) {
        return underscore(name);
    }

    @Override
    public String toModelFilename(String name) {
        return "model_" + underscore(name);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap objectMap = objs.getOperations();
        List<CodegenOperation> operations = objectMap.getOperation();

        // Store original uppercase HTTP method and build fmt.Sprintf-ready paths
        for (CodegenOperation operation : operations) {
            // Store uppercase method for net/http (e.g. "POST", "GET")
            operation.vendorExtensions.put("x-terraform-http-method", operation.httpMethod.toUpperCase(Locale.ROOT));

            // Convert path params to fmt.Sprintf format: /pet/{petId} -> /pet/%v
            if (operation.path != null && operation.pathParams != null && !operation.pathParams.isEmpty()) {
                String fmtPath = operation.path;
                for (CodegenParameter param : operation.pathParams) {
                    fmtPath = fmtPath.replace("{" + param.baseName + "}", "%v");
                }
                operation.vendorExtensions.put("x-terraform-path-fmt", fmtPath);
            }

            // http method verb conversion (e.g. PUT => Put) as parent does
            operation.httpMethod = camelize(operation.httpMethod.toLowerCase(Locale.ROOT));
        }

        // CRUD detection per tag
        CodegenOperation createOp = null;
        CodegenOperation readOp = null;
        CodegenOperation updateOp = null;
        CodegenOperation deleteOp = null;
        CodegenOperation listOp = null;

        // First pass: check for explicit vendor extensions
        for (CodegenOperation op : operations) {
            if (getBooleanVendorExtension(op, "x-terraform-exclude")) {
                continue;
            }
            if (getBooleanVendorExtension(op, "x-terraform-is-create") && createOp == null) {
                createOp = op;
            }
            if (getBooleanVendorExtension(op, "x-terraform-is-read") && readOp == null) {
                readOp = op;
            }
            if (getBooleanVendorExtension(op, "x-terraform-is-update") && updateOp == null) {
                updateOp = op;
            }
            if (getBooleanVendorExtension(op, "x-terraform-is-delete") && deleteOp == null) {
                deleteOp = op;
            }
            if (getBooleanVendorExtension(op, "x-terraform-is-list") && listOp == null) {
                listOp = op;
            }
        }

        // Second pass: auto-detect using REST pattern methods
        for (CodegenOperation op : operations) {
            if (getBooleanVendorExtension(op, "x-terraform-exclude")) {
                continue;
            }
            if (createOp == null && op.isRestfulCreate()) {
                createOp = op;
            } else if (readOp == null && op.isRestfulShow()) {
                readOp = op;
            } else if (updateOp == null && op.isRestfulUpdate()) {
                updateOp = op;
            } else if (deleteOp == null && op.isRestfulDestroy()) {
                deleteOp = op;
            } else if (listOp == null && op.isRestfulIndex()) {
                listOp = op;
            }
        }

        // Mark the selected operations with vendor extensions
        if (createOp != null) {
            createOp.vendorExtensions.put("x-terraform-is-create", true);
        }
        if (readOp != null) {
            readOp.vendorExtensions.put("x-terraform-is-read", true);
        }
        if (updateOp != null) {
            updateOp.vendorExtensions.put("x-terraform-is-update", true);
        }
        if (deleteOp != null) {
            deleteOp.vendorExtensions.put("x-terraform-is-delete", true);
        }
        if (listOp != null) {
            listOp.vendorExtensions.put("x-terraform-is-list", true);
        }

        // Tag-level flags and CRUD details for template rendering
        objectMap.put("hasCreate", createOp != null);
        objectMap.put("hasRead", readOp != null);
        objectMap.put("hasUpdate", updateOp != null);
        objectMap.put("hasDelete", deleteOp != null);
        objectMap.put("hasList", listOp != null);

        // Store CRUD operation details at tag level for simplified template access
        if (createOp != null) {
            objectMap.put("createMethod", createOp.vendorExtensions.get("x-terraform-http-method"));
            objectMap.put("createPath", createOp.path);
        }
        if (readOp != null) {
            objectMap.put("readMethod", readOp.vendorExtensions.get("x-terraform-http-method"));
            objectMap.put("readPath", readOp.vendorExtensions.getOrDefault("x-terraform-path-fmt", readOp.path));
            objectMap.put("readHasPathParams", readOp.pathParams != null && !readOp.pathParams.isEmpty());
        }
        if (updateOp != null) {
            objectMap.put("updateMethod", updateOp.vendorExtensions.get("x-terraform-http-method"));
            objectMap.put("updatePath", updateOp.vendorExtensions.getOrDefault("x-terraform-path-fmt", updateOp.path));
            objectMap.put("updateHasPathParams", updateOp.pathParams != null && !updateOp.pathParams.isEmpty());
        }
        if (deleteOp != null) {
            objectMap.put("deleteMethod", deleteOp.vendorExtensions.get("x-terraform-http-method"));
            objectMap.put("deletePath", deleteOp.vendorExtensions.getOrDefault("x-terraform-path-fmt", deleteOp.path));
            objectMap.put("deleteHasPathParams", deleteOp.pathParams != null && !deleteOp.pathParams.isEmpty());
        }

        // Determine resource name from tag
        String tag = objectMap.getClassname();
        // Strip common suffixes like "Api", "API" from the tag name
        String cleanTag = tag.replaceAll("(?i)api$", "");
        if (cleanTag.isEmpty()) {
            cleanTag = tag;
        }
        String resourceName = underscore(cleanTag).toLowerCase(Locale.ROOT);
        // Allow override via x-terraform-resource-name on any operation
        for (CodegenOperation op : operations) {
            Object nameOverride = op.vendorExtensions.get("x-terraform-resource-name");
            if (nameOverride != null) {
                resourceName = nameOverride.toString();
                break;
            }
        }
        objectMap.put("resourceName", resourceName);
        objectMap.put("resourceClassName", camelize(resourceName));

        // Detect ID field from read operation path params
        String idField = "id";
        if (readOp != null && readOp.pathParams != null && !readOp.pathParams.isEmpty()) {
            idField = readOp.pathParams.get(readOp.pathParams.size() - 1).paramName;
        }
        objectMap.put("idField", idField);

        // Build model info for the resource schema
        // Use the response type from readOp (or createOp) as the resource model
        String responseModel = null;
        if (readOp != null && readOp.returnType != null) {
            responseModel = readOp.returnType;
        } else if (createOp != null && createOp.returnType != null) {
            responseModel = createOp.returnType;
        }
        objectMap.put("responseModel", responseModel);

        // Build request model from createOp body params
        String requestModel = null;
        if (createOp != null && createOp.bodyParam != null && createOp.bodyParam.dataType != null) {
            requestModel = createOp.bodyParam.dataType;
        }
        objectMap.put("requestModel", requestModel);

        // Collect model properties for schema generation and resolve ID field
        String idModelGoName = camelize(idField);
        String idModelGoType = "string";

        if (responseModel != null) {
            for (ModelMap modelMap : allModels) {
                CodegenModel model = modelMap.getModel();
                if (model.classname.equals(responseModel)) {
                    List<Map<String, Object>> tfAttributes = new ArrayList<>();
                    boolean hasListAttributes = false;
                    for (CodegenProperty prop : model.vars) {
                        Map<String, Object> attr = new HashMap<>();
                        attr.put("name", prop.baseName);
                        attr.put("terraformName", underscore(prop.baseName).toLowerCase(Locale.ROOT));
                        attr.put("goName", camelize(prop.baseName));
                        attr.put("goType", prop.dataType);
                        attr.put("terraformType", goTypeToTerraformType(prop.dataType));
                        attr.put("terraformAttrType", goTypeToTerraformAttrType(prop.dataType, prop));
                        attr.put("isRequired", prop.required);
                        attr.put("isComputed", prop.isReadOnly);
                        attr.put("isOptional", !prop.required && !prop.isReadOnly);
                        attr.put("description", prop.description != null ? prop.description : "");
                        attr.put("isSensitive", prop.isWriteOnly || getBooleanVendorExtension(prop, "x-terraform-sensitive"));
                        attr.put("isString", "string".equals(prop.dataType));
                        attr.put("isInt64", "int64".equals(prop.dataType) || "int32".equals(prop.dataType));
                        attr.put("isFloat64", "float64".equals(prop.dataType) || "float32".equals(prop.dataType));
                        attr.put("isBool", "bool".equals(prop.dataType));
                        attr.put("isList", prop.isArray);
                        attr.put("isObject", prop.isModel && !prop.isArray);
                        if (prop.isArray) {
                            hasListAttributes = true;
                            if (prop.items != null) {
                                attr.put("listElementType", goTypeToTerraformElementType(prop.items.dataType));
                            }
                        }
                        tfAttributes.add(attr);
                    }
                    objectMap.put("tfAttributes", tfAttributes);
                    objectMap.put("hasListAttributes", hasListAttributes);

                    // Resolve ID field: match path param name to a model property
                    boolean idResolved = false;
                    // Try exact match on baseName (case-insensitive)
                    for (CodegenProperty prop : model.vars) {
                        if (prop.baseName.equalsIgnoreCase(idField)) {
                            idModelGoName = camelize(prop.baseName);
                            idModelGoType = prop.dataType;
                            idResolved = true;
                            break;
                        }
                    }
                    // Try stripping resource name prefix (e.g., petId -> id)
                    if (!idResolved) {
                        String strippedId = idField.replaceFirst("(?i)^" + resourceName, "");
                        if (!strippedId.isEmpty()) {
                            for (CodegenProperty prop : model.vars) {
                                if (prop.baseName.equalsIgnoreCase(strippedId)) {
                                    idModelGoName = camelize(prop.baseName);
                                    idModelGoType = prop.dataType;
                                    idResolved = true;
                                    break;
                                }
                            }
                        }
                    }
                    // Fall back to "id" property
                    if (!idResolved) {
                        for (CodegenProperty prop : model.vars) {
                            if ("id".equalsIgnoreCase(prop.baseName)) {
                                idModelGoName = camelize(prop.baseName);
                                idModelGoType = prop.dataType;
                                break;
                            }
                        }
                    }

                    break;
                }
            }
        }

        objectMap.put("idFieldExported", idModelGoName);

        // Determine the value accessor for the ID field based on its type
        String idValueAccessor;
        if ("int64".equals(idModelGoType) || "int32".equals(idModelGoType) || "int".equals(idModelGoType)) {
            idValueAccessor = ".ValueInt64()";
        } else if ("float64".equals(idModelGoType) || "float32".equals(idModelGoType)) {
            idValueAccessor = ".ValueFloat64()";
        } else {
            idValueAccessor = ".ValueString()";
        }
        objectMap.put("idFieldValueAccessor", idValueAccessor);

        // Terraform name for the ID field (for path.Root in ImportState)
        objectMap.put("idFieldTerraformName", underscore(idModelGoName).toLowerCase(Locale.ROOT));

        return objs;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // Call parent to handle Go-specific post-processing
        objs = super.postProcessModels(objs);

        for (ModelMap m : objs.getModels()) {
            CodegenModel model = m.getModel();
            for (CodegenProperty prop : model.vars) {
                // Ensure x-go-datatag is set for all model vars.
                // The parent AbstractGoCodegen only sets x-go-datatag on
                // inheritedProperties (oneOf/anyOf entries) when the model has
                // composed schemas without allOf, leaving model.vars without
                // json tags. We fix this by generating the tag here when missing.
                if (!prop.vendorExtensions.containsKey("x-go-datatag")) {
                    String goDataTag = "json:\"" + prop.baseName;
                    if (!prop.required) {
                        goDataTag += ",omitempty";
                    }
                    goDataTag += "\"";
                    goDataTag = " `" + goDataTag + "`";
                    prop.vendorExtensions.put("x-go-datatag", goDataTag);
                }

                // Add Terraform-specific vendor extensions
                if (prop.isReadOnly) {
                    prop.vendorExtensions.put("x-terraform-computed", true);
                }
                if (prop.required) {
                    prop.vendorExtensions.put("x-terraform-required", true);
                }
                if (!prop.required && !prop.isReadOnly) {
                    prop.vendorExtensions.put("x-terraform-optional", true);
                }
                if (prop.isWriteOnly) {
                    prop.vendorExtensions.put("x-terraform-sensitive", true);
                }

                // Map Go types to Terraform types
                prop.vendorExtensions.put("x-terraform-type", goTypeToTerraformType(prop.dataType));
                prop.vendorExtensions.put("x-terraform-attr-type", goTypeToTerraformAttrType(prop.dataType, prop));
            }
        }

        return objs;
    }

    private String goTypeToTerraformType(String goType) {
        if (goType == null) return "types.String";
        switch (goType) {
            case "string":
                return "types.String";
            case "int32":
            case "int64":
            case "int":
                return "types.Int64";
            case "float32":
            case "float64":
                return "types.Float64";
            case "bool":
                return "types.Bool";
            default:
                if (goType.startsWith("[]")) {
                    return "types.List";
                }
                return "types.String";
        }
    }

    private String goTypeToTerraformAttrType(String goType, CodegenProperty prop) {
        if (goType == null) return "schema.StringAttribute";
        switch (goType) {
            case "string":
                return "schema.StringAttribute";
            case "int32":
            case "int64":
            case "int":
                return "schema.Int64Attribute";
            case "float32":
            case "float64":
                return "schema.Float64Attribute";
            case "bool":
                return "schema.BoolAttribute";
            default:
                if (goType.startsWith("[]")) {
                    return "schema.ListAttribute";
                }
                // Complex objects are serialized as JSON strings
                return "schema.StringAttribute";
        }
    }

    private String goTypeToTerraformElementType(String goType) {
        if (goType == null) return "types.StringType";
        switch (goType) {
            case "string":
                return "types.StringType";
            case "int32":
            case "int64":
            case "int":
                return "types.Int64Type";
            case "float32":
            case "float64":
                return "types.Float64Type";
            case "bool":
                return "types.BoolType";
            default:
                return "types.StringType";
        }
    }

    private boolean getBooleanVendorExtension(CodegenOperation op, String key) {
        return op.vendorExtensions.containsKey(key) && Boolean.TRUE.equals(op.vendorExtensions.get(key));
    }

    private boolean getBooleanVendorExtension(CodegenProperty prop, String key) {
        return prop.vendorExtensions.containsKey(key) && Boolean.TRUE.equals(prop.vendorExtensions.get(key));
    }
}
