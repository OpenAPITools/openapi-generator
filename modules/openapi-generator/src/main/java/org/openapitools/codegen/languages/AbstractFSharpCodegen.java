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

import com.google.common.collect.ImmutableMap.Builder;
import com.samskivert.mustache.Mustache.Lambda;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.templating.mustache.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public abstract class AbstractFSharpCodegen extends DefaultCodegen implements CodegenConfig {

    protected boolean optionalAssemblyInfoFlag = true;
    protected boolean optionalProjectFileFlag = true;

    protected boolean useDateTimeOffsetFlag = false;
    protected boolean useCollection = false;
    protected boolean returnICollection = false;
    protected boolean netCoreProjectFileFlag = false;

    protected String modelPropertyNaming = CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.PascalCase.name();

    protected String licenseUrl = "http://localhost";
    protected String licenseName = "NoLicense";

    protected String packageVersion = "1.0.0";
    protected String packageName = "OpenAPI";
    protected String packageTitle = "OpenAPI Library";
    protected String packageProductName = "OpenAPILibrary";
    protected String packageDescription = "A library generated from a OpenAPI doc";
    protected String packageCompany = "OpenAPI";
    protected String packageCopyright = "No Copyright";
    protected String packageAuthors = "OpenAPI";

    protected String interfacePrefix = "I";

    protected String projectFolder = packageName;
    protected String sourceFolder = projectFolder + File.separator + "src";
    protected String testFolder = projectFolder + ".Tests";

    protected Set<String> collectionTypes;
    protected Set<String> mapTypes;

    // true if nullable types will be supported (as option)
    protected boolean supportNullable = Boolean.TRUE;

    protected Set<String> nullableType = new HashSet<String>();


    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractFSharpCodegen.class);

    public AbstractFSharpCodegen() {
        super();

        supportsInheritance = true;

        importMapping.clear();
        importMapping.put("IDictionary", "System.Collections.Generic");

        outputFolder = this.getName();
        embeddedTemplateDir = templateDir = this.getName();

        collectionTypes = new HashSet<String>(Arrays.asList("list", "seq"));

        mapTypes = new HashSet<String>(
                Arrays.asList("IDictionary")
        );

        reservedWords.addAll(
                Arrays.asList(
                        // local variable names in API methods (endpoints)
                        "localVarPath", "localVarPathParams", "localVarQueryParams", "localVarHeaderParams",
                        "localVarFormParams", "localVarFileParams", "localVarStatusCode", "localVarResponse",
                        "localVarPostBody", "localVarHttpHeaderAccepts", "localVarHttpHeaderAccept",
                        "localVarHttpContentTypes", "localVarHttpContentType",
                        "localVarStatusCode",
                        // F# reserved words
                        "abstract", "and", "as", "async", "await", "assert", "base", "begin", "bool", "break", "byte", "case", "catch", "char", "checked",
                        "class", "const", "continue", "decimal", "default", "delegate", "do", "done", "double", "downcast", "downto", "dynamic",
                        "elif", "else", "end", "enum", "event", "exception", "explicit", "extern", "false", "finally", "fixed", "float", "for",
                        "foreach", "fun", "function", "if", "in", "inherit", "inline", "int", "interface", "internal", "is", "lazy", "let", "let!", "lock",
                        "match", "match!", "member", "module", "mutable", "namespace", "new", "not", "null", "of", "open", "option", "or", "override", "params",
                        "private", "public", "raise", "rec", "return", "return!", "sealed", "select", "static", "string", "struct", "then", "to",
                        "true", "try", "type", "upcast", "use", "use!", "val", "void", "volatile", "when", "while", "with", "yield", "yield!")
        );

        // TODO - these are based on C# generator, do we need to add any more?
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "string",
                        "bool",
                        "char",
                        "decimal",
                        "int",
                        "int16",
                        "int64",
                        "nativeint",
                        "unativeint",
                        "uint16",
                        "uint32",
                        "uint64",
                        "float",
                        "byte[]",
                        "ICollection",
                        "Collection",
                        "list",
                        "dict",
                        "seq",
                        "Dictionary",
                        "List",
                        "DateTime",
                        "DataTimeOffset",
                        "Double",
                        "Int32",
                        "Int64",
                        "float",
                        "float32",
                        "single",
                        "double",
                        "System.IO.Stream", // not really a primitive, we include it to avoid model import
                        "obj")
        );

        instantiationTypes.put("array", "list");
        instantiationTypes.put("list", "list");
        instantiationTypes.put("map", "IDictionary");


        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "string");
        typeMapping.put("binary", "byte[]");
        typeMapping.put("ByteArray", "byte[]");
        typeMapping.put("boolean", "bool");
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("long", "int64");
        typeMapping.put("double", "double");
        typeMapping.put("number", "decimal");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("date", "DateTime");
        typeMapping.put("file", "System.IO.Stream");
        typeMapping.put("array", "list");
        typeMapping.put("list", "list");
        typeMapping.put("map", "IDictionary");
        typeMapping.put("object", "obj");
        typeMapping.put("UUID", "Guid");
        typeMapping.put("URI", "string");

        // nullable type
        nullableType = new HashSet<String>(
                Arrays.asList("decimal", "bool", "int", "float", "long", "double", "string", "Guid", "apiKey")
        );
    }

    public void setReturnICollection(boolean returnICollection) {
        this.returnICollection = returnICollection;
    }

    public void setUseCollection(boolean useCollection) {
        this.useCollection = useCollection;
        if (useCollection) {
            typeMapping.put("array", "seq");
            typeMapping.put("list", "seq");

            instantiationTypes.put("array", "seq");
            instantiationTypes.put("list", "seq");
        }
    }

    public void setNetCoreProjectFileFlag(boolean flag) {
        this.netCoreProjectFileFlag = flag;
    }

    public void useDateTimeOffset(boolean flag) {
        this.useDateTimeOffsetFlag = flag;
        if (flag) {
            typeMapping.put("DateTime", "DateTimeOffset?");
        } else {
            typeMapping.put("DateTime", "DateTime?");
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // License info
        if (additionalProperties.containsKey(CodegenConstants.LICENSE_URL)) {
            setLicenseUrl((String) additionalProperties.get(CodegenConstants.LICENSE_URL));
        } else {
            additionalProperties.put(CodegenConstants.LICENSE_URL, this.licenseUrl);
        }

        if (additionalProperties.containsKey(CodegenConstants.LICENSE_NAME)) {
            setLicenseName((String) additionalProperties.get(CodegenConstants.LICENSE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.LICENSE_NAME, this.licenseName);
        }

        // {{packageVersion}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        }

        // {{sourceFolder}}
        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        } else {
            additionalProperties.put(CodegenConstants.SOURCE_FOLDER, this.sourceFolder);
        }

        // {{packageName}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            LOGGER.warn(String.format(Locale.ROOT, "%s is not used by F# generators. Please use %s",
                    CodegenConstants.INVOKER_PACKAGE, CodegenConstants.PACKAGE_NAME));
        }

        // {{packageTitle}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_TITLE)) {
            setPackageTitle((String) additionalProperties.get(CodegenConstants.PACKAGE_TITLE));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_TITLE, packageTitle);
        }

        // {{packageProductName}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_PRODUCTNAME)) {
            setPackageProductName((String) additionalProperties.get(CodegenConstants.PACKAGE_PRODUCTNAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_PRODUCTNAME, packageProductName);
        }

        // {{packageDescription}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_DESCRIPTION)) {
            setPackageDescription((String) additionalProperties.get(CodegenConstants.PACKAGE_DESCRIPTION));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_DESCRIPTION, packageDescription);
        }

        // {{packageCompany}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_COMPANY)) {
            setPackageCompany((String) additionalProperties.get(CodegenConstants.PACKAGE_COMPANY));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_COMPANY, packageCompany);
        }

        // {{packageCopyright}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_COPYRIGHT)) {
            setPackageCopyright((String) additionalProperties.get(CodegenConstants.PACKAGE_COPYRIGHT));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_COPYRIGHT, packageCopyright);
        }

        // {{packageAuthors}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_AUTHORS)) {
            setPackageAuthors((String) additionalProperties.get(CodegenConstants.PACKAGE_AUTHORS));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_AUTHORS, packageAuthors);
        }

        // {{useDateTimeOffset}}
        if (additionalProperties.containsKey(CodegenConstants.USE_DATETIME_OFFSET)) {
            useDateTimeOffset(convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_DATETIME_OFFSET));
        } else {
            additionalProperties.put(CodegenConstants.USE_DATETIME_OFFSET, useDateTimeOffsetFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.USE_COLLECTION)) {
            setUseCollection(convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_COLLECTION));
        } else {
            additionalProperties.put(CodegenConstants.USE_COLLECTION, useCollection);
        }

        if (additionalProperties.containsKey(CodegenConstants.RETURN_ICOLLECTION)) {
            setReturnICollection(convertPropertyToBooleanAndWriteBack(CodegenConstants.RETURN_ICOLLECTION));
        } else {
            additionalProperties.put(CodegenConstants.RETURN_ICOLLECTION, returnICollection);
        }

        if (additionalProperties.containsKey(CodegenConstants.NETCORE_PROJECT_FILE)) {
            setNetCoreProjectFileFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.NETCORE_PROJECT_FILE));
        } else {
            additionalProperties.put(CodegenConstants.NETCORE_PROJECT_FILE, netCoreProjectFileFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.INTERFACE_PREFIX)) {
            String useInterfacePrefix = additionalProperties.get(CodegenConstants.INTERFACE_PREFIX).toString();
            if ("false".equals(useInterfacePrefix.toLowerCase(Locale.ROOT))) {
                setInterfacePrefix("");
            } else if (!"true".equals(useInterfacePrefix.toLowerCase(Locale.ROOT))) {
                // NOTE: if user passes "true" explicitly, we use the default I- prefix. The other supported case here is a custom prefix.
                setInterfacePrefix(sanitizeName(useInterfacePrefix));
            }
        }

        // This either updates additionalProperties with the above fixes, or sets the default if the option was not specified.
        additionalProperties.put(CodegenConstants.INTERFACE_PREFIX, interfacePrefix);
    }

    @Override
    protected Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("camelcase_param", new CamelCaseLambda().generator(this).escapeAsParamName(true));
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        super.postProcessModels(objs);
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            for (CodegenProperty var : cm.vars) {
                // check to see if model name is same as the property name
                // which will result in compilation error
                // if found, prepend with _ to workaround the limitation
                if (var.name.equalsIgnoreCase(cm.name)) {
                    var.name = "_" + var.name;
                }
            }
        }
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    /**
     * Invoked by {@link DefaultGenerator} after all models have been post-processed, allowing for a last pass of codegen-specific model cleanup.
     *
     * @param objs Current state of codegen object model.
     * @return (ew) modified state of the codegen object model.
     */
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        final Map<String, Object> processed = super.postProcessAllModels(objs);
        postProcessEnumRefs(processed);
        return postProcessDependencyOrders(processed);
    }

    /*
     * F# does not allow forward declarations, so files must be imported in the correct order.
     * Output of CodeGen models must therefore bein dependency order (rather than alphabetical order, which seems to be the default).
     * We achieve this by creating a comparator to check whether the first model contains any properties of the comparison model's type
     * This could probably be made more efficient if absolutely needed.
     */
    @SuppressWarnings({"unchecked"})
    public Map<String, Object> postProcessDependencyOrders(final Map<String, Object> objs) {
        Comparator<String> comparator = new Comparator<String>() {
            @Override
            public int compare(String key1, String key2) {
                // Get the corresponding models
                CodegenModel model1 = ModelUtils.getModelByName(key1, objs);
                CodegenModel model2 = ModelUtils.getModelByName(key2, objs);

                List<String> complexVars1 = new ArrayList<String>();
                List<String> complexVars2 = new ArrayList<String>();

                for (CodegenProperty prop : model1.vars) {
                    if (prop.complexType != null)
                        complexVars1.add(prop.complexType);
                }
                for (CodegenProperty prop : model2.vars) {
                    if (prop.complexType != null)
                        complexVars2.add(prop.complexType);
                }

                // if first has complex vars and second has none, first is greater
                if (complexVars1.size() > 0 && complexVars2.size() == 0)
                    return 1;

                // if second has complex vars and first has none, first is lesser
                if (complexVars1.size() == 0 && complexVars2.size() > 0)
                    return -1;

                // if first has complex var that matches the second's key, first is greater
                if (complexVars1.contains(key2))
                    return 1;

                // if second has complex var that matches the first's key, first is lesser
                if (complexVars2.contains(key1))
                    return -1;

                // if none of the above, don't care
                return 0;

            }
        };
        PriorityQueue<String> queue = new PriorityQueue<String>(objs.size(), comparator);
        for (Object k : objs.keySet()) {
            queue.add(k.toString());
        }

        Map<String, Object> sorted = new LinkedHashMap<String, Object>();

        while (queue.size() > 0) {
            String key = queue.poll();
            sorted.put(key, objs.get(key));
        }
        return sorted;
    }

    /**
     * F# differs from other languages in that Enums are not _true_ objects; enums are compiled to integral types.
     * So, in F#, an enum is considers more like a user-defined primitive.
     * <p>
     * When working with enums, we can't always assume a RefModel is a nullable type (where default(YourType) == null),
     * so this post processing runs through all models to find RefModel'd enums. Then, it runs through all vars and modifies
     * those vars referencing RefModel'd enums to work the same as inlined enums rather than as objects.
     *
     * @param models processed models to be further processed for enum references
     */
    @SuppressWarnings({"unchecked"})
    private void postProcessEnumRefs(final Map<String, Object> models) {
        Map<String, CodegenModel> enumRefs = new HashMap<String, CodegenModel>();
        for (Map.Entry<String, Object> entry : models.entrySet()) {
            CodegenModel model = ModelUtils.getModelByName(entry.getKey(), models);
            if (model.isEnum) {
                enumRefs.put(entry.getKey(), model);
            }
        }

        for (Map.Entry<String, Object> entry : models.entrySet()) {
            String openAPIName = entry.getKey();
            CodegenModel model = ModelUtils.getModelByName(openAPIName, models);
            if (model != null) {
                for (CodegenProperty var : model.allVars) {
                    if (enumRefs.containsKey(var.dataType)) {
                        // Handle any enum properties referred to by $ref.
                        // This is different in F# than most other generators, because enums in C# are compiled to integral types,
                        // while enums in many other languages are true objects.
                        CodegenModel refModel = enumRefs.get(var.dataType);
                        var.allowableValues = refModel.allowableValues;
                        var.isEnum = true;

                        // We do these after updateCodegenPropertyEnum to avoid generalities that don't mesh with C#.
                        var.isPrimitiveType = true;
                    }
                }

                // We're looping all models here.
                if (model.isEnum) {
                    // We now need to make allowableValues.enumVars look like the context of CodegenProperty
                    Boolean isString = false;
                    Boolean isInteger = false;
                    Boolean isLong = false;
                    Boolean isByte = false;

                    if (model.dataType.startsWith("byte")) {
                        // F# Actually supports byte and short enums, swagger spec only supports byte.
                        isByte = true;
                        model.vendorExtensions.put("x-enum-byte", true);
                    } else if (model.dataType.startsWith("int32")) {
                        isInteger = true;
                        model.vendorExtensions.put("x-enum-integer", true);
                    } else if (model.dataType.startsWith("int64")) {
                        isLong = true;
                        model.vendorExtensions.put("x-enum-long", true);
                    } else {
                        // F# doesn't support non-integral enums, so we need to treat everything else as strings (e.g. to not lose precision or data integrity)
                        isString = true;
                        model.vendorExtensions.put("x-enum-string", true);
                    }

                    // Since we iterate enumVars for modelnnerEnum and enumClass templates, and CodegenModel is missing some of CodegenProperty's properties,
                    // we can take advantage of Mustache's contextual lookup to add the same "properties" to the model's enumVars scope rather than CodegenProperty's scope.
                    List<Map<String, String>> enumVars = (ArrayList<Map<String, String>>) model.allowableValues.get("enumVars");
                    List<Map<String, Object>> newEnumVars = new ArrayList<Map<String, Object>>();
                    for (Map<String, String> enumVar : enumVars) {
                        Map<String, Object> mixedVars = new HashMap<String, Object>();
                        mixedVars.putAll(enumVar);

                        mixedVars.put("isString", isString);
                        mixedVars.put("isLong", isLong);
                        mixedVars.put("isInteger", isInteger);
                        mixedVars.put("isByte", isByte);

                        newEnumVars.add(mixedVars);
                    }

                    if (!newEnumVars.isEmpty()) {
                        model.allowableValues.put("enumVars", newEnumVars);
                    }
                }
            } else {
                LOGGER.warn("Expected to retrieve model %s by name, but no model was found. Check your -Dmodels inclusions.", openAPIName);
            }
        }
    }

    /**
     * Update codegen property's enum by adding "enumVars" (with name and value)
     *
     * @param var list of CodegenProperty
     */
    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        if (var.vendorExtensions == null) {
            var.vendorExtensions = new HashMap<>();
        }

        super.updateCodegenPropertyEnum(var);

        // Because C# uses nullable primitives for datatype, and datatype is used in DefaultCodegen for determining enum-ness, guard against weirdness here.
        if (var.isEnum) {
            if ("byte".equals(var.dataFormat)) {// C# Actually supports byte and short enums.
                var.vendorExtensions.put("x-enum-byte", true);
                var.isString = false;
                var.isLong = false;
                var.isInteger = false;
            } else if ("int32".equals(var.dataFormat)) {
                var.isInteger = true;
                var.isString = false;
                var.isLong = false;
            } else if ("int64".equals(var.dataFormat)) {
                var.isLong = true;
                var.isString = false;
                var.isInteger = false;
            } else {// C# doesn't support non-integral enums, so we need to treat everything else as strings (e.g. to not lose precision or data integrity)
                var.isString = true;
                var.isInteger = false;
                var.isLong = false;
            }
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        if (objs != null) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {

                    // Check return types for collection
                    if (operation.returnType != null) {
                        String typeMapping;
                        int namespaceEnd = operation.returnType.lastIndexOf(".");
                        if (namespaceEnd > 0) {
                            typeMapping = operation.returnType.substring(namespaceEnd);
                        } else {
                            typeMapping = operation.returnType;
                        }

                        if (this.collectionTypes.contains(typeMapping)) {
                            operation.isListContainer = true;
                            operation.returnContainer = operation.returnType;
                            if (this.returnICollection && (
                                    typeMapping.startsWith("List") ||
                                            typeMapping.startsWith("Collection"))) {
                                // NOTE: ICollection works for both List<T> and Collection<T>
                                int genericStart = typeMapping.indexOf("<");
                                if (genericStart > 0) {
                                    operation.returnType = "ICollection" + typeMapping.substring(genericStart);
                                }
                            }
                        } else {
                            operation.returnContainer = operation.returnType;
                            operation.isMapContainer = this.mapTypes.contains(typeMapping);
                        }
                    }

                    if (operation.examples != null) {
                        for (Map<String, String> example : operation.examples) {
                            for (Map.Entry<String, String> entry : example.entrySet()) {
                                // Replace " with \", \r, \n with \\r, \\n
                                String val = entry.getValue().replace("\"", "\\\"")
                                        .replace("\r", "\\r")
                                        .replace("\n", "\\n");
                                entry.setValue(val);
                            }
                        }
                    }

                    if (!isSupportNullable()) {
                        for (CodegenParameter parameter : operation.allParams) {
                            CodegenModel model = null;
                            for (Object modelHashMap : allModels) {
                                CodegenModel codegenModel = ((HashMap<String, CodegenModel>) modelHashMap).get("model");
                                if (codegenModel.getClassname().equals(parameter.dataType)) {
                                    model = codegenModel;
                                    break;
                                }
                            }

                            if (model == null) {
                                // Primitive data types all come already marked
                                parameter.isNullable = true;
                            } else {
                                // Effectively mark enum models as enums and non-nullable
                                if (model.isEnum) {
                                    parameter.isEnum = true;
                                    parameter.allowableValues = model.allowableValues;
                                    parameter.isPrimitiveType = true;
                                    parameter.isNullable = false;
                                } else {
                                    parameter.isNullable = true;
                                }
                            }
                        }
                    }

                    processOperation(operation);
                }
            }
        }

        return objs;
    }

    protected void processOperation(CodegenOperation operation) {
        // default noop
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage();
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage();
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
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

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn(operationId + " (starting with a number) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return camelize(sanitizeName(operationId));
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize the variable name
        // pet_id => PetId
        name = camelize(name);
        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }
        return name;
    }

    @Override
    public String toParamName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize(lower) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    /**
     * Return the example value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the example value of the property
     */
    @Override
    public String toExampleValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getExample() != null) {
                return "\"" + p.getExample().toString() + "\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getExample() != null) {
                return p.getExample().toString();
            }
        } else if (ModelUtils.isDateSchema(p)) {
            // TODO
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getExample() != null) {
                return p.getExample().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getExample() != null) {
                return p.getExample().toString();
            }
        }

        return null;
    }

    /**
     * Return the default value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isDateSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            }
        } else if (ModelUtils.isDateTimeSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                if (ModelUtils.isFloatSchema(p)) { // float
                    return p.getDefault().toString() + "F";
                } else if (ModelUtils.isDoubleSchema(p)) { // double
                    return p.getDefault().toString() + "D";
                } else {    // decimal
                    return p.getDefault().toString() + "M";
                }
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                String _default = (String) p.getDefault();
                if (p.getEnum() == null) {
                    return "\"" + _default + "\"";
                } else {
                    // convert to enum var name later in postProcessModels
                    return _default;
                }
            }
        }

        return null;
    }

    @Override
    protected boolean isReservedWord(String word) {
        return reservedWords.contains(word);
    }


    public String getNullableType(Schema p, String type) {
        if (languageSpecificPrimitives.contains(type)) {
            if (isSupportNullable() && ModelUtils.isNullable(p) && nullableType.contains(type)) {
                return type + " option";
            } else {
                return type;
            }
        } else {
            return null;
        }
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;

        if (openAPIType == null) {
            LOGGER.error("OpenAPI Type for {} is null. Default to UNKNOWN_OPENAPI_TYPE instead.", p.getName());
            openAPIType = "UNKNOWN_OPENAPI_TYPE";
        }

        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            String languageType = getNullableType(p, type);
            if (languageType != null) {
                return languageType;
            }
        } else {
            type = openAPIType;
        }

        return toModelName(type);
    }

    /**
     * Provides F# strongly typed declaration for simple arrays of some type and arrays of arrays of some type.
     *
     * @param arr The input array property
     * @return The type declaration when the type is an array of arrays.
     */
    private String getArrayTypeDeclaration(ArraySchema arr) {
        // TODO: collection type here should be fully qualified namespace to avoid model conflicts
        // This supports arrays of arrays.
        String arrayType = typeMapping.get("array");
        StringBuilder instantiationType = new StringBuilder(arrayType);
        Schema items = arr.getItems();
        String nestedType = getTypeDeclaration(items);
        // TODO: We may want to differentiate here between generics and primitive arrays.
        return nestedType + "[]";
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            return getArrayTypeDeclaration((ArraySchema) p);
        }
        return super.toInstantiationType(p);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            return getArrayTypeDeclaration((ArraySchema) p);
        } else if (ModelUtils.isMapSchema(p)) {
            // Should we also support maps of maps?
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return getSchemaType(p) + "<string, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

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
    public String apiTestFileFolder() {
        return outputFolder + File.separator + testFolder;
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + testFolder;
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiName(name) + "Tests";
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelName(name) + "Tests";
    }

    public void setLicenseUrl(String licenseUrl) {
        this.licenseUrl = licenseUrl;
    }

    public void setLicenseName(String licenseName) {
        this.licenseName = licenseName;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
        this.projectFolder = packageName;
        this.sourceFolder = projectFolder + File.separator + "src";
        this.testFolder = projectFolder + ".Tests";
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setPackageTitle(String packageTitle) {
        this.packageTitle = packageTitle;
    }

    public void setPackageProductName(String packageProductName) {
        this.packageProductName = packageProductName;
    }

    public void setPackageDescription(String packageDescription) {
        this.packageDescription = packageDescription;
    }

    public void setPackageCompany(String packageCompany) {
        this.packageCompany = packageCompany;
    }

    public void setPackageCopyright(String packageCopyright) {
        this.packageCopyright = packageCopyright;
    }

    public void setPackageAuthors(String packageAuthors) {
        this.packageAuthors = packageAuthors;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public String getInterfacePrefix() {
        return interfacePrefix;
    }

    public void setInterfacePrefix(final String interfacePrefix) {
        this.interfacePrefix = interfacePrefix;
    }

    public boolean isSupportNullable() {
        return supportNullable;
    }

    public void setSupportNullable(final boolean supportNullable) {
        this.supportNullable = supportNullable;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        // C# only supports enums as literals for int, int?, long, long?, byte, and byte?. All else must be treated as strings.
        // Per: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/enum
        // The approved types for an enum are byte, sbyte, short, ushort, int, uint, long, or ulong.
        // but we're not supporting unsigned integral types or shorts.
        if (datatype.startsWith("int") || datatype.startsWith("long") || datatype.startsWith("byte")) {
            return value;
        }

        return escapeText(value);
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

        String enumName = sanitizeName(name);

        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        enumName = camelize(enumName) + "Enum";

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return sanitizeName(camelize(property.name)) + "Enum";
    }

    public String testPackageName() {
        return this.packageName + ".Test";
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*").replace("--", "- -");
    }

    @Override
    public boolean isDataTypeString(String dataType) {
        // also treat double/decimal/float as "string" in enum so that the values (e.g. 2.8) get double-quoted
        return "String".equalsIgnoreCase(dataType) ||
                "double?".equals(dataType) || "decimal?".equals(dataType) || "float?".equals(dataType) ||
                "double".equals(dataType) || "decimal".equals(dataType) || "float".equals(dataType);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter) {

        // set the example value
        // if not specified in x-example, generate a default value
        // TODO need to revise how to obtain the example value
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            codegenParameter.example = Json.pretty(codegenParameter.vendorExtensions.get("x-example"));
        } else if (Boolean.TRUE.equals(codegenParameter.isBoolean)) {
            codegenParameter.example = "true";
        } else if (Boolean.TRUE.equals(codegenParameter.isLong)) {
            codegenParameter.example = "789";
        } else if (Boolean.TRUE.equals(codegenParameter.isInteger)) {
            codegenParameter.example = "56";
        } else if (Boolean.TRUE.equals(codegenParameter.isFloat)) {
            codegenParameter.example = "3.4F";
        } else if (Boolean.TRUE.equals(codegenParameter.isDouble)) {
            codegenParameter.example = "1.2D";
        } else if (Boolean.TRUE.equals(codegenParameter.isNumber)) {
            codegenParameter.example = "8.14";
        } else if (Boolean.TRUE.equals(codegenParameter.isBinary)) {
            codegenParameter.example = "BINARY_DATA_HERE";
        } else if (Boolean.TRUE.equals(codegenParameter.isByteArray)) {
            codegenParameter.example = "BYTE_ARRAY_DATA_HERE";
        } else if (Boolean.TRUE.equals(codegenParameter.isFile)) {
            codegenParameter.example = "/path/to/file.txt";
        } else if (Boolean.TRUE.equals(codegenParameter.isDate)) {
            codegenParameter.example = "2013-10-20";
        } else if (Boolean.TRUE.equals(codegenParameter.isDateTime)) {
            codegenParameter.example = "2013-10-20T19:20:30+01:00";
        } else if (Boolean.TRUE.equals(codegenParameter.isUuid)) {
            codegenParameter.example = "38400000-8cf0-11bd-b23e-10b96e4ef00d";
        } else if (Boolean.TRUE.equals(codegenParameter.isString)) {
            codegenParameter.example = codegenParameter.paramName + "_example";
        }
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        String fsharpPostProcessFile = System.getenv("FSHARP_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(fsharpPostProcessFile)) {
            return; // skip if FSHARP_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with .fs extension
        if ("fs".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = fsharpPostProcessFile + " " + file.toString();
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit code: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: " + command);
                }
            } catch (Exception e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
            }
        }
    }
}
