package io.swagger.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import io.swagger.codegen.*;
import io.swagger.codegen.mustache.*;
import io.swagger.codegen.utils.ModelUtils;
import io.swagger.models.properties.*;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public abstract class AbstractCSharpCodegen extends DefaultCodegen implements CodegenConfig {

    protected boolean optionalAssemblyInfoFlag = true;
    protected boolean optionalProjectFileFlag = true;
    protected boolean optionalEmitDefaultValue = false;
    protected boolean optionalMethodArgumentFlag = true;
    protected boolean useDateTimeOffsetFlag = false;
    protected boolean useCollection = false;
    protected boolean returnICollection = false;
    protected boolean netCoreProjectFileFlag = false;

    protected String modelPropertyNaming = CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.PascalCase.name();

    protected String packageVersion = "1.0.0";
    protected String packageName = "IO.Swagger";
    protected String packageTitle = "Swagger Library";
    protected String packageProductName = "SwaggerLibrary";
    protected String packageDescription = "A library generated from a Swagger doc";
    protected String packageCompany = "Swagger";
    protected String packageCopyright = "No Copyright";
    protected String packageAuthors = "Swagger";

    protected String interfacePrefix = "I";

    protected String sourceFolder = "src";

    // TODO: Add option for test folder output location. Nice to allow e.g. ./test instead of ./src.
    //       This would require updating relative paths (e.g. path to main project file in test project file)
    protected String testFolder = sourceFolder;

    protected Set<String> collectionTypes;
    protected Set<String> mapTypes;

    protected Logger LOGGER = LoggerFactory.getLogger(AbstractCSharpCodegen.class);

    public AbstractCSharpCodegen() {
        super();

        supportsInheritance = true;

        // C# does not use import mapping
        importMapping.clear();

        outputFolder = "generated-code" + File.separator + this.getName();
        embeddedTemplateDir = templateDir = this.getName();

        collectionTypes = new HashSet<String>(
                Arrays.asList(
                        "IList", "List",
                        "ICollection", "Collection",
                        "IEnumerable")
        );

        mapTypes = new HashSet<String>(
                Arrays.asList("IDictionary")
        );

        // NOTE: C# uses camel cased reserved words, while models are title cased. We don't want lowercase comparisons.
        reservedWords.addAll(
                Arrays.asList(
                        // set "client" as a reserved word to avoid conflicts with IO.Swagger.Client
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
                        "long", "namespace", "new", "null", "object", "operator", "out", "override", "params",
                        "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed",
                        "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw",
                        "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using",
                        "virtual", "void", "volatile", "while")
        );

        // TODO: Either include fully qualified names here or handle in DefaultCodegen via lastIndexOf(".") search
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "string",
                        "bool?",
                        "double?",
                        "decimal?",
                        "int?",
                        "long?",
                        "float?",
                        "byte[]",
                        "ICollection",
                        "Collection",
                        "List",
                        "Dictionary",
                        "DateTime?",
                        "DateTimeOffset?",
                        "String",
                        "Boolean",
                        "Double",
                        "Int32",
                        "Int64",
                        "Float",
                        "Guid?",
                        "System.IO.Stream", // not really a primitive, we include it to avoid model import
                        "Object")
        );

        instantiationTypes.put("array", "List");
        instantiationTypes.put("list", "List");
        instantiationTypes.put("map", "Dictionary");

        // Nullable types here assume C# 2 support is not part of base
        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "string");
        typeMapping.put("binary", "byte[]");
        typeMapping.put("bytearray", "byte[]");
        typeMapping.put("boolean", "bool?");
        typeMapping.put("integer", "int?");
        typeMapping.put("float", "float?");
        typeMapping.put("long", "long?");
        typeMapping.put("double", "double?");
        typeMapping.put("number", "decimal?");
        typeMapping.put("datetime", "DateTime?");
        typeMapping.put("date", "DateTime?");
        typeMapping.put("file", "System.IO.Stream");
        typeMapping.put("array", "List");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("object", "Object");
        typeMapping.put("uuid", "Guid?");
    }

    public void setReturnICollection(boolean returnICollection) {
        this.returnICollection = returnICollection;
    }

    public void setOptionalEmitDefaultValue(boolean optionalEmitDefaultValue) {
        this.optionalEmitDefaultValue = optionalEmitDefaultValue;
    }

    public void setUseCollection(boolean useCollection) {
        this.useCollection = useCollection;
        if (useCollection) {
            typeMapping.put("array", "Collection");
            typeMapping.put("list", "Collection");

            instantiationTypes.put("array", "Collection");
            instantiationTypes.put("list", "Collection");
        }
    }

    public void setOptionalMethodArgumentFlag(boolean flag) {
        this.optionalMethodArgumentFlag = flag;
    }

    public void setNetCoreProjectFileFlag(boolean flag) {
        this.netCoreProjectFileFlag = flag;
    }

    public void useDateTimeOffset(boolean flag) {
        this.useDateTimeOffsetFlag = flag;
        if (flag) typeMapping.put("datetime", "DateTimeOffset?");
        else typeMapping.put("datetime", "DateTime?");
    }

    @Override
    public void processOpts() {
        super.processOpts();

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
            LOGGER.warn(String.format("%s is not used by C# generators. Please use %s", CodegenConstants.INVOKER_PACKAGE, CodegenConstants.PACKAGE_NAME));
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

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES)) {
            setOptionalEmitDefaultValue(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES, optionalEmitDefaultValue);
        }

        if (additionalProperties.containsKey(CodegenConstants.NETCORE_PROJECT_FILE)) {
            setNetCoreProjectFileFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.NETCORE_PROJECT_FILE));
        } else {
            additionalProperties.put(CodegenConstants.NETCORE_PROJECT_FILE, netCoreProjectFileFlag);
        }

        if (additionalProperties.containsKey(CodegenConstants.INTERFACE_PREFIX)) {
            String useInterfacePrefix = additionalProperties.get(CodegenConstants.INTERFACE_PREFIX).toString();
            if("false".equals(useInterfacePrefix.toLowerCase())) {
                setInterfacePrefix("");
            } else if(!"true".equals(useInterfacePrefix.toLowerCase())) {
                // NOTE: if user passes "true" explicitly, we use the default I- prefix. The other supported case here is a custom prefix.
                setInterfacePrefix(sanitizeName(useInterfacePrefix));
            }
        }

        // This either updates additionalProperties with the above fixes, or sets the default if the option was not specified.
        additionalProperties.put(CodegenConstants.INTERFACE_PREFIX, interfacePrefix);

        addMustacheLambdas(additionalProperties);
    }

    private void addMustacheLambdas(Map<String, Object> objs) {

        Map<String, Mustache.Lambda> lambdas = new ImmutableMap.Builder<String, Mustache.Lambda>()
                .put("lowercase", new LowercaseLambda().generator(this))
                .put("uppercase", new UppercaseLambda())
                .put("titlecase", new TitlecaseLambda())
                .put("camelcase", new CamelCaseLambda().generator(this))
                .put("camelcase_param", new CamelCaseLambda().generator(this).escapeAsParamName(true))
                .put("indented", new IndentedLambda())
                .put("indented_8", new IndentedLambda(8, " "))
                .put("indented_12", new IndentedLambda(12, " "))
                .put("indented_16", new IndentedLambda(16, " "))
                .build();

        if (objs.containsKey("lambda")) {
            LOGGER.warn("An property named 'lambda' already exists. Mustache lambdas renamed from 'lambda' to '_lambda'. " +
                    "You'll likely need to use a custom template, " +
                    "see https://github.com/swagger-api/swagger-codegen#modifying-the-client-library-format. ");
            objs.put("_lambda", lambdas);
        } else {
            objs.put("lambda", lambdas);
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
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
     * @return An in-place modified state of the codegen object model.
     */
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        final Map<String, Object> processed =  super.postProcessAllModels(objs);
        postProcessEnumRefs(processed);
        return processed;
    }

    /**
     * C# differs from other languages in that Enums are not _true_ objects; enums are compiled to integral types.
     * So, in C#, an enum is considers more like a user-defined primitive.
     *
     * When working with enums, we can't always assume a RefModel is a nullable type (where default(YourType) == null),
     * so this post processing runs through all models to find RefModel'd enums. Then, it runs through all vars and modifies
     * those vars referencing RefModel'd enums to work the same as inlined enums rather than as objects.
     * @param models processed models to be further processed for enum references
     */
    @SuppressWarnings({ "unchecked" })
    private void postProcessEnumRefs(final Map<String, Object> models) {
        Map<String, CodegenModel> enumRefs = new HashMap<String, CodegenModel>();
        for (Map.Entry<String, Object> entry : models.entrySet()) {
            CodegenModel model = ModelUtils.getModelByName(entry.getKey(), models);
            if (model.isEnum) {
                enumRefs.put(entry.getKey(), model);
            }
        }

        for (Map.Entry<String, Object> entry : models.entrySet()) {
            String swaggerName = entry.getKey();
            CodegenModel model = ModelUtils.getModelByName(swaggerName, models);
            if (model != null) {
                for (CodegenProperty var : model.allVars) {
                    if (enumRefs.containsKey(var.datatype)) {
                        // Handle any enum properties referred to by $ref.
                        // This is different in C# than most other generators, because enums in C# are compiled to integral types,
                        // while enums in many other languages are true objects.
                        CodegenModel refModel = enumRefs.get(var.datatype);
                        var.allowableValues = refModel.allowableValues;
                        var.isEnum = true;

                        updateCodegenPropertyEnum(var);

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
                        // C# Actually supports byte and short enums, swagger spec only supports byte.
                        isByte = true;
                        model.vendorExtensions.put("x-enum-byte", true);
                    } else if (model.dataType.startsWith("int32")) {
                        isInteger = true;
                        model.vendorExtensions.put("x-enum-integer", true);
                    } else if (model.dataType.startsWith("int64")) {
                        isLong = true;
                        model.vendorExtensions.put("x-enum-long", true);
                    } else {
                        // C# doesn't support non-integral enums, so we need to treat everything else as strings (e.g. to not lose precision or data integrity)
                        isString = true;
                        model.vendorExtensions.put("x-enum-string", true);
                    }

                    // Since we iterate enumVars for modelnnerEnum and enumClass templates, and CodegenModel is missing some of CodegenProperty's properties,
                    // we can take advantage of Mustache's contextual lookup to add the same "properties" to the model's enumVars scope rather than CodegenProperty's scope.
                    List<Map<String, String>> enumVars = (ArrayList<Map<String, String>>)model.allowableValues.get("enumVars");
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
                LOGGER.warn("Expected to retrieve model %s by name, but no model was found. Check your -Dmodels inclusions.", swaggerName);
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
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        super.postProcessOperations(objs);
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

                    if (operation.examples != null){
                        for (Map<String, String> example : operation.examples)
                        {
                            for (Map.Entry<String, String> entry : example.entrySet())
                            {
                                // Replace " with \", \r, \n with \\r, \\n
                                String val = entry.getValue().replace("\"", "\\\"")
                                    .replace("\r","\\r")
                                    .replace("\n","\\n");
                                entry.setValue(val);
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
        return outputFolder + File.separator + sourceFolder + File.separator + packageName + File.separator + apiPackage();
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + packageName + File.separator + modelPackage();
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
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    /**
     * Return the example value of the property
     *
     * @param p Swagger property object
     * @return string presentation of the example value of the property
     */
    @Override
    public String toExampleValue(Property p) {
        if (p instanceof StringProperty) {
            StringProperty dp = (StringProperty) p;
            if (dp.getExample() != null) {
                return "\"" + dp.getExample().toString() + "\"";
            }
        } else if (p instanceof BooleanProperty) {
            BooleanProperty dp = (BooleanProperty) p;
            if (dp.getExample() != null) {
                return dp.getExample().toString();
            }
        } else if (p instanceof DateProperty) {
            // TODO
        } else if (p instanceof DateTimeProperty) {
            // TODO
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getExample() != null) {
                return dp.getExample().toString();
            }
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getExample() != null) {
                return dp.getExample().toString();
            }
        } else if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getExample() != null) {
                return dp.getExample().toString();
            }
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getExample() != null) {
                return dp.getExample().toString();
            }
        }

        return null;
    }

    /**
     * Return the default value of the property
     *
     * @param p Swagger property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            StringProperty dp = (StringProperty) p;
            if (dp.getDefault() != null) {
               String _default = dp.getDefault();
               if (dp.getEnum() == null) {
                   return "\"" + _default + "\"";
               } else {
                   // convert to enum var name later in postProcessModels
                   return _default;
               }
            }
        } else if (p instanceof BooleanProperty) {
            BooleanProperty dp = (BooleanProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof DateProperty) {
            // TODO
        } else if (p instanceof DateTimeProperty) {
            // TODO
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getDefault() != null) {
                return String.format("%1$sF", dp.getDefault());
            }
        } else if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        }

        return null;
    }

    @Override
    protected boolean isReservedWord(String word) {
        // NOTE: This differs from super's implementation in that C# does _not_ want case insensitive matching.
        return reservedWords.contains(word);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type;

        if (swaggerType == null) {
            swaggerType = ""; // set swagger type to empty string if null
        }

        // NOTE: typeMapping here supports things like string/String, long/Long, datetime/DateTime as lowercase keys.
        //       Should we require explicit casing here (values are not insensitive).
        // TODO avoid using toLowerCase as typeMapping should be case-sensitive
        if (typeMapping.containsKey(swaggerType.toLowerCase())) {
            type = typeMapping.get(swaggerType.toLowerCase());
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    /**
     * Provides C# strongly typed declaration for simple arrays of some type and arrays of arrays of some type.
     * @param arr The input array property
     * @return The type declaration when the type is an array of arrays.
     */
    private String getArrayTypeDeclaration(ArrayProperty arr) {
        // TODO: collection type here should be fully qualified namespace to avoid model conflicts
        // This supports arrays of arrays.
        String arrayType = typeMapping.get("array");
        StringBuilder instantiationType = new StringBuilder(arrayType);
        Property items = arr.getItems();
        String nestedType = getTypeDeclaration(items);
        // TODO: We may want to differentiate here between generics and primitive arrays.
        instantiationType.append("<").append(nestedType).append(">");
        return instantiationType.toString();
    }

    @Override
    public String toInstantiationType(Property p) {
        if (p instanceof ArrayProperty) {
            return getArrayTypeDeclaration((ArrayProperty) p);
        }
        return super.toInstantiationType(p);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            return getArrayTypeDeclaration((ArrayProperty) p);
        } else if (p instanceof MapProperty) {
            // Should we also support maps of maps?
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "<string, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String toModelName(String name) {
        // We need to check if import-mapping has a different model for this class, so we use it
        // instead of the auto-generated one.
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        }
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
        return outputFolder + ".Test";
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + ".Test";
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiName(name) + "Tests";
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelName(name) + "Tests";
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
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

    @Override
    public String toEnumValue(String value, String datatype) {
        // C# only supports enums as literals for int, int?, long, long?, byte, and byte?. All else must be treated as strings.
        // Per: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/enum
        // The approved types for an enum are byte, sbyte, short, ushort, int, uint, long, or ulong.
        // but we're not supporting unsigned integral types or shorts.
        if(datatype.startsWith("int") || datatype.startsWith("long") || datatype.startsWith("byte")) {
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
}
