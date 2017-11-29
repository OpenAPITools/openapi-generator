package io.swagger.codegen.languages;

import static com.google.common.base.Strings.isNullOrEmpty;
import static io.swagger.codegen.CodegenConstants.HAS_ENUMS_EXT_NAME;
import static io.swagger.codegen.CodegenConstants.IS_ENUM_EXT_NAME;
import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.utils.ModelUtils;
import io.swagger.util.Json;

public abstract class AbstractEiffelCodegen extends DefaultCodegen implements CodegenConfig {

    private final Set<String> parentModels = new HashSet<>();
    private final Multimap<String, CodegenModel> childrenByParent = ArrayListMultimap.create();
        
    public AbstractEiffelCodegen(){
        super();
        setReservedWordsLowerCase(Arrays.asList(
                // language reserved words
                "across", "agent", "alias", "all", "and", "as", "assign", "attribute", "check", "class", "convert",
                "create", "Current", "debug", "deferred", "do", "else", "elseif", "end", "ensure", "expanded", "export",
                "external", "False", "feature", "from", "frozen", "if", "implies", "inherit", "inspect", "invariant",
                "like", "local", "loop", "not", "note", "obsolete", "old", "once", "only", "or", "Precursor",
                "redefine", "rename", "require", "rescue", "Result", "retry", "select", "separate", "then", "True",
                "TUPLE", "undefine", "until", "variant", "Void", "when", "xor"));

        defaultIncludes = new HashSet<String>(Arrays.asList("map", "array"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("BOOLEAN", "INTEGER_8", "INTEGER_16", "INTEGER_32", "INTEGER_64", "NATURAL_8",
                        "NATURAL_16", "NATURAL_32", "NATURAL_64", "REAL_32", "REAL_64"));

        instantiationTypes.clear();
      
        typeMapping.clear();
        typeMapping.put("integer", "INTEGER_32");
        typeMapping.put("long", "INTEGER_64");
        typeMapping.put("number", "REAL_32");
        typeMapping.put("float", "REAL_32");
        typeMapping.put("double", "REAL_64");
        typeMapping.put("boolean", "BOOLEAN");
        typeMapping.put("string", "STRING_32");
        typeMapping.put("UUID", "UUID"); //
        typeMapping.put("date", "DATE");
        typeMapping.put("DateTime", "DATE_TIME");
        typeMapping.put("date-time", "DATE_TIME");
        typeMapping.put("password", "STRING");
        typeMapping.put("File", "FILE");
        typeMapping.put("file", "FILE");
        typeMapping.put("binary", "STRING_32");
        typeMapping.put("ByteArray", "ARRAY [NATURAL_8]"); 
        typeMapping.put("object", "ANY");
        typeMapping.put("map", "STRING_TABLE");
        typeMapping.put("array", "LIST");
        typeMapping.put("list", "LIST");

        instantiationTypes.put("array", "ARRAYED_LIST");
        instantiationTypes.put("list", "ARRAYED_LIST");
        instantiationTypes.put("map", "STRING_TABLE");


        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Eiffel Cluster name (convention: lowercase).")
                .defaultValue("swagger"));
        cliOptions
                .add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Eiffel package version.").defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                "hides the timestamp when files were generated").defaultValue(Boolean.TRUE.toString()));
    }
    
    @Override
    public String escapeReservedWord(String name) {
        // Can't start with an underscore, as our fields need to start with an
        // UppercaseLetter so that Go treats them as public/visible.

        // Options?
        // - MyName
        // - AName
        // - TheName
        // - XName
        // - X_Name
        // ... or maybe a suffix?
        // - Name_ ... think this will work.
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        if (name.matches("^\\d.*")) {// prepend var_
            return "var_" + name;
        }    
        return "var_" + name;
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // pet_id
        // petId  => pet_id
        name = unCamelize(name);
        
        if (name.startsWith("_")){
            name = "var" + name;
        }

        // for reserved word
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // for reserved word or word starting with number, append 
        if (name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // params should be lowercase. E.g. "person: PERSON"
        return toVarName(name).toLowerCase();
    }

    @Override
    public String toModelName(String name) {
        // phone_number => PHONE_NUMBER
        return toModelFilename(name).toUpperCase();
    }

    @Override
    public String toModelFilename(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + ("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after
                                    // camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to "
                    + ("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response
                                    // (after camelize)
        }

        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be
                                          // assigned. Also declare the
                                          // methods parameters as 'final'.

        // e.g. PetApi.go => pet_api.go
        return underscore(name) + "_api";
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiName(name).toLowerCase() + "_test";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DEFAULT_API";
        }
        return name.toUpperCase() + "_API";
    }

    /**
     * Overrides postProcessParameter to add a vendor extension
     * "x-exportParamName". This is useful when paramName starts with a
     * lowercase letter, but we need that param to be exportable (starts with an
     * Uppercase letter).
     *
     * @param parameter
     *            CodegenParameter object to be processed.
     */
    @Override
    public void postProcessParameter(CodegenParameter parameter) {

        // Give the base class a chance to process
        super.postProcessParameter(parameter);

        char firstChar = parameter.paramName.charAt(0);

        if (Character.isUpperCase(firstChar)) {
            // First char is already uppercase, just use paramName.
            parameter.vendorExtensions.put("x-exportParamName", parameter.paramName);

        }

        // It's a lowercase first char, let's convert it to uppercase
        StringBuilder sb = new StringBuilder(parameter.paramName);
        sb.setCharAt(0, Character.toUpperCase(firstChar));
        parameter.vendorExtensions.put("x-exportParamName", sb.toString());
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if (!isNullOrEmpty(model.parent)) {
            parentModels.add(model.parent);
            if (!childrenByParent.containsEntry(model.parent, model)) {
                childrenByParent.put(model.parent, model);
            }
        }
        if (!isNullOrEmpty(model.parentSchema)) {
            model.parentSchema = model.parentSchema.toLowerCase();
        }
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String getTypeDeclaration(Schema schema) {
        if (schema instanceof ArraySchema) {
            Schema inner = ((ArraySchema) schema).getItems();
            return String.format("LIST [%s]", getTypeDeclaration(inner));

        } else if (schema instanceof MapSchema) {
            Schema inner = schema.getAdditionalProperties();
            return String.format("%s[%s]", getSchemaType(schema), getTypeDeclaration(inner));
        }
        // return super.getTypeDeclaration(p);

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String schemaType = getSchemaType(schema);
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }

        if (typeMapping.containsValue(schemaType)) {
            return schemaType;
        }

        if (languageSpecificPrimitives.contains(schemaType)) {
            return schemaType;
        }

        return toModelName(schemaType);
    }

    @Override
    public String getSchemaType(Schema propertySchema) {
        String swaggerType = super.getSchemaType(propertySchema);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type))
                return (type);
        } else
            type = swaggerType;
        return type;
    }

    @Override
    public String toOperationId(String operationId) {
        String sanitizedOperationId = sanitizeName(operationId);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(sanitizedOperationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to "
                    + camelize("call_" + operationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }
        // method name from updateSomething to update_Something.
        sanitizedOperationId = unCamelize(sanitizedOperationId);

        return toEiffelFeatureStyle(sanitizedOperationId);
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            // http method verb conversion (e.g. PUT => Put)

            operation.httpMethod = camelize(operation.httpMethod.toLowerCase());
        }

        // remove model imports to avoid error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        if (imports == null)
            return objs;

        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(apiPackage()))
                iterator.remove();
        }
        // if the return type is not primitive, import encoding/json
        for (CodegenOperation operation : operations) {
            if (operation.returnBaseType != null && needToImport(operation.returnBaseType)) {
                imports.add(createMapping("import", "encoding/json"));
                break; // just need to import once
            }
        }

        // this will only import "fmt" if there are items in pathParams
        for (CodegenOperation operation : operations) {
            if (operation.pathParams != null && operation.pathParams.size() > 0) {
                imports.add(createMapping("import", "fmt"));
                break; // just need to import once
            }
        }

        // recursively add import for mapping one type to multiple imports
        List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
        if (recursiveImports == null)
            return objs;

        ListIterator<Map<String, String>> listIterator = imports.listIterator();
        while (listIterator.hasNext()) {
            String _import = listIterator.next().get("import");
            // if the import package happens to be found in the importMapping
            // (key)
            // add the corresponding import package to the list
            if (importMapping.containsKey(_import)) {
                listIterator.add(createMapping("import", importMapping.get(_import)));
            }
        }

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // remove model imports to avoid error
//        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
//        final String prefix = modelPackage();
//        Iterator<Map<String, String>> iterator = imports.iterator();
//        while (iterator.hasNext()) {
//            String _import = iterator.next().get("import");
//            if (_import.startsWith(prefix))
//                iterator.remove();
//        }
//
//        // recursively add import for mapping one type to multiple imports
//        List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
//        if (recursiveImports == null)
//            return objs;
//
//        ListIterator<Map<String, String>> listIterator = imports.listIterator();
//        while (listIterator.hasNext()) {
//            String _import = listIterator.next().get("import");
//            // if the import package happens to be found in the importMapping
//            // (key)
//            // add the corresponding import package to the list
//            if (importMapping.containsKey(_import)) {
//                listIterator.add(createMapping("import", importMapping.get(_import)));
//            }
//        }
//
//        return objs;
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessAllModels(final Map<String, Object> models) {

        final Map<String, Object> processed = super.postProcessAllModels(models);
        postProcessParentModels(models);
        return processed;
    }

    private void postProcessParentModels(final Map<String, Object> models) {
        for (final String parent : parentModels) {
            final CodegenModel parentModel = ModelUtils.getModelByName(parent, models);
            final Collection<CodegenModel> childrenModels = childrenByParent.get(parent);
            for (final CodegenModel child : childrenModels) {
                processParentPropertiesInChildModel(parentModel, child);
            }
        }
    }

    /**
     * Sets the child property's isInherited flag to true if it is an inherited
     * property
     */
    private void processParentPropertiesInChildModel(final CodegenModel parent, final CodegenModel child) {
        final Map<String, CodegenProperty> childPropertiesByName = new HashMap<>(child.vars.size());
        for (final CodegenProperty childProperty : child.vars) {
            childPropertiesByName.put(childProperty.name, childProperty);
        }
        if (parent != null) {
            for (final CodegenProperty parentProperty : parent.vars) {
                final CodegenProperty duplicatedByParent = childPropertiesByName.get(parentProperty.name);
                if (duplicatedByParent != null) {
                    duplicatedByParent.isInherited = true;
                }
            }
        }
    }
    
    @Override
    public CodegenModel fromModel(String name, Schema schema, Map<String, Schema> allSchemas) {
        CodegenModel codegenModel = super.fromModel(name, schema, allSchemas);
        boolean hasEnums = getBooleanValue(codegenModel.getVendorExtensions(), HAS_ENUMS_EXT_NAME);
        if (allSchemas != null && codegenModel.parentSchema != null && hasEnums) {
            final Schema parentModel = allSchemas.get(codegenModel.parentSchema);
            final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent, parentModel);
            codegenModel = AbstractEiffelCodegen.reconcileInlineEnums(codegenModel, parentCodegenModel);
        }
        return codegenModel;
    }
    
    private static CodegenModel reconcileInlineEnums(CodegenModel codegenModel, CodegenModel parentCodegenModel) {
        // This generator uses inline classes to define enums, which breaks when
        // dealing with models that have subTypes. To clean this up, we will analyze
        // the parent and child models, look for enums that match, and remove
        // them from the child models and leave them in the parent.
        // Because the child models extend the parents, the enums will be available via the parent.

        // Only bother with reconciliation if the parent model has enums.
        boolean hasEnums = getBooleanValue(parentCodegenModel.getVendorExtensions(), HAS_ENUMS_EXT_NAME);
        if  (!hasEnums) {
            return codegenModel;
        }

        // Get the properties for the parent and child models
        final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
        List<CodegenProperty> codegenProperties = codegenModel.vars;

        // Iterate over all of the parent model properties
        boolean removedChildEnum = false;
        for (CodegenProperty parentModelCodegenPropery : parentModelCodegenProperties) {
            // Look for enums
            boolean isEnum = getBooleanValue(parentModelCodegenPropery.getVendorExtensions(), IS_ENUM_EXT_NAME);
            if (isEnum) {
                // Now that we have found an enum in the parent class,
                // and search the child class for the same enum.
                Iterator<CodegenProperty> iterator = codegenProperties.iterator();
                while (iterator.hasNext()) {
                    CodegenProperty codegenProperty = iterator.next();
                    isEnum = getBooleanValue(codegenProperty.getVendorExtensions(), IS_ENUM_EXT_NAME);
                    if (isEnum && codegenProperty.equals(parentModelCodegenPropery)) {
                        // We found an enum in the child class that is
                        // a duplicate of the one in the parent, so remove it.
                        iterator.remove();
                        removedChildEnum = true;
                    }
                }
            }
        }

        if(removedChildEnum) {
            // If we removed an entry from this model's vars, we need to ensure hasMore is updated
            int count = 0, numVars = codegenProperties.size();
            for(CodegenProperty codegenProperty : codegenProperties) {
                count += 1;
                codegenProperty.hasMore = (count < numVars) ? true : false;
            }
            codegenModel.vars = codegenProperties;
        }
        return codegenModel;
    }

    
    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type) && !languageSpecificPrimitives.contains(type);
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

    public Map<String, String> createMapping(String key, String value) {
        Map<String, String> customImport = new HashMap<String, String>();
        customImport.put(key, value);

        return customImport;
    }
    
    @Override
    public String toInstantiationType(Schema schema) {
        if (schema instanceof MapSchema) {
            Schema additionalProperties2 = schema.getAdditionalProperties();
            String type = additionalProperties2.getType();
            if (null == type) {
                LOGGER.error("No Type defined for Additional Property " + additionalProperties2 + "\n" //
                      + "\tIn Property: " + schema);
            }
            String inner = toModelName(getSchemaType(additionalProperties2));
            return String.format("%s [%s]", instantiationTypes.get("map"), inner);
        } else if (schema instanceof ArraySchema) {
            ArraySchema arraySchema = (ArraySchema) schema;
            String inner = toModelName(getSchemaType(arraySchema.getItems()));
            //return instantiationTypes.get("array") + " [" + inner + "]";
            return String.format("%s [%s]", instantiationTypes.get("array"), inner);
        } else {
            return null;
        }
    }

    public String unCamelize(String name) {
        return name.replaceAll("(.)(\\p{Upper})", "$1_$2").toLowerCase();
    }

    public String toEiffelFeatureStyle(String operationId) {
        if (operationId.startsWith("get_")) {
            return operationId.substring(4, operationId.length());
        } else {
            return operationId;
        }
    }

    /**
     * Update property for array(list) container
     * @param property Codegen property
     * @param innerProperty Codegen inner property of map or list
     */
    @Override
    protected void updatePropertyForArray(CodegenProperty property, CodegenProperty innerProperty) {
        if (innerProperty == null) {
            LOGGER.warn("skipping invalid array property " + Json.pretty(property));
            return;
        }
        property.dataFormat = innerProperty.dataFormat;
        if (!languageSpecificPrimitives.contains(innerProperty.baseType)) {
            property.complexType = innerProperty.baseType;
        } else {
            property.isPrimitiveType = true;
        }
        property.items = innerProperty;
        // inner item is Enum
        if (isPropertyInnerMostEnum(property)) {  
            // We use the data type instead of the Enum class.
            // at the moment is not supported.
            
            // isEnum is set to true when the type is an enum
            // or the inner type of an array/map is an enum
            //property.isEnum = true;
            // update datatypeWithEnum and default value for array
            // e.g. List<string> => List<StatusEnum>
            //updateDataTypeWithEnumForArray(property);
            // set allowable values to enum values (including array/map of enum)
            //property.allowableValues = getInnerEnumAllowableValues(property);
        }

    } 
 
}
