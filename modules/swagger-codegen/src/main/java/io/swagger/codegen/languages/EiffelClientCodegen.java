package io.swagger.codegen.languages;

import static com.google.common.base.Strings.isNullOrEmpty;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.utils.ModelUtils;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

public class EiffelClientCodegen extends DefaultCodegen implements CodegenConfig {
    static Logger LOGGER = LoggerFactory.getLogger(EiffelClientCodegen.class);

    protected String libraryTarget = "swagger_eiffel_client";
    protected String packageName = "Eiffel";
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs";
    protected String modelDocPath = "docs";
    protected String modelPath = "domain";

    protected UUID uuid;
    protected UUID uuidTest;

    private final Set<String> parentModels = new HashSet<>();
    private final Multimap<String, CodegenModel> childrenByParent = ArrayListMultimap.create();

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "eiffel";
    }

    @Override
    public String getHelp() {
        return "Generates a Eiffel client library (beta).";
    }

    public EiffelClientCodegen() {
        super();
        uuid = UUID.randomUUID();
        uuidTest = UUID.randomUUID();
        ;
        outputFolder = "generated-code/Eiffel";
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        modelTemplateFiles.put("model.mustache", ".e");
        apiTemplateFiles.put("api.mustache", ".e");
        apiTestTemplateFiles.put("test/api_test.mustache", ".e");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "Eiffel";

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

        //instantiationTypes.put("array", "ARRAY");
        //instantiationTypes.put("list", "ARRAYED_LIST");
        //instantiationTypes.put("map", "STRING_TABLE");


        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Eiffel Cluster name (convention: lowercase).")
                .defaultValue("swagger"));
        cliOptions
                .add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Eiffel package version.").defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                "hides the timestamp when files were generated").defaultValue(Boolean.TRUE.toString()));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        // default HIDE_GENERATION_TIMESTAMP to true
        if (!additionalProperties.containsKey(CodegenConstants.HIDE_GENERATION_TIMESTAMP)) {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, Boolean.TRUE.toString());
        } else {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                    Boolean.valueOf(additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("swagger");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("uuid", uuid.toString());
        additionalProperties.put("uuidTest", uuidTest.toString());
        additionalProperties.put("libraryTarget", libraryTarget);
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        modelPackage = packageName;
        apiPackage = packageName;

        final String authFolder = ("src/framework/auth");
        final String serializerFolder = ("src/framework/serialization");
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("ecf.mustache", "", "api_client.ecf"));
        supportingFiles.add(new SupportingFile("test/ecf_test.mustache", "test", "api_test.ecf"));
        supportingFiles.add(new SupportingFile("test/application.mustache", "test", "application.e"));
        supportingFiles.add(new SupportingFile("api_client.mustache", "src", "api_client.e"));
        supportingFiles.add(new SupportingFile("framework/api_i.mustache", "src/framework", "api_i.e"));
        supportingFiles.add(
                new SupportingFile("framework/api_client_request.mustache", "src/framework", "api_client_request.e"));
        supportingFiles.add(
                new SupportingFile("framework/api_client_response.mustache", "src/framework", "api_client_response.e"));
        supportingFiles.add(new SupportingFile("framework/api_error.mustache", "src/framework", "api_error.e"));
        supportingFiles.add(new SupportingFile("framework/configuration.mustache", "src/framework", "configuration.e"));
        supportingFiles
                .add(new SupportingFile("framework/auth/authentication.mustache", authFolder, "authentication.e"));
        supportingFiles.add(new SupportingFile("framework/auth/api_key_auth.mustache", authFolder, "api_key_auth.e"));
        supportingFiles
                .add(new SupportingFile("framework/auth/http_basic_auth.mustache", authFolder, "http_basic_auth.e"));
        supportingFiles.add(new SupportingFile("framework/auth/oauth.mustache", authFolder, "oauth.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/api_deserializer.mustache", serializerFolder,
                "api_deserializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/api_json_deserializer.mustache",
                serializerFolder, "api_json_deserializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/api_json_serializer.mustache", serializerFolder,
                "api_json_serializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/api_serializer.mustache", serializerFolder,
                "api_serializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/json_basic_reflector_deserializer.mustache",
                serializerFolder, "json_basic_reflector_deserializer.e"));
        supportingFiles.add(new SupportingFile("framework/serialization/json_type_utilities_ext.mustache",
                serializerFolder, "json_type_utilities_ext.e"));

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
        return camelize(name) + '_';
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + "src" + File.separator + "api";
    }

    public String modelFileFolder() {
        return outputFolder + File.separator + "src" + File.separator + modelPath;
    }

    public String apiTestFileFolder() {
        return outputFolder + File.separator + "test" + File.separator + "apis";
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

        // for reserved word or word starting with number, append _
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
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
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
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
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "LIST [" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
        }
        // return super.getTypeDeclaration(p);

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String swaggerType = getSwaggerType(p);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }

        if (typeMapping.containsValue(swaggerType)) {
            return swaggerType;
        }

        if (languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }

        return toModelName(swaggerType);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
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
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        final String prefix = modelPackage();
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(prefix))
                iterator.remove();
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
        for (final CodegenProperty parentProperty : parent.vars) {
            final CodegenProperty duplicatedByParent = childPropertiesByName.get(parentProperty.name);
            if (duplicatedByParent != null) {
                duplicatedByParent.isInherited = true;
            }
        }
    }

    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type) && !languageSpecificPrimitives.contains(type);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
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
}
