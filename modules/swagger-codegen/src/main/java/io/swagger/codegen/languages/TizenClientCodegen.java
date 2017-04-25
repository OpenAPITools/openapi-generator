package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DecimalProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

public class TizenClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected static String PREFIX = "Sami";
    protected Set<String> foundationClasses = new HashSet<String>();
    protected String sourceFolder = "client";
    protected Map<String, String> namespaces = new HashMap<String, String>();

    public TizenClientCodegen() {
        super();
        outputFolder = "generated-code/tizen";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".cpp");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-body.mustache", ".cpp");
        embeddedTemplateDir = templateDir = "tizen";
        modelPackage = "";

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                        "bool",
                        "int",
                        "long")
        );
        languageSpecificPrimitives = new HashSet<String>();

        additionalProperties().put("prefix", PREFIX);

        setReservedWordsLowerCase(
                // VERIFY
                Arrays.asList(
                        "void", "char", "short", "int", "void", "char", "short", "int",
                        "long", "float", "double", "signed", "unsigned", "id", "const",
                        "volatile", "in", "out", "inout", "bycopy", "byref", "oneway",
                        "self", "super"
                ));

        super.typeMapping = new HashMap<String, String>();

        typeMapping.put("Date", "DateTime");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("string", "String");
        typeMapping.put("integer", "Integer");
        typeMapping.put("float", "Float");
        typeMapping.put("long", "Long");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("double", "Double");
        typeMapping.put("array", "IList");
        typeMapping.put("map", "HashMap");
        typeMapping.put("number", "Long");
        typeMapping.put("object", PREFIX + "Object");
        typeMapping.put("UUID", "String");
        //TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "String");

        importMapping = new HashMap<String, String>();

        namespaces = new HashMap<String, String>();
        namespaces.put("DateTime", "Tizen::Base::DateTime");
        namespaces.put("Integer", "Tizen::Base::Integer");
        namespaces.put("Long", "Tizen::Base::Long");
        namespaces.put("Boolean", "Tizen::Base::Boolean");
        namespaces.put("Float", "Tizen::Base::Float");
        namespaces.put("String", "Tizen::Base::String");
        namespaces.put("Double", "Tizen::Base::Double");
        namespaces.put("IList", "Tizen::Base::Collection::IList");
        namespaces.put("HashMap", "Tizen::Base::Collection::HashMap");
        namespaces.put("ArrayList", "Tizen::Base::Collection::ArrayList");
        namespaces.put("JsonNumber", "Tizen::Web::Json");
        namespaces.put("JsonString", "Tizen::Web::Json");

        foundationClasses = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "Integer",
                        "Float")
        );
        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("modelFactory.mustache", sourceFolder, PREFIX + "ModelFactory.h"));
        supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder, PREFIX + "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder, PREFIX + "Helpers.cpp"));
        supportingFiles.add(new SupportingFile("apiclient-header.mustache", sourceFolder, PREFIX + "ApiClient.h"));
        supportingFiles.add(new SupportingFile("apiclient-body.mustache", sourceFolder, PREFIX + "ApiClient.cpp"));
        supportingFiles.add(new SupportingFile("object.mustache", sourceFolder, PREFIX + "Object.h"));
        supportingFiles.add(new SupportingFile("error-header.mustache", sourceFolder, PREFIX + "Error.h"));
        supportingFiles.add(new SupportingFile("error-body.mustache", sourceFolder, PREFIX + "Error.cpp"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "tizen";
    }

    @Override
    public String getHelp() {
        return "Generates a Samsung Tizen C++ client library.";
    }

    @Override
    public String toInstantiationType(Property p) {
        if (p instanceof MapProperty) {
            return instantiationTypes.get("map");
        } else if (p instanceof ArrayProperty) {
            return instantiationTypes.get("array");
        } else {
            return null;
        }
    }

    @Override
    public String getTypeDeclaration(String name) {
        if (languageSpecificPrimitives.contains(name) && !foundationClasses.contains(name)) {
            return name;
        } else {
            return name + "*";
        }
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type) && !foundationClasses.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        String swaggerType = getSwaggerType(p);
        if (languageSpecificPrimitives.contains(swaggerType) && !foundationClasses.contains(swaggerType)) {
            return toModelName(swaggerType);
        } else {
            return swaggerType + "*";
        }
    }

    @Override
    public String toModelName(String type) {
        if (typeMapping.keySet().contains(type) ||
                typeMapping.values().contains(type) ||
                foundationClasses.contains(type) ||
                importMapping.values().contains(type) ||
                defaultIncludes.contains(type) ||
                languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return PREFIX + Character.toUpperCase(type.charAt(0)) + type.substring(1);
        }
    }

    @Override
    public String toModelImport(String name) {
        if (namespaces.containsKey(name)) {
            return "using " + namespaces.get(name) + ";";
        }
        return "#include \"" + name + ".h\"";
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            return "new String()";
        } else if (p instanceof BooleanProperty) {
            return "new Boolean(false)";
        } else if (p instanceof DateProperty) {
            return "new DateTime()";
        } else if (p instanceof DateTimeProperty) {
            return "new DateTime()";
        } else if (p instanceof DoubleProperty) {
            return "new Double()";
        } else if (p instanceof FloatProperty) {
            return "new Float()";
        } else if (p instanceof IntegerProperty) {
            return "new Integer()";
        } else if (p instanceof LongProperty) {
            return "new Long()";
        } else if (p instanceof DecimalProperty) {
            return "new Long()";
        } else if (p instanceof MapProperty) {
            return "new HashMap()";
        } else if (p instanceof ArrayProperty) {
            return "new ArrayList()";
        }
        // else
        if (p instanceof RefProperty) {
            RefProperty rp = (RefProperty) p;
            return "new " + toModelName(rp.getSimpleRef()) + "()";
        }
        return "null";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder;
    }

    @Override
    public String toModelFilename(String name) {
        return PREFIX + initialCaps(name);
    }

    @Override
    public String toApiName(String name) {
        return PREFIX + initialCaps(name) + "Api";
    }

    @Override
    public String toApiFilename(String name) {
        return PREFIX + initialCaps(name) + "Api";
    }

    @Override
    public String toVarName(String name) {
        String paramName = name.replaceAll("[^a-zA-Z0-9_]", "");
        paramName = Character.toUpperCase(paramName.charAt(0)) + paramName.substring(1);
        return "p" + paramName;
    }

    @Override
    public String escapeReservedWord(String name) {           
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return$
        if (isReservedWord(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        // add_pet_by_id => addPetById
        return camelize(operationId, true);
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
}
