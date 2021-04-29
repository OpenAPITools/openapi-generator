package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.utils.ModelUtils;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TinyCppClientCodegen extends AbstractCppCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "SHOLOSOSOSO";

    static final Logger LOGGER = LoggerFactory.getLogger(TinyCppClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "tiny-cpp";
    }

    public String getHelp() {
        return "Generates a tiny-cpp client.";
    }

    public TinyCppClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "tiny-cpp";
        
        modelTemplateFiles.put("model.mustache", ".md");
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".cpp");
        apiTemplateFiles.put("api.mustache", ".md");
        embeddedTemplateDir = templateDir = "tiny-cpp-client";
        apiPackage = "Apis";
        modelPackage = "Models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("helpers-header.mustache", "", "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-body.mustache", "", "Helpers.cpp"));
        // TODO: Fill this out.

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                        "bool",
                        "int",
                        "long",
                        "double",
                        "float")
        );
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "bool",
                        "int",
                        "long",
                        "double",
                        "float",
                        "std::string")
        );

    

        super.typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::list");
        typeMapping.put("DateTime", "std::string");
        
    }


    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            return instantiationTypes.get("map");
        } else if (ModelUtils.isArraySchema(p)) {
            return instantiationTypes.get("array");
        } else {
            return null;
        }
    }

    @Override
    public String getTypeDeclaration(String name) {
        if (languageSpecificPrimitives.contains(name)) {
            return name;
        } else {
            return name + "";
        }
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);
        if (languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        } else {
            return openAPIType + "";
        }
    }

    @Override
    public String toModelName(String type) {
        if (typeMapping.keySet().contains(type) ||
                typeMapping.values().contains(type) ||
                defaultIncludes.contains(type) ||
                languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return Character.toUpperCase(type.charAt(0)) + type.substring(1);
        }
    }

    @Override
    public String toModelImport(String name) {
        if (name.equals("std::string")) {
            return "#include <string>";
        } else if (name.equals("std::map")) {
            return "#include <map>";
        } else if (name.equals("std::list")) {
            return "#include <list>";
        }
        return "#include \"" + name + ".h\"";
    }
    


}
