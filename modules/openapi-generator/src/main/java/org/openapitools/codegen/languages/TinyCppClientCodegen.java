package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.utils.ModelUtils;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TinyCppClientCodegen extends AbstractCppCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "TinyClient";

    static final Logger LOGGER = LoggerFactory.getLogger(TinyCppClientCodegen.class);

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator. This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "tiny-cpp";
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with
     * help tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a Arduino rest client.";
    }

    public TinyCppClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "tiny-cpp";

        modelTemplateFiles.put("model.mustache", ".md");
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".cpp");
        apiTemplateFiles.put("api.mustache", ".md");

        modelPackage = sourceFolder + File.separator + "models";
        modelTemplateFiles.put("model.mustache", ".h");

        apiPackage = sourceFolder + File.separator + "service";
        apiTemplateFiles.put("service/api-header.mustache".replace('/', File.separatorChar), ".h");

        apiTemplateFiles.put("service/api-body.mustache".replace('/', File.separatorChar), ".cpp");

        embeddedTemplateDir = templateDir = "tiny-cpp-client";

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

        supportingFiles.add(new SupportingFile("service/Response.h.mustache", serviceFolder, "Response.h")); // TODO find right function for folder
        supportingFiles.add(new SupportingFile("service/AbstractService.h.mustache", serviceFolder, "AbstractService.h")); // TODO find right function for folder
        supportingFiles.add(new SupportingFile("service/AbstractService.cpp.mustache", serviceFolder, "AbstractService.cpp")); // TODO find right function for folder

        makeTypeMappings();

    }


    // FilePaths
    private static final String sourceFolder = "lib";
    private static final String serviceFolder = sourceFolder + File.separator + "service";

    // Types
    private static final String cpp_array_type = "std::list";

    private void makeTypeMappings() {
        typeMapping = new HashMap<>();

        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("long", "long");
        typeMapping.put("boolean", "bool");
        typeMapping.put("double", "double");
        typeMapping.put("array", cpp_array_type);
        typeMapping.put("number", "long");
        typeMapping.put("binary", "std::string");
        typeMapping.put("password", "std::string");
        typeMapping.put("file", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("Date", "std::string");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");
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
