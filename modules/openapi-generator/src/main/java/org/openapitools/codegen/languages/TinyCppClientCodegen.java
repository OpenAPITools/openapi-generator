package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;

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
        outputFolder = "generated-code" + File.separator + "tiny-cpp" + File.separator + sourceFolder;


        modelPackage = sourceFolder + File.separator + "models";
        modelTemplateFiles.put("model.mustache", ".h");

        apiPackage = sourceFolder + File.separator + "service";
        apiTemplateFiles.put("service/api-header.mustache".replace('/', File.separatorChar), ".h");
        apiTemplateFiles.put("service/api-body.mustache".replace('/', File.separatorChar), ".cpp");

        embeddedTemplateDir = templateDir = "tiny-cpp-client";

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("service/Response.h.mustache", serviceFolder, "Response.h")); // TODO find right function for folder
        supportingFiles.add(new SupportingFile("service/AbstractService.h.mustache", serviceFolder, "AbstractService.h")); // TODO find right function for folder
        supportingFiles.add(new SupportingFile("service/AbstractService.cpp.mustache", serviceFolder, "AbstractService.cpp")); // TODO find right function for folder

        makeTypeMappings();

    }


    @Override
    public String toModelImport(String name) {
        if (name.equals("std::string")) {
            return "#include <string>";
        //} else if (name.equals(cpp_hashmap_type)) {
        //    return "#include <map>";
        } else if (name.equals(cpp_array_type)) {
            return "#include <list>";
        }
        return "#include \"" + name + ".h\"";
    }

    // FilePaths
    private static final String sourceFolder = "lib";
    private static final String serviceFolder = sourceFolder + File.separator + "service";

    // Types
    private static final String cpp_array_type = "std::list";
    //private static final String cpp_hashmap_type = "std::map";

    private void makeTypeMappings() {
        super.typeMapping = new HashMap<>();

        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("long", "long long");
        typeMapping.put("boolean", "bool");
        typeMapping.put("double", "double");
        typeMapping.put("array", cpp_array_type);
        typeMapping.put("number", "long long");
        typeMapping.put("binary", "std::string");
        typeMapping.put("password", "std::string");
        typeMapping.put("file", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("Date", "std::string");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");
    }
}
