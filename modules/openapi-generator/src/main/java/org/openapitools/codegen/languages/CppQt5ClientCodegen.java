/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;

import java.io.File;

import static org.openapitools.codegen.utils.StringUtils.*;

public class CppQt5ClientCodegen extends CppQt5AbstractCodegen implements CodegenConfig {
    public static final String OPTIONAL_PROJECT_FILE_DESC = "Generate client.pri.";
    // source folder where to write the files
    protected String sourceFolder = "client";
    protected boolean optionalProjectFileFlag = true;

    public CppQt5ClientCodegen() {
        super();

        // set the output folder here
        outputFolder = "generated-code/qt5cpp";

        /*
         * Models.  You can write model files using the modelTemplateFiles map.
         * if you want to create one template for file, you can do so here.
         * for multiple files for model, just put another entry in the `modelTemplateFiles` with
         * a different extension
         */
        modelTemplateFiles.put(
                "model-header.mustache",
                ".h");

        modelTemplateFiles.put(
                "model-body.mustache",
                ".cpp");

        /*
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        apiTemplateFiles.put(
                "api-header.mustache",   // the template to use
                ".h");       // the extension for each file to write

        apiTemplateFiles.put(
                "api-body.mustache",   // the template to use
                ".cpp");       // the extension for each file to write

        /*
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "cpp-qt5-client";

        addSwitch(CodegenConstants.OPTIONAL_PROJECT_FILE, OPTIONAL_PROJECT_FILE_DESC, this.optionalProjectFileFlag);
        supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder, PREFIX + "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder, PREFIX + "Helpers.cpp"));
        supportingFiles.add(new SupportingFile("HttpRequest.h.mustache", sourceFolder, PREFIX + "HttpRequest.h"));
        supportingFiles.add(new SupportingFile("HttpRequest.cpp.mustache", sourceFolder, PREFIX + "HttpRequest.cpp"));
        supportingFiles.add(new SupportingFile("object.mustache", sourceFolder, PREFIX + "Object.h"));
        supportingFiles.add(new SupportingFile("enum.mustache", sourceFolder, PREFIX + "Enum.h"));        
        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Project.mustache", sourceFolder, "client.pri"));
        }
        typeMapping.put("file", PREFIX + "HttpRequestInputFileElement");
        typeMapping.put("binary", PREFIX +"HttpRequestInputFileElement");
        importMapping.put(PREFIX + "HttpRequestInputFileElement", "#include \"" + PREFIX + "HttpRequest.h\"");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_FILE)) {
            setOptionalProjectFileFlag(convertPropertyToBooleanAndWriteBack(CodegenConstants.OPTIONAL_PROJECT_FILE));
        } else {
            additionalProperties.put(CodegenConstants.OPTIONAL_PROJECT_FILE, optionalProjectFileFlag);
        }
        
        if (additionalProperties.containsKey("modelNamePrefix")) {
            supportingFiles.clear();
            supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder, modelNamePrefix + "Helpers.h"));
            supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder, modelNamePrefix + "Helpers.cpp"));
            supportingFiles.add(new SupportingFile("HttpRequest.h.mustache", sourceFolder, modelNamePrefix + "HttpRequest.h"));
            supportingFiles.add(new SupportingFile("HttpRequest.cpp.mustache", sourceFolder, modelNamePrefix + "HttpRequest.cpp"));
            supportingFiles.add(new SupportingFile("object.mustache", sourceFolder, modelNamePrefix + "Object.h"));
            supportingFiles.add(new SupportingFile("enum.mustache", sourceFolder, modelNamePrefix + "Enum.h"));            

            typeMapping.put("file", modelNamePrefix + "HttpRequestInputFileElement");
            typeMapping.put("binary", modelNamePrefix + "HttpRequestInputFileElement");
            importMapping.put(modelNamePrefix + "HttpRequestInputFileElement", "#include \"" + modelNamePrefix + "HttpRequest.h\"");
            if (optionalProjectFileFlag) {
                supportingFiles.add(new SupportingFile("Project.mustache", sourceFolder, modelNamePrefix + "client.pri"));
            }
        }
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "cpp-qt5-client";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Qt5 C++ client library.";
    }

    /**
     * Location to write model files.  You can use the modelPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace("::", File.separator);
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace("::", File.separator);
    }

    @Override
    public String toApiFilename(String name) {
        return modelNamePrefix + sanitizeName(camelize(name)) + "Api";
    }

    public void setOptionalProjectFileFlag(boolean flag) {
        this.optionalProjectFileFlag = flag;
    }
}
