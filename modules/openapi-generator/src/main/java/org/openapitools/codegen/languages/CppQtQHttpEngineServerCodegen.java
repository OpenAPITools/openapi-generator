/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.DocumentationFeature;

import java.io.File;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class CppQtQHttpEngineServerCodegen extends CppQtAbstractCodegen implements CodegenConfig {

    protected final String SRC_DIR = "/src";
    protected final String MODEL_DIR = "/src/models";
    protected final String APIHANDLER_DIR = "/src/handlers";
    protected final String APIREQUEST_DIR = "/src/requests";

    // source folder where to write the files
    protected String sourceFolder = "server";

    public CppQtQHttpEngineServerCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        // set the output folder here
        outputFolder = "generated-code/cpp-qt-qhttpengine-server";

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
                "apihandler.h.mustache",   // the template to use
                ".h");       // the extension for each file to write

        apiTemplateFiles.put(
                "apihandler.cpp.mustache",   // the template to use
                ".cpp");       // the extension for each file to write

        apiTemplateFiles.put(
                "apirequest.h.mustache",   // the template to use
                ".h");       // the extension for each file to write

        apiTemplateFiles.put(
                "apirequest.cpp.mustache",   // the template to use
                ".cpp");       // the extension for each file to write

        /*
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "cpp-qt-qhttpengine-server";

        supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder + MODEL_DIR, PREFIX + "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder + MODEL_DIR, PREFIX + "Helpers.cpp"));
        supportingFiles.add(new SupportingFile("object.mustache", sourceFolder + MODEL_DIR, PREFIX + "Object.h"));
        supportingFiles.add(new SupportingFile("enum.mustache", sourceFolder + MODEL_DIR, PREFIX + "Enum.h"));
        supportingFiles.add(new SupportingFile("HttpFileElement.h.mustache", sourceFolder + MODEL_DIR, PREFIX + "HttpFileElement.h"));
        supportingFiles.add(new SupportingFile("HttpFileElement.cpp.mustache", sourceFolder + MODEL_DIR, PREFIX + "HttpFileElement.cpp"));
        supportingFiles.add(new SupportingFile("apirouter.h.mustache", sourceFolder + APIHANDLER_DIR, PREFIX + "ApiRouter.h"));
        supportingFiles.add(new SupportingFile("apirouter.cpp.mustache", sourceFolder + APIHANDLER_DIR, PREFIX + "ApiRouter.cpp"));


        supportingFiles.add(new SupportingFile("main.cpp.mustache", sourceFolder + SRC_DIR, "main.cpp"));
        supportingFiles.add(new SupportingFile("src-CMakeLists.txt.mustache", sourceFolder + SRC_DIR, "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("README.md.mustache", sourceFolder, "README.MD"));
        supportingFiles.add(new SupportingFile("Makefile.mustache", sourceFolder, "Makefile"));
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", sourceFolder, "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", sourceFolder, "Dockerfile"));
        supportingFiles.add(new SupportingFile("LICENSE.txt.mustache", sourceFolder, "LICENSE.txt"));
        typeMapping.put("file", PREFIX + "HttpFileElement");
        importMapping.put(PREFIX + "HttpFileElement", "#include \"" + PREFIX + "HttpFileElement.h\"");

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("modelNamePrefix")) {
            supportingFiles.clear();
            supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder + MODEL_DIR, modelNamePrefix + "Helpers.h"));
            supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder + MODEL_DIR, modelNamePrefix + "Helpers.cpp"));
            supportingFiles.add(new SupportingFile("object.mustache", sourceFolder + MODEL_DIR, modelNamePrefix + "Object.h"));
            supportingFiles.add(new SupportingFile("enum.mustache", sourceFolder + MODEL_DIR, modelNamePrefix + "Enum.h"));
            supportingFiles.add(new SupportingFile("HttpFileElement.h.mustache", sourceFolder + MODEL_DIR, modelNamePrefix + "HttpFileElement.h"));
            supportingFiles.add(new SupportingFile("HttpFileElement.cpp.mustache", sourceFolder + MODEL_DIR, modelNamePrefix + "HttpFileElement.cpp"));
            supportingFiles.add(new SupportingFile("apirouter.h.mustache", sourceFolder + APIHANDLER_DIR, modelNamePrefix + "ApiRouter.h"));
            supportingFiles.add(new SupportingFile("apirouter.cpp.mustache", sourceFolder + APIHANDLER_DIR, modelNamePrefix + "ApiRouter.cpp"));


            supportingFiles.add(new SupportingFile("main.cpp.mustache", sourceFolder + SRC_DIR, "main.cpp"));
            supportingFiles.add(new SupportingFile("src-CMakeLists.txt.mustache", sourceFolder + SRC_DIR, "CMakeLists.txt"));
            supportingFiles.add(new SupportingFile("README.md.mustache", sourceFolder, "README.MD"));
            supportingFiles.add(new SupportingFile("Makefile.mustache", sourceFolder, "Makefile"));
            supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", sourceFolder, "CMakeLists.txt"));
            supportingFiles.add(new SupportingFile("Dockerfile.mustache", sourceFolder, "Dockerfile"));
            supportingFiles.add(new SupportingFile("LICENSE.txt.mustache", sourceFolder, "LICENSE.txt"));
            typeMapping.put("file", modelNamePrefix + "HttpFileElement");
            importMapping.put(modelNamePrefix + "HttpFileElement", "#include \"" + modelNamePrefix + "HttpFileElement.h\"");
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
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "cpp-qt-qhttpengine-server";
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Qt C++ Server using the QHTTPEngine HTTP Library.";
    }

    /**
     * Location to write model files.  You can use the modelPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + MODEL_DIR + "/" + modelPackage().replace("::", File.separator);
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + APIHANDLER_DIR + "/" + apiPackage().replace("::", File.separator);
    }

    private String requestFileFolder() {
        return outputFolder + "/" + sourceFolder + APIREQUEST_DIR + "/" + apiPackage().replace("::", File.separator);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if (templateName.contains("apirequest")) {
            result = result.replace("ApiHandler", "ApiRequest");
            result = result.replace(apiFileFolder(), requestFileFolder());
        }
        return result;
    }

    @Override
    public String toApiFilename(String name) {
        return modelNamePrefix + sanitizeName(camelize(name)) + "ApiHandler";
    }

}
