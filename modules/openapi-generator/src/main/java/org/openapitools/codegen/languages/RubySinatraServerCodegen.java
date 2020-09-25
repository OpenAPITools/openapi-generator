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

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.EnumSet;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RubySinatraServerCodegen extends AbstractRubyCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(RubySinatraServerCodegen.class);

    protected String gemName;
    protected String moduleName;
    protected String gemVersion = "1.0.0";
    protected String libFolder = "lib";

    public RubySinatraServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        apiPackage = "lib";
        outputFolder = "generated-code" + File.separator + "sinatra";

        // no model
        modelTemplateFiles.clear();
        apiTemplateFiles.put("api.mustache", ".rb");
        embeddedTemplateDir = templateDir = "ruby-sinatra-server";

        // remove modelPackage and apiPackage added by default
        cliOptions.clear();
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // use constant model/api package (folder path)
        //setModelPackage("models");
        setApiPackage("api");

        supportingFiles.add(new SupportingFile("my_app.mustache", "", "my_app.rb"));
        supportingFiles.add(new SupportingFile("OpenAPIing.rb", libFolder, "openapiing.rb"));
        supportingFiles.add(new SupportingFile("config.ru", "", "config.ru"));
        supportingFiles.add(new SupportingFile("Gemfile", "", "Gemfile"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
        supportingFiles.add(new SupportingFile("openapi.mustache", "", "openapi.yaml"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "ruby-sinatra";
    }

    @Override
    public String getHelp() {
        return "Generates a Ruby Sinatra server library.";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage.replace("/", File.separator);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        if (type == null) {
            return null;
        }
        return type;
    }

    @Override
    public String toModelName(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model filename. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model filename. Renamed to " + underscore("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // underscore the model file name
        // PhoneNumber.rb => phone_number.rb
        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PhoneNumberApi.rb => phone_number_api.rb
        return underscore(name) + "_api";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return camelize(name) + "Api";
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }
}
