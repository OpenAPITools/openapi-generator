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
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

import java.io.File;

public class TypeScriptAngularJsClientCodegen extends AbstractTypeScriptClientCodegen {

    public TypeScriptAngularJsClientCodegen() {
        super();
        outputFolder = "generated-code/typescript-angularjs";
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");
        embeddedTemplateDir = templateDir = "typescript-angularjs";
        apiPackage = "api";
        modelPackage = "model";

        removeOption(NPM_NAME);
        removeOption(NPM_VERSION);
        removeOption(SNAPSHOT);

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.DEPRECATED)
                .build();
    }

    @Override
    public String getName() {
        return "typescript-angularjs-deprecated";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript AngularJS client library. This generator has been deprecated and will be removed in the future release.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        supportingFiles.add(new SupportingFile("models.mustache", modelPackage().replace('.', File.separatorChar), "models.ts"));
        supportingFiles.add(new SupportingFile("apis.mustache", apiPackage().replace('.', File.separatorChar), "api.ts"));
        supportingFiles.add(new SupportingFile("index.mustache", getIndexDirectory(), "index.ts"));
        supportingFiles.add(new SupportingFile("api.module.mustache", getIndexDirectory(), "api.module.ts"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));

    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        if (isLanguagePrimitive(openAPIType) || isLanguageGenericType(openAPIType)) {
            return openAPIType;
        }
        return addModelPrefix(openAPIType);
    }

    @Override
    public String getTypeDeclaration(String name) {
        return addModelPrefix(name);
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        // parameter.dataType = addModelPrefix(parameter.dataType);
    }

    private String getIndexDirectory() {
        String indexPackage = modelPackage.substring(0, Math.max(0, modelPackage.lastIndexOf('.')));
        return indexPackage.replace('.', File.separatorChar);
    }

    private String addModelPrefix(String openAPIType) {
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
        } else {
            type = openAPIType;
        }

        if (!isLanguagePrimitive(type) && !isLanguageGenericType(type)) {
            type = "models." + openAPIType;
        }
        return type;
    }

    private boolean isLanguagePrimitive(String type) {
        return languageSpecificPrimitives.contains(type);
    }

    private boolean isLanguageGenericType(String type) {
        for (String genericType : languageGenericTypes) {
            if (type.startsWith(genericType + "<")) {
                return true;
            }
        }
        return false;
    }
}
