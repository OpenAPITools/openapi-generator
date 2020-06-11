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

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.Locale;

public class AdaServerCodegen extends AbstractAdaCodegen implements CodegenConfig {

    public AdaServerCodegen() {
        super();

        // TODO: Ada maintainer review.
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .excludeWireFormatFeatures(
                        WireFormatFeature.XML,
                        WireFormatFeature.PROTOBUF
                )
                .excludeSecurityFeatures(
                        SecurityFeature.OpenIDConnect,
                        SecurityFeature.OAuth2_Password,
                        SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_ClientCredentials,
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.BearerToken
                )
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
                        ParameterFeature.Header,
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(ClientModificationFeature.BasePath)
        );
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "ada-server";
    }

    @Override
    public String getHelp() {
        return "Generates an Ada server implementation (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            packageName = (String) additionalProperties.get(CodegenConstants.PACKAGE_NAME);
        }
        String srcPrefix = "src" + File.separator;
        String serverPrefix = srcPrefix + "server" + File.separator + toFilename(modelPackage);
        String modelPrefix = srcPrefix + "model" + File.separator + toFilename(modelPackage);
        String implPrefix = srcPrefix + toFilename(modelPackage);
        supportingFiles.add(new SupportingFile("model-spec.mustache", null, modelPrefix + "-models.ads"));
        supportingFiles.add(new SupportingFile("model-body.mustache", null, modelPrefix + "-models.adb"));
        supportingFiles.add(new SupportingFile("server-skeleton-spec.mustache", null, serverPrefix + "-skeletons.ads"));
        supportingFiles.add(new SupportingFile("server-skeleton-body.mustache", null, serverPrefix + "-skeletons.adb"));
        supportingFiles.add(new SupportingFile("server-spec.mustache", null, implPrefix + "-servers.ads"));
        supportingFiles.add(new SupportingFile("server-body.mustache", null, implPrefix + "-servers.adb"));

        supportingFiles.add(new SupportingFile("openapi.mustache", "web" + File.separator + "swagger", "openapi.json"));

        if (additionalProperties.containsKey(CodegenConstants.PROJECT_NAME)) {
            projectName = (String) additionalProperties.get(CodegenConstants.PROJECT_NAME);
        } else {
            // default: set project based on package name
            // e.g. petstore.api (package name) => petstore_api (project name)
            projectName = packageName.replaceAll("\\.", "_");
        }
        String configBaseName = modelPackage.toLowerCase(Locale.ROOT);
        supportingFiles.add(new SupportingFile("gnat-project.mustache", "", toFilename(projectName) + ".gpr"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("config.gpr", "", "config.gpr"));
        supportingFiles.add(new SupportingFile("server-properties.mustache", "", configBaseName + ".properties"));

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("package", this.modelPackage);
        additionalProperties.put("packageConfig", configBaseName);
        additionalProperties.put("packageDir", "server");
        additionalProperties.put("mainName", "server");
        additionalProperties.put("isServer", "true");
        additionalProperties.put(CodegenConstants.PROJECT_NAME, projectName);

        String names[] = this.modelPackage.split("\\.");
        String pkgName = names[0];
        additionalProperties.put("packageLevel1", pkgName);
        supportingFiles.add(new SupportingFile("package-spec-level1.mustache", null,
                            "src" + File.separator + toFilename(names[0]) + ".ads"));
        if (names.length > 1) {
            String fileName = toFilename(names[0]) + "-" + toFilename(names[1]) + ".ads";
            pkgName = names[0] + "." + names[1];
            additionalProperties.put("packageLevel2", pkgName);
            supportingFiles.add(new SupportingFile("package-spec-level2.mustache", null,
                                "src" + File.separator + fileName));
        }
        pkgName = this.modelPackage;
        supportingFiles.add(new SupportingFile("server.mustache", null,
                            "src" + File.separator + toFilename(pkgName) + "-server.adb"));
        additionalProperties.put("packageName", toFilename(pkgName));

        // add lambda for mustache templates
        additionalProperties.put("lambdaAdaComment", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                String content = fragment.execute();
                content = content.trim().replaceAll("\n$", "");
                writer.write(content.replaceAll("\n", "\n   --  "));
            }
        });
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + "model" + File.separator + modelPackage().replace('.', File.separatorChar);
    }
}
