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

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.EnumSet;

public class GoClientCodegen extends AbstractGoCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(GoClientCodegen.class);

    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected boolean isGoSubmodule = false;
    public static final String WITH_GO_CODEGEN_COMMENT = "withGoCodegenComment";
    public static final String WITH_XML = "withXml";
    public static final String STRUCT_PREFIX = "structPrefix";
    public static final String WITH_AWSV4_SIGNATURE = "withAWSV4Signature";

    public GoClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .includeGlobalFeatures(
                        GlobalFeature.ParameterizedServer
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
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        outputFolder = "generated-code/go";
        modelTemplateFiles.put("model.mustache", ".go");
        apiTemplateFiles.put("api.mustache", ".go");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        embeddedTemplateDir = templateDir = "go";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        cliOptions.add(CliOption.newBoolean(CodegenConstants.IS_GO_SUBMODULE, CodegenConstants.IS_GO_SUBMODULE_DESC));
        cliOptions.add(CliOption.newBoolean(WITH_GO_CODEGEN_COMMENT, "whether to include Go codegen comment to disable Go Lint and collapse by default in GitHub PRs and diffs"));
        cliOptions.add(CliOption.newBoolean(WITH_XML, "whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML)"));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ENUM_CLASS_PREFIX, CodegenConstants.ENUM_CLASS_PREFIX_DESC));
        cliOptions.add(CliOption.newBoolean(STRUCT_PREFIX, "whether to prefix struct with the class name. e.g. DeletePetOpts => PetApiDeletePetOpts"));
        cliOptions.add(CliOption.newBoolean(WITH_AWSV4_SIGNATURE, "whether to include AWS v4 signature support"));

        // option to change the order of form/body parameter
        cliOptions.add(CliOption.newBoolean(
                CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS,
                CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS_DESC)
                .defaultValue(Boolean.FALSE.toString()));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("openapi.mustache", "api", "openapi.yaml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("configuration.mustache", "", "configuration.go"));
        supportingFiles.add(new SupportingFile("client.mustache", "", "client.go"));
        supportingFiles.add(new SupportingFile("response.mustache", "", "response.go"));
        supportingFiles.add(new SupportingFile("go.mod.mustache", "", "go.mod"));
        supportingFiles.add(new SupportingFile("go.sum", "", "go.sum"));
        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));

        if (additionalProperties.containsKey(WITH_GO_CODEGEN_COMMENT)) {
            setWithGoCodegenComment(Boolean.parseBoolean(additionalProperties.get(WITH_GO_CODEGEN_COMMENT).toString()));
            if (withGoCodegenComment) {
                additionalProperties.put(WITH_GO_CODEGEN_COMMENT, "true");
            }
        }

        if (additionalProperties.containsKey(WITH_AWSV4_SIGNATURE)) {
            setWithAWSV4Signature(Boolean.parseBoolean(additionalProperties.get(WITH_AWSV4_SIGNATURE).toString()));
            if (withAWSV4Signature) {
                additionalProperties.put(WITH_AWSV4_SIGNATURE, "true");
            }
        }

        if (additionalProperties.containsKey(WITH_XML)) {
            setWithXml(Boolean.parseBoolean(additionalProperties.get(WITH_XML).toString()));
            if (withXml) {
                additionalProperties.put(WITH_XML, "true");
            }
        }

        if (additionalProperties.containsKey(CodegenConstants.ENUM_CLASS_PREFIX)) {
            setEnumClassPrefix(Boolean.parseBoolean(additionalProperties.get(CodegenConstants.ENUM_CLASS_PREFIX).toString()));
            if (enumClassPrefix) {
                additionalProperties.put(CodegenConstants.ENUM_CLASS_PREFIX, "true");
            }
        }

        if (additionalProperties.containsKey(CodegenConstants.IS_GO_SUBMODULE)) {
            setIsGoSubmodule(Boolean.parseBoolean(additionalProperties.get(CodegenConstants.IS_GO_SUBMODULE).toString()));
            if (isGoSubmodule) {
                additionalProperties.put(CodegenConstants.IS_GO_SUBMODULE, "true");
            }
        }

        if (additionalProperties.containsKey(STRUCT_PREFIX)) {
            setStructPrefix(Boolean.parseBoolean(additionalProperties.get(STRUCT_PREFIX).toString()));
            if (structPrefix) {
                additionalProperties.put(STRUCT_PREFIX, "true");
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
        return "go";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Go client library (beta).";
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator;
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

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setIsGoSubmodule(boolean isGoSubmodule) {
        this.isGoSubmodule = isGoSubmodule;
    }

}
