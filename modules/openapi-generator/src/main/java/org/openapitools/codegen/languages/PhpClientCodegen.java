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

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.model.WebhooksMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * <p>Mustache templates are located in
 * {@code src/main/resources/php/} (root templates shared across all libraries) and
 * {@code src/main/resources/php/libraries/} (library-specific overrides).
 * A library-specific template shadows a root-level template of the same name.
 */
public class PhpClientCodegen extends AbstractPhpCodegen {
    @SuppressWarnings("hiding")
    private final Logger LOGGER = LoggerFactory.getLogger(PhpClientCodegen.class);
    public static final String GUZZLE = "guzzle";
    public static final String PSR18 = "psr-18";

    public PhpClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .includeGlobalFeatures(GlobalFeature.MultiServer)
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
        );

        // clear import mapping (from default generator) as php does not use it
        // at the moment
        importMapping.clear();

        setInvokerPackage("OpenAPI\\Client");
        setApiPackage(getInvokerPackage() + "\\" + apiDirName);
        setModelPackage(getInvokerPackage() + "\\" + modelDirName);
        setPackageName("OpenAPIClient-php");
        supportsInheritance = true;
        setOutputDir("generated-code" + File.separator + "php");
        modelTestTemplateFiles.put("model_test.mustache", ".php");
        embeddedTemplateDir = templateDir = "php";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // provide primitives to mustache template
        List sortedLanguageSpecificPrimitives = new ArrayList(languageSpecificPrimitives);
        Collections.sort(sortedLanguageSpecificPrimitives);
        String primitives = "'" + StringUtils.join(sortedLanguageSpecificPrimitives, "', '") + "'";
        additionalProperties.put("primitives", primitives);

        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.ALLOW_UNICODE_IDENTIFIERS_DESC)
                .defaultValue(Boolean.TRUE.toString()));

        supportedLibraries.put(GUZZLE, "Guzzle");
        supportedLibraries.put(PSR18, "psr/http-client-implementation, also known as PSR-18. (beta support)");
        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "HTTP library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        // set GUZZLE as the default
        libraryOption.setDefault(GUZZLE);
        cliOptions.add(libraryOption);
        setLibrary(GUZZLE);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "php";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        supportingFiles.add(new SupportingFile("ApiException.mustache", toSrcPath(invokerPackage, srcBasePath), "ApiException.php"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", toSrcPath(invokerPackage, srcBasePath), "Configuration.php"));
        supportingFiles.add(new SupportingFile("FormDataProcessor.mustache", toSrcPath(invokerPackage, srcBasePath), "FormDataProcessor.php"));
        supportingFiles.add(new SupportingFile("ObjectSerializer.mustache", toSrcPath(invokerPackage, srcBasePath), "ObjectSerializer.php"));
        supportingFiles.add(new SupportingFile("ModelInterface.mustache", toSrcPath(modelPackage, srcBasePath), "ModelInterface.php"));
        supportingFiles.add(new SupportingFile("HeaderSelector.mustache", toSrcPath(invokerPackage, srcBasePath), "HeaderSelector.php"));
        supportingFiles.add(new SupportingFile("composer.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("phpunit.xml.mustache", "", "phpunit.xml.dist"));
        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile(".php-cs-fixer.dist.php", "", ".php-cs-fixer.dist.php"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));

        if (additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            this.setLibrary((String) additionalProperties.get(CodegenConstants.LIBRARY));
        }

        if (PSR18.equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("DebugPlugin.mustache", toSrcPath(invokerPackage, srcBasePath), "DebugPlugin.php"));
        }

    }

    @Override
    public boolean supportsAdditionalOperations() {
        // only the guzzle template can emit arbitrary methods verbatim: psr-18
        // delegates method handling to the injected PSR-17/PSR-18 implementation
        // (Guzzle factory up-cases, Symfony rejects non-uppercase tokens)
        return GUZZLE.equals(getLibrary());
    }

    @Override
    protected boolean supportsQueryStringParameters() {
        return GUZZLE.equals(getLibrary());
    }

    private static final Set<String> STANDARD_HTTP_METHODS = new HashSet<>(Arrays.asList(
            "GET", "PUT", "POST", "DELETE", "OPTIONS", "HEAD", "PATCH", "TRACE", "CONNECT"));

    // RFC 9110 tchar: method tokens the generated client can send verbatim
    private static final java.util.regex.Pattern HTTP_METHOD_TOKEN_PATTERN =
            java.util.regex.Pattern.compile("[!#$%&'*+\\-.^_`|~0-9A-Za-z]+");

    /**
     * Marks OpenAPI 3.2 (query/additionalOperations) HTTP methods for verbatim emission and
     * operations carrying {@code in: querystring} parameters. Operation names that are not
     * valid RFC 9110 tokens are warned about and skipped.
     *
     * <p>Extensions set: {@code x-php-verbatim-method} (non-standard method present),
     * {@code x-php-http-method-literal} (token escaped for a PHP single-quoted string -
     * {@code '} is valid tchar but would terminate the literal), and
     * {@code x-php-http-method-doc} ({@code |} escaped for markdown tables).
     */
    private void flagVerbatimHttpMethods(List<CodegenOperation> operationList) {
        Iterator<CodegenOperation> it = operationList.iterator();
        while (it.hasNext()) {
            CodegenOperation op = it.next();
            if (op.allParams != null && op.allParams.stream().anyMatch(p -> p.isQueryStringParam)) {
                op.vendorExtensions.put("x-php-has-querystring-param", true);
            }
            if (op.httpMethod == null || STANDARD_HTTP_METHODS.contains(op.httpMethod)) {
                continue;
            }
            if (!HTTP_METHOD_TOKEN_PATTERN.matcher(op.httpMethod).matches()) {
                LOGGER.warn("Skipping operation {}: HTTP method name '{}' is not a valid "
                        + "RFC 9110 token and cannot be emitted as a PHP string literal.",
                        op.operationId, op.httpMethod);
                it.remove();
                continue;
            }
            op.vendorExtensions.put("x-php-verbatim-method", true);
            op.vendorExtensions.put("x-php-http-method-literal", op.httpMethod.replace("'", "\\'"));
            op.vendorExtensions.put("x-php-http-method-doc", op.httpMethod.replace("|", "\\|"));
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationsMap map = super.postProcessOperationsWithModels(objs, allModels);
        flagVerbatimHttpMethods(map.getOperations().getOperation());
        return map;
    }

    @Override
    public WebhooksMap postProcessWebhooksWithModels(WebhooksMap objs, List<ModelMap> allModels) {
        WebhooksMap map = super.postProcessWebhooksWithModels(objs, allModels);
        flagVerbatimHttpMethods(map.getWebhooks().getOperation());
        return map;
    }
}
