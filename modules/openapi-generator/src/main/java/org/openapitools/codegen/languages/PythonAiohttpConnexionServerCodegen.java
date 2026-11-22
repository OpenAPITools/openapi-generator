/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * <p>Mustache templates are located in {@code src/main/resources/python-aiohttp/}.
 */
public class PythonAiohttpConnexionServerCodegen extends AbstractPythonConnexionServerCodegen {
    /**
     * additionalProperties key holding the handler argument name Connexion passes the
     * aiohttp request context to ({@code pass_context_arg_name}).
     */
    public static final String CONNEXION_CONTEXT_ARG_NAME = "connexionContextArgName";
    public static final String DEFAULT_CONNEXION_CONTEXT_ARG_NAME = "request";
    private static final String FALLBACK_CONNEXION_CONTEXT_ARG_NAME = "request_context";
    private static final Pattern TRAILING_DIGITS = Pattern.compile("\\d+\\z");

    private final Logger LOGGER = LoggerFactory.getLogger(PythonAiohttpConnexionServerCodegen.class);

    public PythonAiohttpConnexionServerCodegen() {
        super("python-aiohttp", true);

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
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

        testPackage = "tests";
        embeddedTemplateDir = templateDir = "python-aiohttp";
        additionalProperties.put(CONNEXION_CONTEXT_ARG_NAME, DEFAULT_CONNEXION_CONTEXT_ARG_NAME);
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-aiohttp";
    }

    @Override
    protected void addSupportingFiles() {
        supportingFiles.add(new SupportingFile("conftest.mustache", testPackage, "conftest.py"));
        supportingFiles.add(new SupportingFile("__init__test.mustache", testPackage, "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__main.mustache", packagePath(), "__init__.py"));
        supportingFiles.add(new SupportingFile("setup.mustache", "", "setup.py"));
        supportingFiles.add(new SupportingFile("tox.mustache", "", "tox.ini"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
    }

    @Override
    public String generatorLanguageVersion() {
        return "3.5.2+";
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        // The generated app calls add_api once for the whole spec, so a single context
        // argument name must not collide with any operation's handler argument name.
        additionalProperties.put(CONNEXION_CONTEXT_ARG_NAME, connexionContextArgName(openAPI));
    }

    /**
     * Resolves the handler argument name Connexion passes the aiohttp request context to.
     * Returns {@value #DEFAULT_CONNEXION_CONTEXT_ARG_NAME} unless an operation uses that
     * name for a parameter, in which case {@code request_context} (numbered if needed) is
     * chosen so it does not collide with any handler argument name.
     */
    private String connexionContextArgName(OpenAPI openAPI) {
        Set<String> argNames = new HashSet<>();
        if (openAPI.getPaths() != null) {
            for (PathItem pathItem : openAPI.getPaths().values()) {
                for (Operation operation : pathItem.readOperationsMap().values()) {
                    collectOperationArgNames(pathItem, operation, openAPI, argNames);
                }
            }
        }
        if (openAPI.getWebhooks() != null) {
            for (PathItem pathItem : openAPI.getWebhooks().values()) {
                for (Operation operation : pathItem.readOperationsMap().values()) {
                    collectOperationArgNames(pathItem, operation, openAPI, argNames);
                }
            }
        }
        String candidate = DEFAULT_CONNEXION_CONTEXT_ARG_NAME;
        if (argNames.contains(candidate)) {
            candidate = FALLBACK_CONNEXION_CONTEXT_ARG_NAME;
            for (int suffix = 2; argNames.contains(candidate); suffix++) {
                candidate = FALLBACK_CONNEXION_CONTEXT_ARG_NAME + suffix;
            }
            LOGGER.warn("The default context argument name '{}' is already used by a handler " +
                            "argument. Connexion will pass the request context to handlers as '{}' instead.",
                    DEFAULT_CONNEXION_CONTEXT_ARG_NAME, candidate);
        }
        return candidate;
    }

    /**
     * Collects every name that can become a handler argument or a local variable in the
     * generated handler of one operation. Base names are gathered in the order
     * {@code DefaultCodegen} builds {@code allParams} — regular parameters (operation
     * parameters, then path-item parameters propagated by {@code DefaultGenerator}), then
     * form fields or the body argument — and renamed with the same trailing-number scheme
     * {@code DefaultCodegen} applies to duplicates ({@code status} → {@code status2},
     * {@code status2} → {@code status3}), so names produced by that renumbering are
     * collected too.
     */
    private void collectOperationArgNames(PathItem pathItem, Operation operation, OpenAPI openAPI, Set<String> argNames) {
        List<String> names = new ArrayList<>();
        Set<String> operationParamIds = new HashSet<>();
        if (operation.getParameters() != null) {
            for (Parameter parameter : operation.getParameters()) {
                operationParamIds.add(parameterId(parameter));
                addParamName(parameter, openAPI, names);
            }
        }
        if (pathItem.getParameters() != null) {
            for (Parameter parameter : pathItem.getParameters()) {
                // DefaultGenerator propagates a path-item parameter into the operation
                // unless the operation defines one with the same name and location
                if (operationParamIds.add(parameterId(parameter))) {
                    addParamName(parameter, openAPI, names);
                }
            }
        }
        RequestBody requestBody = operation.getRequestBody();
        if (requestBody != null) {
            if (StringUtils.isNotEmpty(requestBody.get$ref())) {
                requestBody = ModelUtils.getReferencedRequestBody(openAPI, requestBody);
            }
            if (requestBody != null) {
                collectBodyArgNames(operation, requestBody, openAPI, names, argNames);
            }
        }
        Set<String> usedNames = new HashSet<>();
        for (String name : names) {
            while (!usedNames.add(name)) {
                name = nextParamName(name);
            }
            argNames.add(name);
        }
    }

    /**
     * The key {@code DefaultGenerator.generateParameterId} uses to decide whether a
     * path-item parameter is already defined at the operation level.
     */
    private static String parameterId(Parameter parameter) {
        return parameter.get$ref() == null
                ? parameter.getName() + ":" + parameter.getIn()
                : parameter.get$ref();
    }

    /**
     * The renumbering {@code DefaultCodegen.generateNextName} applies to a duplicate
     * parameter name: increment a trailing number, otherwise append {@code 2}.
     */
    private static String nextParamName(String name) {
        Matcher matcher = TRAILING_DIGITS.matcher(name);
        if (matcher.find()) {
            String numStr = matcher.group();
            return name.substring(0, name.length() - numStr.length()) + (Integer.parseInt(numStr) + 1);
        }
        return name + "2";
    }

    private void addParamName(Parameter parameter, OpenAPI openAPI, List<String> argNames) {
        if (StringUtils.isNotEmpty(parameter.get$ref())) {
            parameter = ModelUtils.getReferencedParameter(openAPI, parameter);
        }
        if (parameter != null && StringUtils.isNotEmpty(parameter.getName())) {
            argNames.add(toParamName(parameter.getName()));
        }
    }

    /**
     * Appends the names a request body contributes to the handler signature to
     * {@code names} (the ordered base names the duplicate-parameter renumbering runs over).
     * For a form body these are the form fields, which become handler arguments even when
     * declared inside a composed schema, so the schema is walked the same way
     * {@code DefaultCodegen.fromRequestBodyToFormParameters} walks it. For other bodies it
     * is the body argument name. Names that can only reach the handler through Connexion
     * ({@code x-body-name}) go to {@code extraArgNames} instead.
     */
    private void collectBodyArgNames(Operation operation, RequestBody requestBody, OpenAPI openAPI,
                                     List<String> names, Set<String> extraArgNames) {
        // the handler argument the request body is bound to: x-codegen-request-body-name
        // ("body" by default); x-body-name is the keyword Connexion passes it under
        String bodyArgName = extensionArgName(operation.getExtensions(), "x-codegen-request-body-name");
        if (requestBody.getExtensions() != null) {
            String requestBodyName = extensionArgName(requestBody.getExtensions(), "x-codegen-request-body-name");
            if (requestBodyName != null) {
                bodyArgName = requestBodyName;
            }
            addExtensionArgName(requestBody.getExtensions(), "x-body-name", extraArgNames);
        }
        String contentType = getContentType(requestBody);
        if (contentType != null) {
            contentType = contentType.toLowerCase(Locale.ROOT);
        }
        if (contentType != null && (contentType.startsWith("application/x-www-form-urlencoded")
                || contentType.startsWith("multipart"))) {
            Schema<?> schema = ModelUtils.getReferencedSchema(openAPI, ModelUtils.getSchemaFromRequestBody(requestBody));
            Map<String, Schema> properties = new LinkedHashMap<>();
            addProperties(properties, new ArrayList<>(), schema, new HashSet<>());
            for (String propertyName : properties.keySet()) {
                names.add(toParamName(propertyName));
            }
            extraArgNames.add(toParamName(bodyArgName != null ? bodyArgName : "body"));
        } else {
            names.add(toParamName(bodyArgName != null ? bodyArgName : "body"));
        }
    }

    private String extensionArgName(Map<String, Object> extensions, String extensionName) {
        Object name = extensions != null ? extensions.get(extensionName) : null;
        return name != null ? String.valueOf(name) : null;
    }

    private void addExtensionArgName(Map<String, Object> extensions, String extensionName, Set<String> argNames) {
        String name = extensionArgName(extensions, extensionName);
        if (name != null) {
            argNames.add(toParamName(name));
        }
    }
}
