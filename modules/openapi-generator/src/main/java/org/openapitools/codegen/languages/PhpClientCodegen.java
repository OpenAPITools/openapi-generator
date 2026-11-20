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
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

    /**
     * Matches a trailing run of digits at the end of a name, used when a
     * renamed parameter still collides with another parameter of the same
     * operation and must be numbered ({@code param_multipart2}, ...).
     */
    private static final Pattern PARAM_TRAILING_DIGITS = Pattern.compile("\\d+$");

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationsMap result = super.postProcessOperationsWithModels(objs, allModels);
        for (CodegenOperation op : result.getOperations().getOperation()) {
            renameCollidingSpecParams(op);
        }
        return result;
    }

    /**
     * Renames spec parameters whose final param name collides with a
     * function-scope name the api templates emit for this operation, so the
     * generated client keeps working. The check runs on the converted param
     * name, so it stays convention-aware: under the default snake_case
     * convention e.g. {@code resourcePath} becomes {@code $resource_path} and
     * does not collide with the {@code $resourcePath} local, while under
     * camelCase it does collide. Colliding parameters are renamed to
     * {@code param_<name>} (converted through {@link #toVarName(String)});
     * when that name is already taken by another parameter of the same
     * operation it is numbered ({@code param_multipart2}, ...).
     *
     * <p>Only the combinations verified to break the generated client are
     * renamed; the same name is left alone wherever it is harmless:
     * <ul>
     * <li>{@code this} - always: {@code function op($this)} fails with
     * "Cannot use $this as parameter", and under x-group-parameters the
     * unboxing line re-assigns {@code $this}</li>
     * <li>{@code resourcePath}, {@code formParams}, {@code queryParams},
     * {@code headerParams}, {@code httpBody}, {@code multipart} - always:
     * initialized at the top of xxxRequest() before the caller's value is
     * consumed</li>
     * <li>{@code returnType} - except under x-group-parameters: assigned
     * before the parameters are forwarded to xxxRequest() inside
     * xxxAsyncWithHttpInfo(). Under x-group-parameters the parameter stays
     * inside {@code $associative_array} and is unboxed only within
     * xxxRequest(), so it does not collide</li>
     * <li>{@code contentType} - guzzle only: {@code string $contentType} is
     * appended to every operation signature (duplicate parameter
     * declarations are a compile-time fatal error); under x-group-parameters
     * it is additionally re-read from {@code $associative_array['contentType']}
     * after the spec param unboxing</li>
     * <li>{@code hostIndex}, {@code variables} - guzzle, only for operations
     * with servers: appended to the signature, which is a duplicate
     * parameter declaration; under x-group-parameters they are re-read
     * from {@code $associative_array['hostIndex']}/['variables'] after the
     * spec param unboxing instead. psr-18 appends no signature parameters,
     * so these names stay safe there</li>
     * <li>{@code associative_array} - only for operations with
     * x-group-parameters: the unboxing lines re-assign
     * {@code $associative_array} before the remaining keys are read.
     * psr-18 never reads the wrapper after the unboxing, so there the
     * name only collides when another parameter is still unboxed after
     * it; guzzle re-reads {@code $associative_array['contentType']}/
     * ['hostIndex']/['variables'] afterwards, so it always collides</li>
     * <li>{@code localVariableKey}, {@code localVariableValue} - psr-18
     * only: the explode query foreach in xxxRequest() uses these loop
     * variables and would overwrite the caller's value before it is
     * read. guzzle emits no such locals</li>
     * <li>{@code headers} - only a body param of that name: read by
     * {@code isset($headers)} after {@code $headers} already holds the
     * headerSelector result, so the caller's body is never sent. Params in
     * other positions are consumed before the assignment and stay safe</li>
     * <li>{@code formDataProcessor} - only a form param of that name: the
     * field is read inside {@code prepare([...])} after
     * {@code $formDataProcessor = new FormDataProcessor()} overwrote it.
     * Params in other positions are consumed before the assignment</li>
     * </ul>
     *
     * <p>A parameter whose name comes from {@code parameterNameMappings} is
     * never renamed; the user's explicit mapping wins.
     */
    private void renameCollidingSpecParams(CodegenOperation op) {
        boolean groupParams;
        if (op.vendorExtensions.containsKey("x-group-parameters")) {
            // {{#exts.x-group-parameters}} renders for any truthy value, not
            // just Boolean.TRUE
            Object groupFlag = op.vendorExtensions.get("x-group-parameters");
            groupParams = groupFlag != null && !Boolean.FALSE.equals(groupFlag);
        } else {
            // DefaultGenerator injects the top-level flag into operations
            // without their own only when it parses to true
            groupParams = Boolean.parseBoolean(
                    String.valueOf(vendorExtensions().get("x-group-parameters")));
        }
        boolean hasServers = op.servers != null && !op.servers.isEmpty();
        boolean guzzle = !isLibrary(PSR18);

        // the same spec parameter appears as separate copies in several lists
        // (allParams, the per-location lists, required/optional, ...); group
        // them by baseName + location so every copy the templates read gets
        // the same new name
        Map<String, List<CodegenParameter>> copies = new LinkedHashMap<>();
        collectParamCopies(copies, op.allParams);
        collectParamCopies(copies, op.bodyParams);
        collectParamCopies(copies, op.formParams);
        collectParamCopies(copies, op.queryParams);
        collectParamCopies(copies, op.pathParams);
        collectParamCopies(copies, op.headerParams);
        collectParamCopies(copies, op.cookieParams);
        collectParamCopies(copies, op.implicitHeadersParams);
        collectParamCopies(copies, op.constantParams);
        collectParamCopies(copies, op.requiredParams);
        collectParamCopies(copies, op.optionalParams);
        collectParamCopies(copies, op.requiredAndNotNullableParams);
        collectParamCopies(copies, op.notNullableParams);
        if (op.bodyParam != null) {
            copies.computeIfAbsent(paramKey(op.bodyParam), k -> new ArrayList<>()).add(op.bodyParam);
        }

        Set<String> usedNames = new HashSet<>();
        for (CodegenParameter p : op.allParams) {
            usedNames.add(p.paramName);
        }

        for (int i = 0; i < op.allParams.size(); i++) {
            CodegenParameter p = op.allParams.get(i);
            String newName = renamedParamName(p, guzzle, hasServers, groupParams,
                    i == op.allParams.size() - 1);
            if (newName == null) {
                continue;
            }
            while (usedNames.contains(newName)) {
                newName = nextParamName(newName);
            }
            usedNames.add(newName);
            for (CodegenParameter copy : copies.getOrDefault(paramKey(p), Collections.singletonList(p))) {
                copy.paramName = newName;
            }
        }
    }

    /**
     * Collects the parameters of one list into {@code copies}, keyed by
     * {@link #paramKey(CodegenParameter)}.
     */
    private static void collectParamCopies(Map<String, List<CodegenParameter>> copies,
            List<CodegenParameter> params) {
        if (params == null) {
            return;
        }
        for (CodegenParameter p : params) {
            copies.computeIfAbsent(paramKey(p), k -> new ArrayList<>()).add(p);
        }
    }

    /**
     * Identity of a spec parameter inside an operation: the wire name plus
     * the location, so e.g. a body param and a header param sharing the name
     * {@code headers} are still told apart.
     */
    private static String paramKey(CodegenParameter p) {
        return p.baseName + " " + p.isBodyParam + p.isFormParam + p.isQueryParam
                + p.isPathParam + p.isHeaderParam + p.isCookieParam;
    }

    /**
     * Returns the name the spec parameter must be renamed to, or null when
     * the name does not collide for this operation. {@code lastParam} is
     * true when the parameter is the last one the x-group-parameters
     * unboxing emits.
     */
    private String renamedParamName(CodegenParameter p, boolean guzzle,
            boolean hasServers, boolean groupParams, boolean lastParam) {
        if (parameterNameMapping.containsKey(p.baseName)) {
            // a name the user chose explicitly through parameterNameMappings
            // wins over the collision handling
            return null;
        }
        boolean collides;
        switch (p.paramName) {
            case "this":
            case "resourcePath":
            case "formParams":
            case "queryParams":
            case "headerParams":
            case "httpBody":
            case "multipart":
                collides = true;
                break;
            case "returnType":
                collides = !groupParams;
                break;
            case "contentType":
                collides = guzzle;
                break;
            case "hostIndex":
            case "variables":
                collides = guzzle && hasServers;
                break;
            case "associative_array":
                // guzzle re-reads $associative_array['contentType']/
                // ['hostIndex']/['variables'] after the unboxing, so the
                // wrapper must stay an array. psr-18 never reads it after
                // the unboxing, so the name is harmless on the parameter
                // unboxed last
                collides = groupParams && (guzzle || !lastParam);
                break;
            case "localVariableKey":
            case "localVariableValue":
                // psr-18's explode query foreach uses these loop variables;
                // guzzle emits no such locals
                collides = !guzzle;
                break;
            case "headers":
                collides = p.isBodyParam;
                break;
            case "formDataProcessor":
                collides = p.isFormParam;
                break;
            default:
                collides = false;
        }
        return collides ? toVarName("param_" + p.baseName) : null;
    }

    /**
     * Numbering used when a renamed parameter still collides with another
     * parameter of the same operation: {@code param_x} => {@code param_x2},
     * {@code param_x2} => {@code param_x3}, ... Same convention as the
     * existing deduplication inside DefaultCodegen.fromOperation.
     */
    private static String nextParamName(String name) {
        Matcher matcher = PARAM_TRAILING_DIGITS.matcher(name);
        if (matcher.find()) {
            String digits = matcher.group();
            return name.substring(0, name.length() - digits.length())
                    + (Integer.parseInt(digits) + 1);
        }
        return name + "2";
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
}
