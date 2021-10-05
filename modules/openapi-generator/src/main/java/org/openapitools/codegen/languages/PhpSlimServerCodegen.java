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

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class PhpSlimServerCodegen extends AbstractPhpCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(PhpSlimServerCodegen.class);

    public static final String USER_CLASSNAME_KEY = "userClassname";

    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-server";
    protected String authDirName = "Auth";
    protected String authPackage = "";

    public PhpSlimServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
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
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.DEPRECATED)
                .build();

        // clear import mapping (from default generator) as slim does not use it
        // at the moment
        importMapping.clear();

        variableNamingConvention = "camelCase";
        artifactVersion = "1.0.0";
        setInvokerPackage("OpenAPIServer");
        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;
        authPackage = invokerPackage + "\\" + authDirName;
        outputFolder = "generated-code" + File.separator + "slim";

        modelTestTemplateFiles.put("model_test.mustache", ".php");
        // no doc files
        modelDocTemplateFiles.clear();
        apiDocTemplateFiles.clear();

        embeddedTemplateDir = templateDir = "php-slim-server";

        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);

        // override cliOptions from AbstractPhpCodegen
        for (CliOption co : cliOptions) {
            if (AbstractPhpCodegen.VARIABLE_NAMING_CONVENTION.equals(co.getOpt())) {
                co.setDescription("naming convention of variable name, e.g. camelCase.");
                co.setDefault("camelCase");
                break;
            }
        }
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "php-slim-deprecated";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP Slim Framework server library. IMPORTANT NOTE: this generator (Slim 3.x)  is no longer actively maintained so please use 'php-slim4' generator instead.";
    }

    @Override
    public String apiFileFolder() {
        if (apiPackage.startsWith(invokerPackage + "\\")) {
            // need to strip out invokerPackage from path
            return (outputFolder + File.separator + toSrcPath(StringUtils.removeStart(apiPackage, invokerPackage + "\\"), srcBasePath));
        }
        return (outputFolder + File.separator + toSrcPath(apiPackage, srcBasePath));
    }

    @Override
    public String modelFileFolder() {
        if (modelPackage.startsWith(invokerPackage + "\\")) {
            // need to strip out invokerPackage from path
            return (outputFolder + File.separator + toSrcPath(StringUtils.removeStart(modelPackage, invokerPackage + "\\"), srcBasePath));
        }
        return (outputFolder + File.separator + toSrcPath(modelPackage, srcBasePath));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            // Update the invokerPackage for the default authPackage
            authPackage = invokerPackage + "\\" + authDirName;
        }

        // make auth src path available in mustache template
        additionalProperties.put("authPackage", authPackage);
        additionalProperties.put("authSrcPath", "./" + toSrcPath(authPackage, srcBasePath));

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("composer.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("index.mustache", "", "index.php"));
        supportingFiles.add(new SupportingFile(".htaccess", "", ".htaccess"));
        supportingFiles.add(new SupportingFile("SlimRouter.mustache", toSrcPath(invokerPackage, srcBasePath), "SlimRouter.php"));
        supportingFiles.add(new SupportingFile("phpunit.xml.mustache", "", "phpunit.xml.dist"));
        supportingFiles.add(new SupportingFile("phpcs.xml.mustache", "", "phpcs.xml.dist"));
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        addUserClassnameToOperations(operations);
        escapeMediaType(operationList);
        return objs;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Map<String, Object> apiInfo = (Map<String, Object>) objs.get("apiInfo");
        List<HashMap<String, Object>> apiList = (List<HashMap<String, Object>>) apiInfo.get("apis");
        for (HashMap<String, Object> api : apiList) {
            HashMap<String, Object> operations = (HashMap<String, Object>) api.get("operations");
            List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

            // Sort operations to avoid static routes shadowing
            // ref: https://github.com/nikic/FastRoute/blob/master/src/DataGenerator/RegexBasedAbstract.php#L92-L101
            Collections.sort(operationList, new Comparator<CodegenOperation>() {
                @Override
                public int compare(CodegenOperation one, CodegenOperation another) {
                    if (one.getHasPathParams() && !another.getHasPathParams()) return 1;
                    if (!one.getHasPathParams() && another.getHasPathParams()) return -1;
                    return 0;
                }
            });
        }
        return objs;
    }

    @Override
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> securitySchemeMap) {
        List<CodegenSecurity> codegenSecurities = super.fromSecurity(securitySchemeMap);
        if (Boolean.FALSE.equals(codegenSecurities.isEmpty())) {
            supportingFiles.add(new SupportingFile("abstract_authenticator.mustache", toSrcPath(authPackage, srcBasePath), toAbstractName("Authenticator") + ".php"));
        }
        return codegenSecurities;
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return toAbstractName("DefaultApi");
        }
        return toAbstractName(camelize(name) + "Api");
    }

    @Override
    public String toApiTestFilename(String name) {
        if (name.length() == 0) {
            return "DefaultApiTest";
        }
        return camelize(name) + "ApiTest";
    }

    /**
     * Strips out abstract prefix and suffix from classname and puts it in "userClassname" property of operations object.
     *
     * @param operations codegen object with operations
     */
    private void addUserClassnameToOperations(Map<String, Object> operations) {
        String classname = (String) operations.get("classname");
        classname = classname.replaceAll("^" + abstractNamePrefix, "");
        classname = classname.replaceAll(abstractNameSuffix + "$", "");
        operations.put(USER_CLASSNAME_KEY, classname);
    }

    @Override
    public String encodePath(String input) {
        if (input == null) {
            return input;
        }

        // from DefaultCodegen.java
        // remove \t, \n, \r
        // replace \ with \\
        // replace " with \"
        // outer unescape to retain the original multi-byte characters
        // finally escalate characters avoiding code injection
        input = super.escapeUnsafeCharacters(
                StringEscapeUtils.unescapeJava(
                        StringEscapeUtils.escapeJava(input)
                                .replace("\\/", "/"))
                        .replaceAll("[\\t\\n\\r]", " ")
                        .replace("\\", "\\\\"));
        // .replace("\"", "\\\""));

        // from AbstractPhpCodegen.java
        // Trim the string to avoid leading and trailing spaces.
        input = input.trim();
        try {

            input = URLEncoder.encode(input, "UTF-8")
                    .replaceAll("\\+", "%20")
                    .replaceAll("\\%2F", "/")
                    .replaceAll("\\%7B", "{") // keep { part of complex placeholders
                    .replaceAll("\\%7D", "}") // } part
                    .replaceAll("\\%5B", "[") // [ part
                    .replaceAll("\\%5D", "]") // ] part
                    .replaceAll("\\%3A", ":") // : part
                    .replaceAll("\\%2B", "+") // + part
                    .replaceAll("\\%5C\\%5Cd", "\\\\d"); // \d part
        } catch (UnsupportedEncodingException e) {
            // continue
            LOGGER.error(e.getMessage(), e);
        }
        return input;
    }

    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);
        op.path = encodePath(path);
        return op;
    }

}
