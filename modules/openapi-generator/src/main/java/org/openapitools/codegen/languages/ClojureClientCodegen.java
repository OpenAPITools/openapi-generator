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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;

import java.io.File;
import java.util.List;
import java.util.Locale;
import java.util.Map;


public class ClojureClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final String PROJECT_NAME = "projectName";
    private static final String PROJECT_DESCRIPTION = "projectDescription";
    private static final String PROJECT_VERSION = "projectVersion";
    private static final String PROJECT_URL = "projectUrl";
    private static final String PROJECT_LICENSE_NAME = "projectLicenseName";
    private static final String PROJECT_LICENSE_URL = "projectLicenseUrl";
    private static final String BASE_NAMESPACE = "baseNamespace";

    protected String projectName;
    protected String projectDescription;
    protected String projectVersion;
    protected String baseNamespace;

    protected String sourceFolder = "src";

    public ClojureClientCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "clojure";
        apiTemplateFiles.put("api.mustache", ".clj");
        embeddedTemplateDir = templateDir = "clojure";

        cliOptions.add(new CliOption(PROJECT_NAME,
                "name of the project (Default: generated from info.title or \"openapi-clj-client\")"));
        cliOptions.add(new CliOption(PROJECT_DESCRIPTION,
                "description of the project (Default: using info.description or \"Client library of <projectNname>\")"));
        cliOptions.add(new CliOption(PROJECT_VERSION,
                "version of the project (Default: using info.version or \"1.0.0\")"));
        cliOptions.add(new CliOption(PROJECT_URL,
                "URL of the project (Default: using info.contact.url or not included in project.clj)"));
        cliOptions.add(new CliOption(PROJECT_LICENSE_NAME,
                "name of the license the project uses (Default: using info.license.name or not included in project.clj)"));
        cliOptions.add(new CliOption(PROJECT_LICENSE_URL,
                "URL of the license the project uses (Default: using info.license.url or not included in project.clj)"));
        cliOptions.add(new CliOption(BASE_NAMESPACE,
                "the base/top namespace (Default: generated from projectName)"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "clojure";
    }

    @Override
    public String getHelp() {
        return "Generates a Clojure client library.";
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        if (additionalProperties.containsKey(PROJECT_NAME)) {
            projectName = ((String) additionalProperties.get(PROJECT_NAME));
        }
        if (additionalProperties.containsKey(PROJECT_DESCRIPTION)) {
            projectDescription = ((String) additionalProperties.get(PROJECT_DESCRIPTION));
        }
        if (additionalProperties.containsKey(PROJECT_VERSION)) {
            projectVersion = ((String) additionalProperties.get(PROJECT_VERSION));
        }
        if (additionalProperties.containsKey(BASE_NAMESPACE)) {
            baseNamespace = ((String) additionalProperties.get(BASE_NAMESPACE));
        }

        if (openAPI.getInfo() != null) {
            Info info = openAPI.getInfo();
            if (projectName == null && info.getTitle() != null) {
                // when projectName is not specified, generate it from info.title
                projectName = org.openapitools.codegen.utils.StringUtils.dashize(info.getTitle());
            }
            if (projectVersion == null) {
                // when projectVersion is not specified, use info.version
                projectVersion = info.getVersion();
            }
            if (projectDescription == null) {
                // when projectDescription is not specified, use info.description
                projectDescription = info.getDescription();
            }

            if (info.getContact() != null) {
                Contact contact = info.getContact();
                if (additionalProperties.get(PROJECT_URL) == null) {
                    additionalProperties.put(PROJECT_URL, contact.getUrl());
                }
            }
            if (info.getLicense() != null) {
                License license = info.getLicense();
                if (additionalProperties.get(PROJECT_LICENSE_NAME) == null) {
                    additionalProperties.put(PROJECT_LICENSE_NAME, license.getName());
                }
                if (additionalProperties.get(PROJECT_LICENSE_URL) == null) {
                    additionalProperties.put(PROJECT_LICENSE_URL, license.getUrl());
                }
            }
        }

        // default values
        if (projectName == null) {
            projectName = "openapi-clj-client";
        }
        if (projectVersion == null) {
            projectVersion = "1.0.0";
        }
        if (projectDescription == null) {
            projectDescription = "Client library of " + projectName;
        }
        if (baseNamespace == null) {
            baseNamespace = org.openapitools.codegen.utils.StringUtils.dashize(projectName);
        }
        apiPackage = baseNamespace + ".api";

        additionalProperties.put(PROJECT_NAME, projectName);
        additionalProperties.put(PROJECT_DESCRIPTION, escapeText(projectDescription));
        additionalProperties.put(PROJECT_VERSION, projectVersion);
        additionalProperties.put(BASE_NAMESPACE, baseNamespace);
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);

        final String baseNamespaceFolder = sourceFolder + File.separator + namespaceToFolder(baseNamespace);
        supportingFiles.add(new SupportingFile("project.mustache", "", "project.clj"));
        supportingFiles.add(new SupportingFile("core.mustache", baseNamespaceFolder, "core.clj"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
    }

    @Override
    public String sanitizeTag(String tag) {
        return tag.replaceAll("[^a-zA-Z_]+", "_");
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + namespaceToFolder(apiPackage);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");
        }

        return org.openapitools.codegen.utils.StringUtils.dashize(sanitizeName(operationId));
    }

    @Override
    public String toApiFilename(String name) {
        return org.openapitools.codegen.utils.StringUtils.underscore(toApiName(name));
    }

    @Override
    public String toApiName(String name) {
        return org.openapitools.codegen.utils.StringUtils.dashize(name);
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toVarName(String name) {
        name = name.replaceAll("[^a-zA-Z0-9_-]+", ""); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        name = org.openapitools.codegen.utils.StringUtils.dashize(name);
        return name;
    }

    @Override
    public String escapeText(String input) {
        if (input == null) {
            return null;
        }
        return input.trim().replace("\\", "\\\\").replace("\"", "\\\"");
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> allModels) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>) objs.get("operation");
        for (CodegenOperation op : ops) {
            // Convert httpMethod to lower case, e.g. "get", "post"
            op.httpMethod = op.httpMethod.toLowerCase(Locale.ROOT);
        }
        return operations;
    }

    @SuppressWarnings("static-method")
    protected String namespaceToFolder(String ns) {
        return ns.replace(".", File.separator).replace("-", "_");
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // ref: https://clojurebridge.github.io/community-docs/docs/clojure/comment/
        return input.replace("(comment", "(_comment");
    }
}
