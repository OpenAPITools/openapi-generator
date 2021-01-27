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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.OnceLogger.once;
import static org.openapitools.codegen.utils.StringUtils.dashize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class ClojureClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ClojureClientCodegen.class);
    private static final String PROJECT_NAME = "projectName";
    private static final String PROJECT_DESCRIPTION = "projectDescription";
    private static final String PROJECT_VERSION = "projectVersion";
    private static final String PROJECT_URL = "projectUrl";
    private static final String PROJECT_LICENSE_NAME = "projectLicenseName";
    private static final String PROJECT_LICENSE_URL = "projectLicenseUrl";
    private static final String BASE_NAMESPACE = "baseNamespace";

    static final String VENDOR_EXTENSION_X_BASE_SPEC = "x-base-spec";
    static final String X_MODELS = "x-models";

    protected String projectName;
    protected String projectDescription;
    protected String projectVersion;
    protected String baseNamespace;
    protected Set<String> baseSpecs;
    protected Set<String> models = new HashSet<>();

    protected String sourceFolder = "src";

    public ClojureClientCodegen() {
        super();

        // TODO: Clojure maintainer review
        modifyFeatureSet(features -> features
                .excludeDocumentationFeatures(
                        DocumentationFeature.Readme
                )
                .securityFeatures(EnumSet.of(
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey
                ))
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.Union
                )
        );

        outputFolder = "generated-code" + File.separator + "clojure";
        modelTemplateFiles.put("spec.mustache", ".clj");
        apiTemplateFiles.put("api.mustache", ".clj");
        embeddedTemplateDir = templateDir = "clojure";

        cliOptions.add(new CliOption(PROJECT_NAME,
                "name of the project (Default: generated from info.title or \"openapi-clj-client\")"));
        cliOptions.add(new CliOption(PROJECT_DESCRIPTION,
                "description of the project (Default: using info.description or \"Client library of <projectName>\")"));
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

        typeMapping.clear();

        // We have specs for most of the types:
        typeMapping.put("integer", "int?");
        typeMapping.put("long", "int?");
        typeMapping.put("short", "int?");
        typeMapping.put("number", "float?");
        typeMapping.put("float", "float?");
        typeMapping.put("double", "float?");
        typeMapping.put("array", "list?");
        typeMapping.put("map", "map?");
        typeMapping.put("boolean", "boolean?");
        typeMapping.put("string", "string?");
        typeMapping.put("char", "char?");
        typeMapping.put("date", "inst?");
        typeMapping.put("DateTime", "inst?");
        typeMapping.put("UUID", "uuid?");
        typeMapping.put("URI", "string?");

        // But some type mappings are not really worth/meaningful to check for:
        typeMapping.put("object", "any?"); // Like, everything is an object.
        typeMapping.put("file", "any?");   // We don't really have specs for files,
        typeMapping.put("binary", "any?"); // nor binary.
        // And while there is a way to easily check if something is a bytearray,
        // (https://stackoverflow.com/questions/14796964/), it's not possible
        // to conform it yet, so we leave it as is.
        typeMapping.put("ByteArray", "any?");

        // Set of base specs that don't need to be imported
        baseSpecs = new HashSet<>(
                Arrays.asList(
                        "int?",
                        "float?",
                        "list?",
                        "map?",
                        "boolean?",
                        "string?",
                        "char?",
                        "inst?",
                        "uuid?",
                        "any?"
                )
        );
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
    public String getTypeDeclaration(Schema p) {
        if (p instanceof ArraySchema) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();

            return "(s/coll-of " + getTypeDeclaration(inner) + ")";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = (Schema) p.getAdditionalProperties();

            return "(s/map-of string? " + getTypeDeclaration(inner) + ")";
        }

        // If it's a type we defined, we want to append the spec suffix
        if (!typeMapping.containsKey(super.getSchemaType(p))) {
            return super.getTypeDeclaration(p) + "-spec";
        } else {
            return super.getTypeDeclaration(p);
        }
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);

        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        } else {
            return toModelName(openAPIType);
        }
    }

    @Override
    public String toModelName(String name) {
        return dashize(name);
    }

    @Override
    public CodegenModel fromModel(String name, Schema mod) {
        CodegenModel model = super.fromModel(name, mod);
        // If a var is a base spec we won't need to import it
        for (CodegenProperty var : model.vars) {
            var.vendorExtensions.put(VENDOR_EXTENSION_X_BASE_SPEC, baseSpecs.contains(var.complexType));
            if (var.items != null) {
                var.items.vendorExtensions.put(VENDOR_EXTENSION_X_BASE_SPEC, baseSpecs.contains(var.items.complexType));
            }
        }

        // We also add all models to our model list so we can import them e.g. in operations
        models.add(model.classname);

        return model;
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
                projectName = dashize(info.getTitle());
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
            baseNamespace = dashize(projectName);
        }
        apiPackage = baseNamespace + ".api";
        modelPackage = baseNamespace + ".specs";

        additionalProperties.put(PROJECT_NAME, projectName);
        additionalProperties.put(PROJECT_DESCRIPTION, escapeText(projectDescription));
        additionalProperties.put(PROJECT_VERSION, projectVersion);
        additionalProperties.put(BASE_NAMESPACE, baseNamespace);
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);

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
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + namespaceToFolder(modelPackage);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");
        }

        return dashize(sanitizeName(operationId));
    }

    @Override
    public String toApiFilename(String name) {
        return underscore(toApiName(name));
    }

    @Override
    public String toModelFilename(String name) {
        return underscore(toModelName(name));
    }

    @Override
    public String toApiName(String name) {
        return dashize(name);
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toVarName(String name) {
        name = name.replaceAll("[^a-zA-Z0-9_-]+", ""); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
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

            op.vendorExtensions.put(X_MODELS, models);
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
