package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Contact;
import io.swagger.models.Info;
import io.swagger.models.License;
import io.swagger.models.Swagger;
import org.apache.commons.lang.StringUtils;

import java.io.File;
import java.util.Map;
import java.util.List;

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
                "name of the project (Default: generated from info.title or \"swagger-clj-client\")"));
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
    public void preprocessSwagger(Swagger swagger) {
        super.preprocessSwagger(swagger);

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

        if (swagger.getInfo() != null) {
            Info info = swagger.getInfo();
            if (projectName == null &&  info.getTitle() != null) {
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
            projectName = "swagger-clj-client";
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

        additionalProperties.put(PROJECT_NAME, projectName);
        additionalProperties.put(PROJECT_DESCRIPTION, escapeText(projectDescription));
        additionalProperties.put(PROJECT_VERSION, projectVersion);
        additionalProperties.put(BASE_NAMESPACE, baseNamespace);
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);

        final String baseNamespaceFolder = sourceFolder + File.separator + namespaceToFolder(baseNamespace);
        supportingFiles.add(new SupportingFile("project.mustache", "", "project.clj"));
        supportingFiles.add(new SupportingFile("core.mustache", baseNamespaceFolder, "core.clj"));
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

        return dashize(sanitizeName(operationId));
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
        name = dashize(name);
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
    public Map<String, Object> postProcessOperations(Map<String, Object> operations) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>) objs.get("operation");
        for (CodegenOperation op : ops) {
            // Convert httpMethod to lower case, e.g. "get", "post"
            op.httpMethod = op.httpMethod.toLowerCase();
        }
        return operations;
    }

    @SuppressWarnings("static-method")
    protected String namespaceToFolder(String ns) {
        return ns.replace(".", File.separator).replace("-", "_");
    }
}
