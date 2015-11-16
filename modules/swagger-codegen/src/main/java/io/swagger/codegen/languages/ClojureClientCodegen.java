package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Info;
import io.swagger.models.Swagger;
import org.apache.commons.lang.StringUtils;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.List;

public class ClojureClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final String PROJECT_NAME = "projectName";
    private static final String PROJECT_DESCRIPTION = "projectDescription";
    private static final String PROJECT_VERSION = "projectVersion";
    private static final String BASE_NAMESPACE = "baseNamespace";

    protected String projectName = null;
    protected String projectDescription = null;
    protected String projectVersion = null;
    protected String sourceFolder = "src";

    public ClojureClientCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "clojure";
        apiTemplateFiles.put("api.mustache", ".clj");
        embeddedTemplateDir = templateDir = "clojure";
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

        if (swagger.getInfo() != null) {
            Info info = swagger.getInfo();
            if (projectName == null &&  info.getTitle() != null) {
                // when projectName is not specified, generate it from info.title
                projectName = dashize(info.getTitle());
            }
            if (projectVersion == null && info.getVersion() != null) {
                // when projectVersion is not specified, use info.version
                projectVersion = info.getVersion();
            }
            if (projectDescription == null && info.getDescription() != null) {
                // when projectDescription is not specified, use info.description
                projectDescription = info.getDescription();
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

        final String baseNamespace = dashize(projectName);
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
        name = name.replaceAll("[^a-zA-Z0-9_-]+", "");
        name = dashize(name);
        return name;
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

    protected String namespaceToFolder(String ns) {
        return ns.replace(".", File.separator).replace("-", "_");
    }

    protected String dashize(String s) {
        return underscore(s).replaceAll("[_ ]", "-");
    }
}
