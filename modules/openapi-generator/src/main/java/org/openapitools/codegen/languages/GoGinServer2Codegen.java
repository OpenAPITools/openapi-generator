package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.security.SecurityScheme;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class GoGinServer2Codegen extends AbstractGoCodegen {
    public static final String PROJECT_NAME = "projectName";

    final Logger LOGGER = LoggerFactory.getLogger(GoGinServer2Codegen.class);

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "go-gin-server2";
    }

    public String getHelp() {
        return "Generates a go-gin-server2 server.";
    }

    private final static Map<String, String> commonAbbreviations = Stream.of(new String[][] {
            { "Acl", "ACL" },
            { "Api", "API" },
            { "Ascii", "ASCII" },
            { "Cpu", "CPU" },
            { "Css", "CSS" },
            { "Dns", "DNS" },
            { "Eof", "EOF" },
            { "Guid", "GUID" },
            { "Html", "HTML" },
            { "Http", "HTTP" },
            { "Https", "HTTPS" },
            { "Id", "ID" },
            { "Ip", "IP" },
            { "Json", "JSON" },
            { "Lhs", "LHS" },
            { "Qps", "QPS" },
            { "Ram", "RAM" },
            { "Rhs", "RHS" },
            { "Rpc", "RPC" },
            { "Sla", "SLA" },
            { "Smtp", "SMTP" },
            { "Sql", "SQL" },
            { "Ssh", "SSH" },
            { "Tcp", "TCP" },
            { "Tls", "TLS" },
            { "Ttl", "TTL" },
            { "Udp", "UDP" },
            { "Ui", "UI" },
            { "Uid", "UID" },
            { "Uuid", "UUID" },
            { "Uri", "URI" },
            { "Url", "URL" },
            { "Utf8", "UTF8" },
            { "Vm", "VM" },
            { "Xml", "XML" },
            { "Xmpp", "XMPP" },
            { "Xsrf", "XSRF" },
            { "Xss", "XSS" },
    }).collect(Collectors.toMap(data -> data[0], data -> data[1]));

    protected String path;

    protected String goVersion = "1.20";

    protected int serverPort = 8080;

    protected String ginVersion = "v1.9.1";

    protected Boolean corsFeatureEnabled = false;

    protected String corsLibVersion = "v1.4.0";

    protected Boolean openapiUUIDFeatureEnabled = true;

    protected String openapiUUIDLibVersion = "v0.21.7";

    public GoGinServer2Codegen() {
        super();

        outputFolder = "generated-code" + File.separator + "go-gin-server2";
        modelTemplateFiles.put("model.mustache", ".go");
        apiTemplateFiles.put("api.mustache", ".go");
        embeddedTemplateDir = templateDir = "go-gin-server2";
        apiPackage = "operations";
        modelPackage = "models";
        path = packageName;

        setEnumVarNameToUpperCase(false);
        appendCommonAbbreviations(commonAbbreviations);
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
            path = packageName;
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_PATH)) {
            setPackagePath((String) additionalProperties.get(CodegenConstants.PACKAGE_PATH));
            additionalProperties.put(CodegenConstants.PACKAGE_PATH, packagePath);
            path = String.format("%s/%s", packagePath, packageName);
        }

        if (additionalProperties.containsKey(CodegenConstants.ENUM_CLASS_PREFIX)) {
            setEnumClassPrefix(Boolean.parseBoolean(additionalProperties.get(CodegenConstants.ENUM_CLASS_PREFIX).toString()));
        }

        String varNameToUpperCase = "enumVarNameToUpperCase";
        if (additionalProperties.containsKey(varNameToUpperCase)) {
            setEnumVarNameToUpperCase(Boolean.parseBoolean(additionalProperties.get(varNameToUpperCase).toString()));
        }

        corsFeatureEnabled = isFeatureEnabled("corsFeatureEnabled", corsFeatureEnabled);
        openapiUUIDFeatureEnabled = isFeatureEnabled("openapiUUIDEnabled", openapiUUIDFeatureEnabled);
        goVersion = additionalProperties.getOrDefault("goVersion", goVersion).toString();
        ginVersion = additionalProperties.getOrDefault("ginVersion", ginVersion).toString();
        corsLibVersion = additionalProperties.getOrDefault("corsLibVersion", corsLibVersion).toString();
        openapiUUIDLibVersion = additionalProperties
                .getOrDefault("openapiUUIDLibVersion", openapiUUIDLibVersion)
                .toString();

        additionalProperties.put("goVersion", goVersion);
        additionalProperties.put("ginVersion", ginVersion);
        additionalProperties.put("featureOpenapiUUID", openapiUUIDFeatureEnabled);
        additionalProperties.put("featureCORS", corsFeatureEnabled);
        additionalProperties.put("corsLibVersion", corsLibVersion);
        additionalProperties.put("openapiUUIDLibVersion", openapiUUIDLibVersion);

        if (openapiUUIDFeatureEnabled) {
            typeMapping.put("UUID", "strfmt.UUID");
        }

        typeMapping.put("int64", "int64");

        Map<String, SecurityScheme> securitySchemeMap = openAPI != null ?
                (openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null) : null;
        List<CodegenSecurity> authMethods = fromSecurity(securitySchemeMap);

        if (authMethods != null && !authMethods.isEmpty()) {
            additionalProperties.put("securities", authMethods);
            additionalProperties.put("hasSecurities", true);
        } else {
            additionalProperties.put("hasSecurities", false);
        }

        supportingFiles.add(new SupportingFile("securer.mustache", path, "securer.go"));
        supportingFiles.add(new SupportingFile("main.mustache", "", "main.go"));
        supportingFiles.add(new SupportingFile("go.mod.mustache", "", "go.mod"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));
        supportingFiles.add(new SupportingFile("routers.mustache", path, "routers.go"));
        supportingFiles.add(new SupportingFile("invalid_request_error.mustache", path + "/errorutil", "invalid_request_error.go"));
        supportingFiles.add(new SupportingFile("error_logger.mustache", path + "/errorutil", "error_logger.go"));
        supportingFiles.add(new SupportingFile("error_utils.mustache", path + "/errorutil", "utils.go"));
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationsMap operations = super.postProcessOperationsWithModels(objs, allModels);
        List<Map<String, String>> imports = operations.getImports();
        imports.clear();
        boolean addedTimeImport = imports.contains(createMapping("import", "time"));
        boolean addedUuidImport = imports.contains(createMapping("import", "github.com/go-openapi/strfmt"));

        for (CodegenOperation operation : operations.getOperations().getOperation()) {
            boolean addTimeImport = !addedTimeImport &&
                    (operation.queryParams.stream().anyMatch(p -> requiresImport(p, "time.Time")) ||
                    operation.pathParams.stream().anyMatch(p -> requiresImport(p, "time.Time")));

            if (addTimeImport) {
                imports.add(createMapping("import", "time"));
                addedTimeImport = true;
            }

            boolean addUuidImport = openapiUUIDFeatureEnabled && !addedUuidImport &&
                    (operation.queryParams.stream().anyMatch(p -> requiresImport(p, "strfmt.UUID")) ||
                    operation.pathParams.stream().anyMatch(p -> requiresImport(p, "strfmt.UUID")));

            if (addUuidImport) {
                imports.add(createMapping("import", "github.com/go-openapi/strfmt"));
                addedUuidImport = true;
            }

            if (operation.path != null) {
                operation.path = operation.path.replaceAll("\\{(.*?)\\}", ":$1");
            }
        }

        return objs;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        ModelsMap modelsMap = super.postProcessModels(objs);
        List<Map<String, String>> imports = objs.getImports();
        final String prefix = modelPackage();
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(prefix))
                iterator.remove();
        }

        if (openapiUUIDFeatureEnabled) {
            for (ModelMap m : modelsMap.getModels()) {
                CodegenModel model = m.getModel();

                List<CodegenProperty> inheritedProperties = new ArrayList<>();
                if (model.getComposedSchemas() != null) {
                    if (model.getComposedSchemas().getAllOf() != null) {
                        inheritedProperties.addAll(model.getComposedSchemas().getAllOf());
                    }
                    if (model.getComposedSchemas().getAnyOf() != null) {
                        inheritedProperties.addAll(model.getComposedSchemas().getAnyOf());
                    }
                    if (model.getComposedSchemas().getOneOf() != null) {
                        inheritedProperties.addAll(model.getComposedSchemas().getOneOf());
                    }
                }

                List<CodegenProperty> codegenProperties = new ArrayList<>();
                if(model.getIsModel() || model.getComposedSchemas() == null) {
                    // If the model is a model, use model.vars as it only
                    // contains properties the generated struct will own itself.
                    // If model is no model and it has no composed schemas use
                    // model.vars.
                    codegenProperties.addAll(model.vars);
                } else {
                    // If the model is no model, but is a
                    // allOf, anyOf or oneOf, add all first level options
                    // from allOf, anyOf or oneOf.
                    codegenProperties.addAll(inheritedProperties);
                }

                codegenProperties.addAll(model.allVars);
                boolean strfmtImportAdded = imports.contains(createMapping("import", "github.com/go-openapi/strfmt"));
                boolean timeImportAdded = imports.contains(createMapping("import", "time"));

                for (CodegenProperty cp : codegenProperties) {
                    if (strfmtImportAdded) {
                        break;
                    }

                    if (requiresImport(cp, "strfmt.UUID")) {
                        imports.add(createMapping("import", "github.com/go-openapi/strfmt"));
                        strfmtImportAdded = true;
                    }
                }

                for (CodegenProperty cp : codegenProperties) {
                    if (timeImportAdded) {
                        break;
                    }

                    if (requiresImport(cp, "time.Time")) {
                        imports.add(createMapping("import", "time"));
                        timeImportAdded = true;
                    }
                }
            }
        }

        return modelsMap;
    }

    @Override
    public String toModelName(String name) {
        // underscoring would also lowercase the whole name, thus losing acronyms which are in capitals
        return camelize(toModel(name, false));
    }

    @Override
    public String toModelFilename(String name) {
        name = toModel(name);

        if (isReservedFilename(name)) {
            LOGGER.warn("{}.go with suffix (reserved word) cannot be used as filename. Renamed to {}_.go", name,
                    name);
            name += "_";
        }
        return name;
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        String api = name.replaceAll("-", "_");
        // e.g. PetApi.go => pet_api.go
        api = underscore(api);
        if (isReservedFilename(api)) {
            LOGGER.warn("{}.go with suffix (reserved word) cannot be used as filename. Renamed to {}_.go", name,
                    api);
            api += "_";
        }
        return api;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + path + File.separator + modelPackage;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + path + File.separator + apiPackage;
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        return apiFileFolder() + File.separator + toFolderName(tag) + File.separator + toApiFilename(tag) + suffix;
    }

    private String toFolderName(String tag) {
        String[] words = tag.split("_");
        return String.join("", words).toLowerCase(Locale.ROOT);
    }

    private boolean requiresImport(CodegenParameter parameter, String format) {
        String pointer = "*" + format;
        return parameter.dataType.equals(format) ||
                parameter.dataType.equals(pointer) ||
                (parameter.items != null &&
                        (parameter.items.dataType.equals(format) || parameter.items.dataType.equals(pointer)));
    }

    private boolean requiresImport(CodegenProperty property, String format) {
        String pointer = "*" + format;
        return property.dataType.equals(format) ||
                property.dataType.equals(pointer) ||
                (property.items != null &&
                        (property.items.dataType.equals(format) || property.items.dataType.equals(pointer)));
    }

    private boolean isFeatureEnabled(String feature, Boolean defaultValue) {
        String featureEnabled = additionalProperties
                .getOrDefault(feature, defaultValue)
                .toString();

        return Boolean.parseBoolean(featureEnabled);
    }
}
