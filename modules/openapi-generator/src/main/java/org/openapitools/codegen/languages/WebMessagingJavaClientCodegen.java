package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class WebMessagingJavaClientCodegen extends PureCloudJavaClientCodegen {
    protected Logger LOGGER = LoggerFactory.getLogger(WebMessagingJavaClientCodegen.class);

    public WebMessagingJavaClientCodegen() {
        super();
    }

    @Override
    public String getName() {
        return "webmessagingjava";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Wipe out all the templates so we can start with a clean slate
        this.apiTemplateFiles.clear();
        this.apiDocTemplateFiles.clear();
        this.apiTestTemplateFiles.clear();
        this.modelTemplateFiles.clear();
        this.modelDocTemplateFiles.clear();
        this.modelTestTemplateFiles.clear();
        this.operationTemplateFiles.clear();
        this.supportingFiles.clear();

        modelTemplateFiles.put("model.mustache", ".java");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiTemplateFiles.put("api.mustache", ".java");
        operationTemplateFiles.put("requestBuilder.mustache", ".java");
        apiDocTemplateFiles.put("api_json.mustache", ".json");

        if (additionalProperties.get(CodegenConstants.ARTIFACT_ID).equals("web-messaging-sdk")) {
            supportingFiles.add(new SupportingFile("pom.xml", "", "pom.xml"));
            supportingFiles.add(new SupportingFile("props.properties.mustache", "", "props.properties"));
        } else {
            supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        }

        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");
        supportingFiles.add(new SupportingFile("WebMessagingClient.mustache", invokerFolder, "WebMessagingClient.java"));
        supportingFiles.add(new SupportingFile("WebMessagingException.mustache", invokerFolder, "WebMessagingException.java"));
        supportingFiles.add(new SupportingFile("GenesysCloudRegionWebSocketHosts.mustache", invokerFolder, "GenesysCloudRegionWebSocketHosts.java"));
        supportingFiles.add(new SupportingFile("ApiDateFormat.mustache", invokerFolder, "ApiDateFormat.java"));
        supportingFiles.add(new SupportingFile("LocalDateDeserializer.mustache", invokerFolder, "LocalDateDeserializer.java"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", invokerFolder, "Configuration.java"));
        supportingFiles.add(new SupportingFile("Pair.mustache", invokerFolder, "Pair.java"));
        supportingFiles.add(new SupportingFile("StringUtil.mustache", invokerFolder, "StringUtil.java"));

        final String authFolder = (sourceFolder + '/' + invokerPackage + ".auth").replace(".", "/");
        supportingFiles.add(new SupportingFile("auth/OAuth.mustache", authFolder, "OAuth.java"));
        supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));

        supportingFiles.add(new SupportingFile("ApiClient.mustache", invokerFolder, "ApiClient.java").doNotOverwrite());
    }

    @Override
    public String getterAndSetterCapitalize(String name) {
        String sanitizedName = name;

        // Special handling to avoid a method conflict named "getClass"
        if ("class".equalsIgnoreCase(name)) {
            sanitizedName = "classProperty";
        }

        return super.getterAndSetterCapitalize(sanitizedName);
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        property.datatypeWithEnum = property.datatypeWithEnum.replace(".", "");
        if (property.complexType != null)
            property.complexType = property.complexType.replace(".", "");

        if (!property.isPrimitiveType) {
            property.datatypeWithEnum = StringUtils.camelize(property.datatypeWithEnum);
            property.dataType = StringUtils.camelize(property.dataType);
        }

        Set<String> imports = new HashSet<>();
        for (String im : model.getImports()) {
            imports.add(camelize(im, false));
        }
        model.imports = imports;
    }
}
