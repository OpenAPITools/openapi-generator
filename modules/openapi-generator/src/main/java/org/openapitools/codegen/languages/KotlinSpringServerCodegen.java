package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.*;

/**
 * TODO handle "INVOKER_PACKAGE" and "HIDE_GENERATION_TIMESTAMP"
 * TODO integrate Spring Fox
 */
public class KotlinSpringServerCodegen extends AbstractKotlinCodegen {

    private static Logger LOGGER =
            LoggerFactory.getLogger(KotlinSpringServerCodegen.class);

    public static final String TITLE = "title";
    public static final String SERVER_PORT = "serverPort";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String SPRING_BOOT = "spring-boot";

    protected String resourceFolder = "src/main/resources";

    protected String basePackage;
    protected String configPackage;
    protected String serverPort = "8080";
    protected String title = "OpenAPI Kotlin Spring";

    public KotlinSpringServerCodegen() {
        super();

        outputFolder = "generated-code/kotlin-spring";
        apiTestTemplateFiles.clear(); // TODO: add test template
        embeddedTemplateDir = templateDir = "kotlin-spring";

        artifactId = "openapi-spring";
        basePackage = "org.openapitools";
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";
        configPackage = "org.openapitools.conf";

        // spring uses the jackson lib
        additionalProperties.put("jackson", "true");

        addOption(TITLE, "server title name or client service name", title);
        addOption(BASE_PACKAGE, "base package for generated code", basePackage);
        addOption(CONFIG_PACKAGE, "configuration package for generated code", configPackage);
        addOption(SERVER_PORT, "configuration the port in which the sever is to run on", serverPort);
        addOption(CodegenConstants.MODEL_PACKAGE, "model package for generated code", modelPackage);
        addOption(CodegenConstants.API_PACKAGE, "api package for generated code", apiPackage);

        supportedLibraries.put(SPRING_BOOT, "Spring-boot Server application.");
        setLibrary(SPRING_BOOT);

        CliOption cliOpt = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        cliOpt.setDefault(SPRING_BOOT);
        cliOpt.setEnum(supportedLibraries);
        cliOptions.add(cliOpt);

//        modelTemplateFiles.put("model.mustache", ".zz");
//        apiTemplateFiles.put("api.mustache", ".zz");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "kotlin-spring";
    }

    @Override
    public String getHelp() {
        return "Generates a Kotlin Spring application.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (!additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            additionalProperties.put(CodegenConstants.LIBRARY, library);
        }

        if (additionalProperties.containsKey(BASE_PACKAGE)) {
            this.setBasePackage((String) additionalProperties.get(BASE_PACKAGE));
        } else {
            additionalProperties.put(BASE_PACKAGE, basePackage);
        }

        if (additionalProperties.containsKey(SERVER_PORT)) {
            this.setServerPort((String) additionalProperties.get(SERVER_PORT));
        } else {
            additionalProperties.put(SERVER_PORT, serverPort);
        }

        if (additionalProperties.containsKey(CONFIG_PACKAGE)) {
            this.setConfigPackage((String) additionalProperties.get(CONFIG_PACKAGE));
        } else {
            additionalProperties.put(CONFIG_PACKAGE, configPackage);
        }

        supportingFiles.add(new SupportingFile("buildGradleKts.mustache", "", "build.gradle.kts"));
        supportingFiles.add(new SupportingFile("settingsGradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("application.mustache", resourceFolder, "application.yaml"));

        if (library.equals(SPRING_BOOT)) {
            supportingFiles.add(new SupportingFile("openapi2SpringBoot.mustache",
                    sanitizeDirectory(sourceFolder + File.separator + basePackage), "Application.kt"));
        }
    }

    public String getBasePackage() {
        return this.basePackage;
    }

    public void setBasePackage(String basePackage) {
        this.basePackage = basePackage;
    }

    public String getConfigPackage() {
        return this.configPackage;
    }

    public void setConfigPackage(String configPackage) {
        this.configPackage = configPackage;
    }

    public String getServerPort() {
        return this.serverPort;
    }

    public void setServerPort(String serverPort) {
        this.serverPort = serverPort;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        /* TODO the following logic should not need anymore in OAS 3.0
        if ("/".equals(swagger.getBasePath())) {
            swagger.setBasePath("");
        }
        */

        if (!additionalProperties.containsKey(TITLE)) {
            // From the title, compute a reasonable name for the package and the API
            String title = openAPI.getInfo().getTitle();

            // Drop any API suffix
            if (title != null) {
                title = title.trim().replace(" ", "-");
                if (title.toUpperCase().endsWith("API")) {
                    title = title.substring(0, title.length() - 3);
                }

                this.title = camelize(sanitizeName(title), true);
            }
            additionalProperties.put(TITLE, this.title);
        }

        if (!additionalProperties.containsKey(SERVER_PORT)) {
            URL url = URLPathUtils.getServerURL(openAPI);
            this.additionalProperties.put(SERVER_PORT, URLPathUtils.getPort(url, 8080));
        }

        if (openAPI.getPaths() != null) {
            for (String pathname : openAPI.getPaths().keySet()) {
                PathItem path = openAPI.getPaths().get(pathname);
                if (path.readOperations() != null) {
                    for (Operation operation : path.readOperations()) {
                        if (operation.getTags() != null) {
                            List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
                            for (String tag : operation.getTags()) {
                                Map<String, String> value = new HashMap<String, String>();
                                value.put("tag", tag);
                                value.put("hasMore", "true");
                                tags.add(value);
                            }
                            if (tags.size() > 0) {
                                tags.get(tags.size() - 1).remove("hasMore");
                            }
                            if (operation.getTags().size() > 0) {
                                String tag = operation.getTags().get(0);
                                operation.setTags(Arrays.asList(tag));
                            }
                            operation.addExtension("x-tags", tags);
                        }
                    }
                }
            }
        }
    }

    private static String sanitizeDirectory(String in) {
        return in.replace(".", File.separator);
    }
}
