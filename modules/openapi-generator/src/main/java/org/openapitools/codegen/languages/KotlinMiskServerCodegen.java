package org.openapitools.codegen.languages;

import lombok.Setter;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class KotlinMiskServerCodegen extends AbstractKotlinCodegen {

    protected String rootPackage = "org.openapitools.server.api";
    protected String apiVersion = "1.0.0-SNAPSHOT";
    @Setter protected String moduleClassName = "OpenApiModule";
    @Setter protected String moduleFileName = moduleClassName + ".kt";

    public static final String ROOT_PACKAGE = "rootPackage";
    public static final String PROJECT_NAME = "projectName";
    public static final String MODULE_CLASS_NAME = "moduleClassName";
    public static final String MODULE_FILE_NAME = "moduleFileName";

    final Logger LOGGER = LoggerFactory.getLogger(KotlinMiskServerCodegen.class);

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "kotlin-misk";
    }

    @Override
    public String getHelp() {
        return "Generates a kotlin-misk server.";
    }

    public KotlinMiskServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.PROTOBUF))
                .securityFeatures(EnumSet.noneOf(
                        SecurityFeature.class
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
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        embeddedTemplateDir = templateDir = "kotlin-misk";

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

        outputFolder = "generated-code" + File.separator + "kotlin-misk";

        // Add CLI options for module customization
        addOption(MODULE_CLASS_NAME, "Name of the generated module class", moduleClassName);
        addOption(MODULE_FILE_NAME, "Name of the generated module file", moduleFileName);

        apiTestTemplateFiles.clear();
        apiTestTemplateFiles.put("api_test.mustache", ".kt");

        apiDocTemplateFiles.clear();
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        modelDocTemplateFiles.clear();
        modelDocTemplateFiles.put("model_doc.mustache", ".md");

        supportingFiles.clear();

        apiTemplateFiles.clear();
        apiTemplateFiles.put("apiController.mustache", "Controller.kt");
        apiTemplateFiles.put("apiImpl.mustache", "Impl.kt");
        apiTemplateFiles.put("apiInterface.mustache", ".kt");
        modelTemplateFiles.put("model.mustache", ".kt");

        apiPackage = rootPackage + ".api";
        modelPackage = rootPackage + ".model";
        artifactId = "openapi-kotlin-misk-server";
        artifactVersion = apiVersion;

        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        additionalProperties.put(ROOT_PACKAGE, rootPackage);

        // Add supporting files
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.gradle.kts.mustache", "", "build.gradle.kts"));
        supportingFiles.add(new SupportingFile("settings.gradle.kts.mustache", "", "settings.gradle.kts"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(MODULE_CLASS_NAME)) {
            setModuleClassName((String) additionalProperties.get(MODULE_CLASS_NAME));
        }
        additionalProperties.put(MODULE_CLASS_NAME, moduleClassName);

        if (additionalProperties.containsKey(MODULE_FILE_NAME)) {
            setModuleFileName((String) additionalProperties.get(MODULE_FILE_NAME));
        }
        additionalProperties.put(MODULE_FILE_NAME, moduleFileName);

        String apiModuleFolder = (sourceFolder + File.separator + apiPackage).replace(".", File.separator);
        supportingFiles.add(new SupportingFile("miskModule.mustache", apiModuleFolder, moduleFileName));
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap objectMap = objs.getOperations();
        List<CodegenOperation> operations = objectMap.getOperation();

        for (CodegenOperation operation : operations) {

            if (operation.hasConsumes) {
                for (Map<String, String> consumes : operation.consumes) {
                    consumes.computeIfPresent("mediaType", (__, mediaType) -> mapMediaType(mediaType));
                }
            }

            if (operation.hasProduces) {
                for (Map<String, String> produces : operation.produces) {
                    produces.computeIfPresent("mediaType", (__, mediaType) -> mapMediaType(mediaType));
                }
            }

            // http method verb conversion (e.g. PUT => Put)
            operation.httpMethod = camelize(operation.httpMethod.toLowerCase(Locale.ROOT));
        }

        return objs;
    }

    private String mapMediaType(String mediaType) {
        return MEDIA_MAPPING.get(mediaType);
    }

    private final static Map<String, String> MEDIA_MAPPING = getMappings();

    private static Map<String, String> getMappings() {
        Map<String, String> result = new HashMap<>();
        // @todo add others as needed
        result.put("application/json", "MediaTypes.APPLICATION_JSON");
        result.put("application/xml", "MediaTypes.APPLICATION_XML");
        return result;
    }
}