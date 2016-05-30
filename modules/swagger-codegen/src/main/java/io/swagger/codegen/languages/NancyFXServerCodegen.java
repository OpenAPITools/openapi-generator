package io.swagger.codegen.languages;

import static com.google.common.base.Strings.isNullOrEmpty;
import static io.swagger.codegen.CodegenConstants.OPTIONAL_PROJECT_FILE;
import static io.swagger.codegen.CodegenConstants.OPTIONAL_PROJECT_FILE_DESC;
import static io.swagger.codegen.CodegenConstants.PACKAGE_NAME;
import static io.swagger.codegen.CodegenConstants.PACKAGE_VERSION;
import static io.swagger.codegen.CodegenConstants.RETURN_ICOLLECTION;
import static io.swagger.codegen.CodegenConstants.RETURN_ICOLLECTION_DESC;
import static io.swagger.codegen.CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG;
import static io.swagger.codegen.CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC;
import static io.swagger.codegen.CodegenConstants.SOURCE_FOLDER;
import static io.swagger.codegen.CodegenConstants.SOURCE_FOLDER_DESC;
import static io.swagger.codegen.CodegenConstants.USE_COLLECTION;
import static io.swagger.codegen.CodegenConstants.USE_COLLECTION_DESC;
import static io.swagger.codegen.CodegenConstants.USE_DATETIME_OFFSET;
import static io.swagger.codegen.CodegenConstants.USE_DATETIME_OFFSET_DESC;
import static io.swagger.codegen.CodegenType.SERVER;
import static java.util.Arrays.asList;
import static java.util.UUID.randomUUID;
import static org.apache.commons.lang3.StringUtils.capitalize;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Swagger;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;

import java.io.File;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableMap;

public class NancyFXServerCodegen extends AbstractCSharpCodegen {
    private static final Logger log = LoggerFactory.getLogger(NancyFXServerCodegen.class);

    private static final String API_NAMESPACE = "Modules";
    private static final String MODEL_NAMESPACE = "Models";

    private static final Map<String, Predicate<Property>> propertyToSwaggerTypeMapping =
            createPropertyToSwaggerTypeMapping();

    private final String packageGuid = "{" + randomUUID().toString().toUpperCase() + "}";

    public NancyFXServerCodegen() {
        outputFolder = "generated-code" + File.separator + getName();
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");

        // contextually reserved words
        setReservedWordsLowerCase(
            asList("var", "async", "await", "dynamic", "yield")
        );

        cliOptions.clear();

        // CLI options
        addOption(PACKAGE_NAME, "C# package name (convention: Title.Case).", packageName);
        addOption(PACKAGE_VERSION, "C# package version.", packageVersion);
        addOption(SOURCE_FOLDER, SOURCE_FOLDER_DESC, sourceFolder);

        // CLI Switches
        addSwitch(SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_BY_REQUIRED_FLAG_DESC, sortParamsByRequiredFlag);
        addSwitch(OPTIONAL_PROJECT_FILE, OPTIONAL_PROJECT_FILE_DESC, optionalProjectFileFlag);
        addSwitch(USE_DATETIME_OFFSET, USE_DATETIME_OFFSET_DESC, useDateTimeOffsetFlag);
        addSwitch(USE_COLLECTION, USE_COLLECTION_DESC, useCollection);
        addSwitch(RETURN_ICOLLECTION, RETURN_ICOLLECTION_DESC, returnICollection);
        typeMapping.putAll(nodaTimeTypesMappings());
    }

    @Override
    public CodegenType getTag() {
        return SERVER;
    }

    @Override
    public String getName() {
        return "nancyfx";
    }

    @Override
    public String getHelp() {
        return "Generates a NancyFX Web API server.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        apiPackage = isNullOrEmpty(packageName) ? API_NAMESPACE : packageName +  "." + API_NAMESPACE;
        modelPackage = isNullOrEmpty(packageName) ? MODEL_NAMESPACE : packageName + "." + MODEL_NAMESPACE;

        supportingFiles.add(new SupportingFile("parameters.mustache", sourceFile("Utils"), "Parameters.cs"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", sourceFolder(), "packages.config"));
        supportingFiles.add(new SupportingFile("nuspec.mustache", sourceFolder(), packageName + ".nuspec"));

        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
            supportingFiles.add(new SupportingFile("Project.mustache", sourceFolder(), packageName + ".csproj"));
        }
        additionalProperties.put("packageGuid", packageGuid);
    }

    private String sourceFolder() {
        return "src" + File.separator + packageName;
    }

    private String sourceFile(final String fileName) {
        return sourceFolder() + File.separator + fileName;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder() + File.separator + API_NAMESPACE;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder() + File.separator + MODEL_NAMESPACE;
    }

    @Override
    protected void processOperation(final CodegenOperation operation) {
        super.processOperation(operation);
        if (!isNullOrEmpty(operation.path) && operation.path.contains("?")) {
            operation.path = operation.path.replace("?", "/");
        }
        if (!isNullOrEmpty(operation.httpMethod)) {
            operation.httpMethod = capitalize(operation.httpMethod.toLowerCase());
        }
    }

    @Override
    public String toEnumVarName(final String name, final String datatype) {
        final String enumName = camelize(
                sanitizeName(name)
                .replaceFirst("^_", "")
                .replaceFirst("_$", ""));
        final String result;
        if (enumName.matches("\\d.*")) {
            result = "_" + enumName;
        } else {
            result = enumName;
        }
        log.info(String.format("toEnumVarName('%s', %s) = '%s'", name, datatype, enumName));
        return result;
    }

    @Override
    public String toApiName(final String name) {
        final String apiName;
        if (isNullOrEmpty(name)) {
            apiName = "Default";
        } else {
            apiName = capitalize(name);
        }
        log.info(String.format("toApiName('%s') = '%s'", name, apiName));
        return apiName;
    }

    @Override
    public String toApiFilename(final String name) {
        return super.toApiFilename(name) + "Module";
    }

    @Override
    public void preprocessSwagger(final Swagger swagger) {
        additionalProperties.put("packageContext", sanitizeName(swagger.getBasePath()));
        additionalProperties.put("baseContext", swagger.getBasePath());
    }

    @Override
    public String toEnumName(final CodegenProperty property) {
        return sanitizeName(camelize(property.name)) ;
    }

    @Override
    public String getSwaggerType(final Property property) {
        for (Entry<String, Predicate<Property>> entry : propertyToSwaggerTypeMapping.entrySet()) {
            if (entry.getValue().apply(property)) {
                return entry.getKey();
            }
        }
        return super.getSwaggerType(property);
    }

    private static Map<String, Predicate<Property>> createPropertyToSwaggerTypeMapping() {
        final ImmutableMap.Builder<String, Predicate<Property>> mapping = ImmutableMap.builder();
        mapping.put("time", timeProperty());
        return mapping.build();
    }

    private static Predicate<Property> timeProperty() {
        return new Predicate<Property>() {
            @Override
            public boolean apply(Property property) {
                return property instanceof StringProperty && "time".equalsIgnoreCase(property.getFormat());
            }
        };
    }

    private static Map<String, String> nodaTimeTypesMappings() {
        return ImmutableMap.of(
                "time", "LocalTime?",
                "date", "ZonedDateTime?",
                "datetime", "ZonedDateTime?");
    }
}
