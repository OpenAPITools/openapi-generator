package io.swagger.codegen.languages;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Map;

import static io.swagger.codegen.CodegenConstants.*;
import static io.swagger.codegen.CodegenType.SERVER;
import static java.util.Arrays.asList;
import static java.util.UUID.randomUUID;

public class NancyFXServerCodegen extends AbstractCSharpCodegen {
    private static final Logger log = LoggerFactory.getLogger(NancyFXServerCodegen.class);

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

        apiPackage = packageName + ".Modules";
        modelPackage = packageName + ".Models";

        supportingFiles.add(new SupportingFile("parameters.mustache", sourceFile("Utils"), "Parameters.cs"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", sourceFolder(), "packages.config"));

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
        return outputFolder + File.separator + sourceFolder() + File.separator + "Modules";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder() + File.separator + "Models";
    }

    @Override
    protected void processOperation(CodegenOperation operation) {
        super.processOperation(operation);

        // HACK: Unlikely in the wild, but we need to clean operation paths for MVC Routing
        if (operation.path != null) {
            String original = operation.path;
            operation.path = operation.path.replace("?", "/");
            if (!original.equals(operation.path)) {
                log.warn("Normalized " + original + " to " + operation.path + ". Please verify generated source.");
            }
        }

        // Converts, for example, PUT to HttpPut for controller attributes
        operation.httpMethod = operation.httpMethod.substring(0, 1) + operation.httpMethod.substring(1).toLowerCase();
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        final String enumName = camelize(
                sanitizeName(name)
                .replaceFirst("^_", "")
                .replaceFirst("_$", ""));
        log.info("toEnumVarName = " + enumName);

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultModule";
        }
        return initialCaps(name) + "Module";
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return sanitizeName(camelize(property.name)) ;
    }

    @Override
    public String getSwaggerType(Property property) {
        if (property instanceof StringProperty && "time".equalsIgnoreCase(property.getFormat())) {
            return "time";
        }
        return super.getSwaggerType(property);
    }

    private static Map<String, String> nodaTimeTypesMappings() {
        return ImmutableMap.of(
                "time", "LocalTime?",
                "date", "ZonedDateTime?",
                "datetime", "ZonedDateTime?");
    }
}
