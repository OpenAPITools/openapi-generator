package io.swagger.codegen.languages;

import java.io.File;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;

public abstract class AbstractScalaCodegen extends DefaultCodegen {

    protected String modelPropertyNaming = "camelCase";
    protected String invokerPackage = "io.swagger.client";
    protected String sourceFolder = "src/main/scala";

    public AbstractScalaCodegen() {
        super();

        languageSpecificPrimitives.addAll(Arrays.asList(
                "String",
                "boolean",
                "Boolean",
                "Double",
                "Int",
                "Long",
                "Float",
                "Object",
                "Any",
                "List",
                "Seq",
                "Map"));

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public String getSourceFolder() {
        return sourceFolder;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Schema propertySchema) {
        if (propertySchema instanceof ArraySchema) {
            Schema inner = ((ArraySchema) propertySchema).getItems();
            return String.format("%s[%s]", getSchemaType(propertySchema), getTypeDeclaration(inner));
        } else if (propertySchema instanceof MapSchema) {
            Schema inner = propertySchema.getAdditionalProperties();
            return String.format("%s[String, %s]", getSchemaType(propertySchema), getTypeDeclaration(inner));
        }
        return super.getTypeDeclaration(propertySchema);
    }

    @Override
    public String getSchemaType(Schema propertySchema) {
        String swaggerType = super.getSchemaType(propertySchema);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type))
                return (type);
        } else
            type = swaggerType;
        return type;
    }

    @Override
    public String toInstantiationType(Schema schemaProperty) {
        if (schemaProperty instanceof MapSchema) {
            String inner = getSchemaType(schemaProperty.getAdditionalProperties());
            return String.format("%s[%s]", instantiationTypes.get("map"), inner);
        } else if (schemaProperty instanceof ArraySchema) {
            ArraySchema arraySchema = (ArraySchema) schemaProperty;
            String inner = getSchemaType(arraySchema.getItems());
            return String.format("%s[%s]", instantiationTypes.get("array"), inner);
        } else {
            return null;
        }
    }

    @Override
    public String toDefaultValue(Schema propertySchema) {
        if (propertySchema instanceof MapSchema) {
            String inner = getSchemaType(propertySchema.getAdditionalProperties());
            return String.format("new HashMap[String, %s]()", inner);
        } else if(propertySchema instanceof ArraySchema) {
            ArraySchema arraySchema = (ArraySchema) propertySchema;
            String inner = getSchemaType(arraySchema.getItems());
            return String.format("new ListBuffer[%s]()", inner);
        } else {
            return "null";
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // remove model imports to avoid warnings for importing class in the same package in Scala
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        final String prefix = modelPackage() + ".";
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(prefix)) iterator.remove();
        }
        return objs;
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    protected String formatIdentifier(String name, boolean capitalized) {
        String identifier = camelize(sanitizeName(name), true);
        if (capitalized) {
            identifier = StringUtils.capitalize(identifier);
        }
        if (identifier.matches("[a-zA-Z_$][\\w_$]+") && !isReservedWord(identifier)) {
            return identifier;
        }
        return escapeReservedWord(identifier);
    }

    protected String stripPackageName(String input) {
        if (StringUtils.isEmpty(input) || input.lastIndexOf(".") < 0)
            return input;

        int lastIndexOfDot = input.lastIndexOf(".");
        return input.substring(lastIndexOfDot + 1);
    }
}