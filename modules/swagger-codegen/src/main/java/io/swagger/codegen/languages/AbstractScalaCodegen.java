package io.swagger.codegen.languages;

import java.io.File;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;
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
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            return getSwaggerType(p) + "[String, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String toInstantiationType(Property p) {
        if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            String inner = getSwaggerType(ap.getAdditionalProperties());
            return instantiationTypes.get("map") + "[String, " + inner + "]";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            return instantiationTypes.get("array") + "[" + inner + "]";
        } else {
            return null;
        }
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            return "null";
        } else if (p instanceof BooleanProperty) {
            return "null";
        } else if (p instanceof DateProperty) {
            return "null";
        } else if (p instanceof DateTimeProperty) {
            return "null";
        } else if (p instanceof DoubleProperty) {
            return "null";
        } else if (p instanceof FloatProperty) {
            return "null";
        } else if (p instanceof IntegerProperty) {
            return "null";
        } else if (p instanceof LongProperty) {
            return "null";
        } else if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            String inner = getSwaggerType(ap.getAdditionalProperties());
            return "new HashMap[String, " + inner + "]() ";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            return "new ListBuffer[" + inner + "]() ";
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