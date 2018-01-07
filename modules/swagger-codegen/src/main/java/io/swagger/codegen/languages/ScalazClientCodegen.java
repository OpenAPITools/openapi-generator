package io.swagger.codegen.languages;

import com.google.common.base.CaseFormat;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import io.swagger.codegen.*;

import io.swagger.models.auth.SecuritySchemeDefinition;
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

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ScalazClientCodegen extends AbstractScalaCodegen implements CodegenConfig {

    public ScalazClientCodegen() {
        super();
        outputFolder        = "generated-code/scalaz";
        embeddedTemplateDir = templateDir = "scalaz";
        apiPackage          = "io.swagger.client.api";
        modelPackage        = "io.swagger.client.api";

        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");

        setReservedWordsLowerCase(
            Arrays.asList(
                // local variable names used in API methods (endpoints)
                "path", "contentTypes", "contentType", "queryParams", "headerParams",
                "formParams", "postBody", "mp", "basePath", "apiInvoker",

                // scala reserved words
                "abstract", "case", "catch", "class", "def", "do", "else", "extends",
                "false", "final", "finally", "for", "forSome", "if", "implicit",
                "import", "lazy", "match", "new", "null", "object", "override", "package",
                "private", "protected", "return", "sealed", "super", "this", "throw",
                "trait", "try", "true", "type", "val", "var", "while", "with", "yield")
            );

        additionalProperties.put("apiPackage", apiPackage);

        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        supportingFiles.add(new SupportingFile("dateTimeCodecs.mustache", (sourceFolder + File.separator + apiPackage).replace(".", File.separator), "DateTimeCodecs.scala"));
        supportingFiles.add(new SupportingFile("HelperCodecs.mustache", (sourceFolder + File.separator + apiPackage).replace(".", File.separator), "HelperCodecs.scala"));
        supportingFiles.add(new SupportingFile("QueryParamTypeclass.mustache", (sourceFolder + File.separator + apiPackage).replace(".", File.separator), "QueryParamTypeclass.scala"));

        importMapping.remove("List");
        importMapping.remove("Set");
        importMapping.remove("Map");

        importMapping.put("Date", "java.util.Date");
        importMapping.put("ListBuffer", "scala.collection.mutable.ListBuffer");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("enum", "NSString");
        typeMapping.put("array", "List");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("byte", "Byte");
        typeMapping.put("short", "Short");
        typeMapping.put("char", "Char");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Any");
        typeMapping.put("file", "File");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("date-time", "DateTime");
        typeMapping.put("date", "DateTime");


        //instantiationTypes.put("array", "ListBuffer");
        instantiationTypes.put("array", "ListBuffer");
        instantiationTypes.put("map", "HashMap");

        additionalProperties.put("fnEnumEntry", new EnumEntryLambda());

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue("camelCase"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming((String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }
    }
    
    public void setModelPropertyNaming(String naming) {
        if ("original".equals(naming) || "camelCase".equals(naming) ||
            "PascalCase".equals(naming) || "snake_case".equals(naming)) {
            this.modelPropertyNaming = naming;
        } else {
            throw new IllegalArgumentException("Invalid model property naming '" +
                                               naming + "'. Must be 'original', 'camelCase', " +
                                               "'PascalCase' or 'snake_case'");
        }
    }

    public String getModelPropertyNaming() {
        return this.modelPropertyNaming;
    }
    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
    
        if("_".equals(name)) {
            name = "_u";
        }
    
        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }
    
        name = getNameUsingModelPropertyNaming(name);
    
        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }
    
        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return formatIdentifier(property.baseName, true);
    }

    public String getNameUsingModelPropertyNaming(String name) {
        switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
        case original:    return name;
        case camelCase:   return camelize(name, true);
        case PascalCase:  return camelize(name);
        case snake_case:  return underscore(name);
        default:          throw new IllegalArgumentException("Invalid model property naming '" +
                                                             name + "'. Must be 'original', 'camelCase', " +
                                                             "'PascalCase' or 'snake_case'");
        }

    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "scalaz";
    }

    @Override
    public String getHelp() {
        return "Generates a Scalaz client library (beta) that uses http4s";
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return camelize(operationId, true);
    }

    @Override
    public String toModelName(final String name) {
        final String sanitizedName = sanitizeName(modelNamePrefix + this.stripPackageName(name) + modelNameSuffix);
    
        // camelize the model name
        // phone_number => PhoneNumber
        final String camelizedName = camelize(sanitizedName);
    
        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            final String modelName = "Model" + camelizedName;
            LOGGER.warn(camelizedName + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }
    
        // model name starts with number
        if (name.matches("^\\d.*")) {
            final String modelName = "Model" + camelizedName; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }
    
        return camelizedName;
    }

    private static abstract class CustomLambda implements Mustache.Lambda {
        @Override
        public void execute(Template.Fragment frag, Writer out) throws IOException {
            final StringWriter tempWriter = new StringWriter();
            frag.execute(tempWriter);
            out.write(formatFragment(tempWriter.toString()));
        }

        public abstract String formatFragment(String fragment);
    }

    private class EnumEntryLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            return formatIdentifier(fragment, true);
        }
    }
}
