package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ScalaPlayFrameworkServerCodegen extends AbstractScalaCodegen implements CodegenConfig {
    public static final String TITLE = "title";
    public static final String SKIP_STUBS = "skipStubs";
    public static final String SUPPORT_ASYNC = "supportAsync";

    static Logger LOGGER = LoggerFactory.getLogger(ScalaPlayFrameworkServerCodegen.class);

    protected boolean skipStubs = false;
    protected boolean supportAsync = false;

    public ScalaPlayFrameworkServerCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "scala-play-framework";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "scala-play-framework";
        hideGenerationTimestamp = false;
        sourceFolder = "app";

        instantiationTypes.put("map", "Map");
        instantiationTypes.put("array", "List");

        typeMapping.put("DateTime", "OffsetDateTime");
        typeMapping.put("Date", "LocalDate");
        typeMapping.put("Integer", "Int");
        typeMapping.put("binary", "Array[Byte]");
        typeMapping.put("ByteArray", "Array[Byte]");
        typeMapping.put("object", "JsObject");

        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.remove("BigDecimal");

        addCliOptionWithDefault(SKIP_STUBS, "If set, skips generation of stub classes.", skipStubs);
        addCliOptionWithDefault(SUPPORT_ASYNC, "Whether or not to wrap API return types with Futures.", supportAsync);
    }

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "scala-play-framework";
    }

    public String getHelp() {
        return "Generates a Scala server application with Play Framework.";
    }

    public void setSupportAsync(boolean supportAsync) {
        this.supportAsync = supportAsync;
    }

    public void setSkipStubs(boolean skipStubs) {
        this.skipStubs = skipStubs;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(SKIP_STUBS)) {
            this.setSkipStubs(convertPropertyToBoolean(SKIP_STUBS));
        }
        writePropertyBack(SKIP_STUBS, skipStubs);

        if (additionalProperties.containsKey(SUPPORT_ASYNC)) {
            this.setSupportAsync(convertPropertyToBoolean(SUPPORT_ASYNC));
        }
        writePropertyBack(SUPPORT_ASYNC, supportAsync);

        apiTemplateFiles.remove("api.mustache");

        if (!skipStubs) {
            apiTemplateFiles.put("app/apiImplStubs.scala.mustache", "Impl.scala");
        }

        apiTemplateFiles.put("app/apiTrait.scala.mustache", ".scala");
        apiTemplateFiles.put("app/apiController.scala.mustache", "Controller.scala");

        supportingFiles.add(new SupportingFile("README.md.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        supportingFiles.add(new SupportingFile("conf/application.conf.mustache", "conf", "application.conf"));
        supportingFiles.add(new SupportingFile("conf/logback.xml.mustache", "conf", "logback.xml"));
        supportingFiles.add(new SupportingFile("project/build.properties.mustache", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("project/plugins.sbt.mustache", "project", "plugins.sbt"));
        supportingFiles.add(new SupportingFile("app/module.scala.mustache", sourceFolder, "Module.scala"));
    }

    @Override
    public String getAlias(String name) {
        if (typeAliases != null && typeAliases.containsKey(name)) {
            return typeAliases.get(name);
        }
        return name;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                Pattern pathVariableMatcher = Pattern.compile("\\{([^}]+)}");
                Matcher match = pathVariableMatcher.matcher(operation.path);
                while (match.find()) {
                    String completeMatch = match.group();
                    String replacement = ":" + camelize(match.group(1), true);
                    operation.path = operation.path.replace(completeMatch, replacement);
                }
            }
        }

        return objs;
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        openAPIType = getAlias(openAPIType);

        // don't apply renaming on types from the typeMapping
        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }

        if (null == openAPIType) {
            LOGGER.error("No Type defined for Schema " + p);
        }
        return toModelName(openAPIType);
    }

    @Override
    public String toDefaultValue(Schema p) {
        // TODO: Check if required...

        if (p.getDefault() != null) {
            return p.getDefault().toString();
        }

        if (ModelUtils.isBooleanSchema(p)) {
            return "false";
        }

        if (ModelUtils.isDateSchema(p)) {
            return "LocalDate.now";
        }

        if (ModelUtils.isDateTimeSchema(p)) {
            return "OffsetDateTime.now";
        }

        if (ModelUtils.isDoubleSchema(p)) {
            return "0.0";
        }

        if (ModelUtils.isFloatSchema(p)) {
            return "0.0F";
        }

        if (ModelUtils.isIntegerSchema(p)) {
            return "0";
        }

        if (ModelUtils.isLongSchema(p)) {
            return "0L";
        }

        if (ModelUtils.isMapSchema(p)) {
            Schema ap = ModelUtils.getAdditionalProperties(p);
            String inner = getSchemaType(ap);
            return "Map.empty[String, " + inner + "]";
        }

        if (ModelUtils.isArraySchema(p)) {
            Schema items = ((ArraySchema)p).getItems();
            String inner = getSchemaType(items);
            return "List.empty[" + inner + "]";
        }

        return "null";
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return StringUtils.camelize(property.name);
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (value.length() == 0) {
            return "EMPTY";
        }

        String var = StringUtils.camelize(value.replaceAll("\\W+", "_"));
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        super.postProcessModels(objs);

        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            boolean isCaseObject = !cm.hasVars && !cm.isArrayModel && !cm.isMapModel;

            postProcessModelsEnum(objs);
            cm.classVarName = camelize(cm.classVarName, true);
            mo.put("isCaseObject", isCaseObject);
            mo.put("parentPropName", cm.isMapModel ? "additionalProperties" : cm.isArrayModel ? "items" : null);
        }

        return objs;
    }

    private void addCliOptionWithDefault(String name, String description, boolean defaultValue) {
        cliOptions.add(CliOption.newBoolean(name, description).defaultValue(Boolean.toString(defaultValue)));
    }
}
