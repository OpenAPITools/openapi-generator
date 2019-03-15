package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ScalaPlayFrameworkServerCodegen extends AbstractScalaCodegen implements CodegenConfig {
    public static final String TITLE = "title";
    public static final String IMPL_STUBS = "implStubs";
    public static final String API_FUTURES = "apiFutures";

    static Logger LOGGER = LoggerFactory.getLogger(ScalaPlayFrameworkServerCodegen.class);

    protected boolean implStubs = true;
    protected boolean apiFutures = false;

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
        typeMapping.put("ArrayByte", "Array[Byte]");

        importMapping.put("OffsetDateTime", "java.time.OffsetDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.remove("BigDecimal");

        addCliOptionWithDefault(IMPL_STUBS, "Whether or not to generate a default implementation class.", implStubs);
        addCliOptionWithDefault(API_FUTURES, "Whether or not to wrap API return types with Futures.", apiFutures);
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

    public void setApiFutures(boolean apiFutures) {
        this.apiFutures = apiFutures;
    }

    public void setImplStubs(boolean implStubs) {
        this.implStubs = implStubs;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(IMPL_STUBS)) {
            this.setImplStubs(convertPropertyToBoolean(IMPL_STUBS));
        }
        writePropertyBack(IMPL_STUBS, implStubs);

        if (additionalProperties.containsKey(API_FUTURES)) {
            this.setApiFutures(convertPropertyToBoolean(API_FUTURES));
        }
        writePropertyBack(API_FUTURES, apiFutures);

        apiTemplateFiles.remove("api.mustache");

        if (implStubs) {
            apiTemplateFiles.put("app/apiImplStubs.scala.mustache", "Impl.scala");
        }

        apiTemplateFiles.put("app/apiTrait.scala.mustache", ".scala");
        apiTemplateFiles.put("app/apiController.scala.mustache", "Controller.scala");

        supportingFiles.add(new SupportingFile("LICENSE.mustache", "", "LICENSE"));
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

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> allModels) {
        Set<String> aliasModels = new HashSet<>();

        for (String modelKey : allModels.keySet()) {
            Map<String, Object> outerModel = (Map<String, Object>) allModels.get(modelKey);
            List<Object> innerModels = (List<Object>) outerModel.get("models");

            for (Object innerModelObj : innerModels) {
                Map<String, Object> innerModel = (Map<String, Object>) innerModelObj;
                CodegenModel model = (CodegenModel) innerModel.get("model");
                boolean isExtensibleObject = "Object".equals(model.dataType) && model.parent != null;
                boolean isCaseObject = !model.hasVars && !model.isArrayModel && !isExtensibleObject;

                if (model.isAlias) {
                    aliasModels.add(model.name);
                    continue;
                }

                fixEnumNames(model);
                makeMultilineComments(model);
                model.classVarName = camelize(model.classVarName, true);
                innerModel.put("isExtensibleObject", isExtensibleObject);
                innerModel.put("isCaseObject", isCaseObject);
                innerModel.put("parentPropName", isExtensibleObject ? "additionalProperties" : model.isArrayModel ? "items" : null);
            }
        }

        // Retain the original map, which uses a custom comparator
        for (String aliasModel : aliasModels) {
            allModels.remove(aliasModel);
        }

        return allModels;
    }

    @SuppressWarnings("unchecked")
    private void fixEnumNames(CodegenModel model) {
        for (CodegenProperty var : model.vars) {
            if (var.isEnum) {
                var.enumName = camelize(var.baseName);
                List<Object> values = (List<Object>) var.allowableValues.get("values");
                List<String> camelizedValues = new ArrayList<>(values.size());

                for (Object value : values) {
                    camelizedValues.add(camelize((String) value));
                }

                var.allowableValues.put("values", camelizedValues);
            }
        }
    }

    private void makeMultilineComments(CodegenModel model) {
        if (model.description != null) {
            model.description = model.description.replaceAll("(.{80}[^\\s]*)\\s*", "$1\n  * ");
        }
    }

    private void addCliOptionWithDefault(String name, String description, boolean defaultValue) {
        cliOptions.add(CliOption.newBoolean(name, description).defaultValue(Boolean.toString(defaultValue)));
    }
}
