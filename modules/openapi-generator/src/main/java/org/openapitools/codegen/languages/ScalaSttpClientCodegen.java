/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ScalaSttpClientCodegen extends AbstractScalaCodegen implements CodegenConfig {
    private static final StringProperty STTP_CLIENT_VERSION = new StringProperty("sttpClientVersion", "The version of " +
            "sttp client", "2.2.0");
    private static final BooleanProperty USE_SEPARATE_ERROR_CHANNEL = new BooleanProperty("separateErrorChannel",
            "Whether to return response as " +
                    "F[Either[ResponseError[ErrorType], ReturnType]]] or to flatten " +
                    "response's error raising them through enclosing monad (F[ReturnType]).", true);
    private static final StringProperty JODA_TIME_VERSION = new StringProperty("jodaTimeVersion", "The version of " +
            "joda-time library", "2.10.6");
    private static final StringProperty JSON4S_VERSION = new StringProperty("json4sVersion", "The version of json4s " +
            "library", "3.6.8");
    private static final StringProperty CIRCE_VERSION = new StringProperty("circeVersion", "The version of circe " +
            "library", "0.13.0");
    private static final JsonLibraryProperty JSON_LIBRARY_PROPERTY = new JsonLibraryProperty();

    private static final String DEFAULT_PACKAGE_NAME = "org.openapitools.client";
    private static final MainPackageProperty MAIN_PACKAGE = new MainPackageProperty();
    private static final StringProperty MODEL_PACKAGE = new StringProperty(CodegenConstants.MODEL_PACKAGE,
            CodegenConstants.MODEL_PACKAGE_DESC, DEFAULT_PACKAGE_NAME + ".model");
    private static final StringProperty INVOKER_PACKAGE = new StringProperty(CodegenConstants.INVOKER_PACKAGE,
            CodegenConstants.MODEL_PACKAGE_DESC, DEFAULT_PACKAGE_NAME + ".core");
    private static final StringProperty API_PACKAGE = new StringProperty(CodegenConstants.API_PACKAGE,
            CodegenConstants.API_PACKAGE_DESC, DEFAULT_PACKAGE_NAME + ".api");

    private static final List<Property> properties = Arrays.asList(STTP_CLIENT_VERSION, USE_SEPARATE_ERROR_CHANNEL,
            JODA_TIME_VERSION, JSON4S_VERSION, CIRCE_VERSION, JSON_LIBRARY_PROPERTY,MAIN_PACKAGE, API_PACKAGE,
            MODEL_PACKAGE, INVOKER_PACKAGE);

    private final Logger LOGGER = LoggerFactory.getLogger(ScalaSttpClientCodegen.class);

    protected String mainPackage = "org.openapitools.client";
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";
    protected boolean registerNonStandardStatusCodes = true;
    protected boolean renderJavadoc = true;
    protected boolean removeOAuthSecurities = true;

    public ScalaSttpClientCodegen() {
        super();
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.BearerToken
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
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        outputFolder = "generated-code/scala-sttp";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "scala-sttp";
        apiPackage = mainPackage + ".api";
        modelPackage = mainPackage + ".model";
        invokerPackage = mainPackage + ".core";

        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        if (renderJavadoc) {
            additionalProperties.put("javadocRenderer", new JavadocLambda());
        }
        additionalProperties.put("fnCapitalize", new CapitalizeLambda());
        additionalProperties.put("fnCamelize", new CamelizeLambda(false));
        additionalProperties.put("fnEnumEntry", new EnumEntryLambda());

        importMapping.remove("Seq");
        importMapping.remove("List");
        importMapping.remove("Set");
        importMapping.remove("Map");

        typeMapping = new HashMap<>();
        typeMapping.put("array", "Seq");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("int", "Int");
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("byte", "Byte");
        typeMapping.put("short", "Short");
        typeMapping.put("char", "Char");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Any");
        typeMapping.put("file", "File");
        typeMapping.put("binary", "File");
        typeMapping.put("number", "Double");

        instantiationTypes.put("array", "ListBuffer");
        instantiationTypes.put("map", "Map");

        properties.forEach(p -> cliOptions.add(p.toCliOption()));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        properties.forEach(p -> p.updateAdditionalProperties(additionalProperties));
        invokerPackage = INVOKER_PACKAGE.getValue(additionalProperties);
        apiPackage = API_PACKAGE.getValue(additionalProperties);
        modelPackage = MODEL_PACKAGE.getValue(additionalProperties);

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        final String invokerFolder = (sourceFolder + File.separator + invokerPackage).replace(".", File.separator);
        supportingFiles.add(new SupportingFile("jsonSupport.mustache", invokerFolder, "JsonSupport.scala"));
        supportingFiles.add(new SupportingFile("project/build.properties.mustache", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("dateSerializers.mustache", invokerFolder, "DateSerializers.scala"));
    }

    @Override
    public String getName() {
        return "scala-sttp";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala client library (beta) based on Sttp.";
    }

    @Override
    public String encodePath(String input) {
        String result = super.encodePath(input);
        return result.replace("{", "${");
    }

    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);
        op.path = encodePath(path);
        return op;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "`" + name + "`";
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        if (registerNonStandardStatusCodes) {
            try {
                @SuppressWarnings("unchecked")
                Map<String, ArrayList<CodegenOperation>> opsMap = (Map<String, ArrayList<CodegenOperation>>) objs.get("operations");
                HashSet<Integer> unknownCodes = new HashSet<Integer>();
                for (CodegenOperation operation : opsMap.get("operation")) {
                    for (CodegenResponse response : operation.responses) {
                        if ("default".equals(response.code)) {
                            continue;
                        }
                        try {
                            int code = Integer.parseInt(response.code);
                            if (code >= 600) {
                                unknownCodes.add(code);
                            }
                        } catch (NumberFormatException e) {
                            LOGGER.error("Status code is not an integer : response.code", e);
                        }
                    }
                }
                if (!unknownCodes.isEmpty()) {
                    additionalProperties.put("unknownStatusCodes", unknownCodes);
                }
            } catch (Exception e) {
                LOGGER.error("Unable to find operations List", e);
            }
        }
        return super.postProcessOperationsWithModels(objs, allModels);
    }

    @Override
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> schemes) {
        final List<CodegenSecurity> codegenSecurities = super.fromSecurity(schemes);
        if (!removeOAuthSecurities) {
            return codegenSecurities;
        }

        // Remove OAuth securities
        Iterator<CodegenSecurity> it = codegenSecurities.iterator();
        while (it.hasNext()) {
            final CodegenSecurity security = it.next();
            if (security.isOAuth) {
                it.remove();
            }
        }
        // Adapt 'hasMore'
        it = codegenSecurities.iterator();
        while (it.hasNext()) {
            final CodegenSecurity security = it.next();
            security.hasMore = it.hasNext();
        }

        if (codegenSecurities.isEmpty()) {
            return null;
        }
        return codegenSecurities;
    }

    @Override
    public String toParamName(String name) {
        return formatIdentifier(name, false);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return formatIdentifier(property.baseName, true);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getRequired() != null && p.getRequired().contains(p.getName())) {
            return "None";
        }

        if (ModelUtils.isBooleanSchema(p)) {
            return null;
        } else if (ModelUtils.isDateSchema(p)) {
            return null;
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return null;
        } else if (ModelUtils.isNumberSchema(p)) {
            return null;
        } else if (ModelUtils.isIntegerSchema(p)) {
            return null;
        } else if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(getAdditionalProperties(p));
            return "Map[String, " + inner + "].empty ";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            if (ModelUtils.isSet(ap)) {
                return "Set[" + inner + "].empty ";
            }
            return "Seq[" + inner + "].empty ";
        } else if (ModelUtils.isStringSchema(p)) {
            return null;
        } else {
            return null;
        }
    }

    private static abstract class Property<T> {
        final String name;
        final String description;
        final T defaultValue;

        private Property(String name, String description, T defaultValue) {
            this.name = name;
            this.description = description;
            this.defaultValue = defaultValue;
        }

        public abstract CliOption toCliOption();

        public abstract void updateAdditionalProperties(Map<String, Object> additionalProperties);

        public abstract T getValue(Map<String, Object> additionalProperties);

        public void setValue(Map<String, Object> additionalProperties, T value) {
            additionalProperties.put(name, value);
        }
    }

    private static class StringProperty extends Property<String> {
        private StringProperty(String name, String description, String defaultValue) {
            super(name, description, defaultValue);
        }

        @Override
        public CliOption toCliOption() {
            return CliOption.newString(name, description).defaultValue(defaultValue);
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            if (!additionalProperties.containsKey(name)) {
                additionalProperties.put(name, defaultValue);
            }
        }

        @Override
        public String getValue(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault(name, defaultValue).toString();
        }
    }

    private static class BooleanProperty extends Property<Boolean> {
        private BooleanProperty(String name, String description, Boolean defaultValue) {
            super(name, description, defaultValue);
        }

        @Override
        public CliOption toCliOption() {
            return CliOption.newBoolean(name, description, defaultValue);
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            Boolean value = getValue(additionalProperties);
            additionalProperties.put(name, value);
        }

        @Override
        public Boolean getValue(Map<String, Object> additionalProperties) {
            return Boolean.valueOf(additionalProperties.getOrDefault(name, defaultValue.toString()).toString());
        }
    }

    private static class JsonLibraryProperty extends StringProperty {
        private static final String JSON4S = "json4s";
        private static final String CIRCE = "circe";

        private JsonLibraryProperty() {
            super("jsonLibrary", "Json library to use. Possible values are: json4s and circe.", JSON4S);
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            String value = getValue(additionalProperties);
            if (value.equals(CIRCE) || value.equals(JSON4S)) {
                additionalProperties.put(CIRCE, value.equals(CIRCE));
                additionalProperties.put(JSON4S, value.equals(JSON4S));
            } else {
                IllegalArgumentException exception =
                        new IllegalArgumentException("Invalid json library: " + value + ". Must be " + CIRCE + " " +
                                "or " + JSON4S);
                throw exception;
            }
        }
    }

    private static class MainPackageProperty extends StringProperty {

        private MainPackageProperty() {
            super("mainPackage", "Top-level package name, which defines 'apiPackage', 'modelPackage', " +
                    "'invokerPackage'", DEFAULT_PACKAGE_NAME);
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            if (additionalProperties.containsKey(name)) {
                String mainPackage = getValue(additionalProperties);
                if (!additionalProperties.containsKey(CodegenConstants.API_PACKAGE)){
                    String apiPackage = mainPackage + ".api";
                    additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
                }
                if (!additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)){
                    String modelPackage = mainPackage + ".model";
                    additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
                }
                if (!additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)){
                    String invokerPackage = mainPackage + ".core";
                    additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
                }
            }
        }
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

    private static class JavadocLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            final String[] lines = fragment.split("\\r?\\n");
            final StringBuilder sb = new StringBuilder();
            sb.append("  /**\n");
            for (String line : lines) {
                sb.append("   * ").append(line).append("\n");
            }
            sb.append("   */\n");
            return sb.toString();
        }
    }

    private static class CapitalizeLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            return StringUtils.capitalize(fragment);
        }
    }

    private static class CamelizeLambda extends CustomLambda {
        private final boolean capitalizeFirst;

        public CamelizeLambda(boolean capitalizeFirst) {
            this.capitalizeFirst = capitalizeFirst;
        }

        @Override
        public String formatFragment(String fragment) {
            return camelize(fragment, !capitalizeFirst);
        }
    }

    private class EnumEntryLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            return formatIdentifier(fragment, true);
        }
    }

}
