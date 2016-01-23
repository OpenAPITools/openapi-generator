package io.swagger.codegen.languages;

import com.google.common.base.CaseFormat;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenResponse;
import io.swagger.codegen.CodegenSecurity;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
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
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class AkkaScalaClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected String mainPackage = "io.swagger.client";
    protected String invokerPackage = mainPackage + ".core";
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-client";
    protected String artifactVersion = "1.0.0";
    protected String sourceFolder = "src/main/scala";
    protected String resourcesFolder = "src/main/resources";
    protected String configKey = "apiRequest";
    protected int defaultTimeoutInMs = 5000;
    protected String configKeyPath = mainPackage;
    protected boolean registerNonStandardStatusCodes = true;
    protected boolean renderJavadoc = true;
    protected boolean removeOAuthSecurities = true;
    /**
     * If set to true, only the default response (the one with le lowest 2XX code) will be considered as a success, and all
     * others as ApiErrors.
     * If set to false, all responses defined in the model will be considered as a success upon reception. Only http errors,
     * unmarshalling problems and any other RuntimeException will be considered as ApiErrors.
     */
    protected boolean onlyOneSuccess = true;
    
    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(AkkaScalaClientCodegen.class);

    public AkkaScalaClientCodegen() {
        super();
        outputFolder = "generated-code/scala";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "akka-scala";
        apiPackage = mainPackage + ".api";
        modelPackage = mainPackage + ".model";

        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "abstract", "case", "catch", "class", "def", "do", "else", "extends",
                        "false", "final", "finally", "for", "forSome", "if", "implicit",
                        "import", "lazy", "match", "new", "null", "object", "override", "package",
                        "private", "protected", "return", "sealed", "super", "this", "throw",
                        "trait", "try", "true", "type", "val", "var", "while", "with", "yield")
        );

        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        additionalProperties.put("configKey", configKey);
        additionalProperties.put("configKeyPath", configKeyPath);
        additionalProperties.put("defaultTimeout", defaultTimeoutInMs);
        if (renderJavadoc) {
            additionalProperties.put("javadocRenderer", new JavadocLambda());
        }
        additionalProperties.put("fnCapitalize", new CapitalizeLambda());
        additionalProperties.put("fnCamelize", new CamelizeLambda(false));
        additionalProperties.put("fnEnumEntry", new EnumEntryLambda());
        additionalProperties.put("onlyOneSuccess", onlyOneSuccess);

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("reference.mustache", resourcesFolder, "reference.conf"));
        final String invokerFolder = (sourceFolder + File.separator + invokerPackage).replace(".", File.separator);
        supportingFiles.add(new SupportingFile("apiRequest.mustache", invokerFolder, "ApiRequest.scala"));
        supportingFiles.add(new SupportingFile("apiInvoker.mustache", invokerFolder, "ApiInvoker.scala"));
        supportingFiles.add(new SupportingFile("requests.mustache", invokerFolder, "requests.scala"));
        supportingFiles.add(new SupportingFile("apiSettings.mustache", invokerFolder, "ApiSettings.scala"));
        final String apiFolder = (sourceFolder + File.separator + apiPackage).replace(".", File.separator);
        supportingFiles.add(new SupportingFile("enumsSerializers.mustache", apiFolder, "EnumsSerializers.scala"));

        importMapping.remove("Seq");
        importMapping.remove("List");
        importMapping.remove("Set");
        importMapping.remove("Map");

        importMapping.put("DateTime", "org.joda.time.DateTime");

        typeMapping = new HashMap<String, String>();
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
        typeMapping.put("long", "Long");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Any");
        typeMapping.put("file", "File");
        typeMapping.put("number", "Double");

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Int",
                        "Long",
                        "Float",
                        "Object",
                        "List",
                        "Seq",
                        "Map")
        );
        instantiationTypes.put("array", "ListBuffer");
        instantiationTypes.put("map", "Map");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "akka-scala";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala client library base on Akka/Spray.";
    }

    @Override
    public String escapeReservedWord(String name) {
        return "`" + name + "`";
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
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
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
        return super.postProcessOperations(objs);
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
    public List<CodegenSecurity> fromSecurity(Map<String, SecuritySchemeDefinition> schemes) {
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
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        return super.toOperationId(CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_CAMEL, operationId));
    }

    private String formatIdentifier(String name, boolean capitalized) {
        String identifier = camelize(name, true);
        if (capitalized) {
            identifier = StringUtils.capitalize(identifier);
        }
        if (identifier.matches("[a-zA-Z_$][\\w_$]+") && !reservedWords.contains(identifier)) {
            return identifier;
        }
        return escapeReservedWord(identifier);
    }

    @Override
    public String toParamName(String name) {
        return formatIdentifier(name, false);
    }

    @Override
    public String toVarName(String name) {
        return formatIdentifier(name, false);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return formatIdentifier(property.baseName, true);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type;
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
        if (!p.getRequired()) {
            return "None";
        }
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
            return "Map[String, " + inner + "].empty ";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            return "Seq[" + inner + "].empty ";
        } else {
            return "null";
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
