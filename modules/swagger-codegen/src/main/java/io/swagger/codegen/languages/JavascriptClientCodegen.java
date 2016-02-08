package io.swagger.codegen.languages;

import com.google.common.base.Strings;
import io.swagger.codegen.*;
import io.swagger.models.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class JavascriptClientCodegen extends DefaultCodegen implements CodegenConfig {
    @SuppressWarnings("hiding")
    private static final Logger LOGGER = LoggerFactory.getLogger(JavascriptClientCodegen.class);

    private static final String PROJECT_NAME = "projectName";
    private static final String MODULE_NAME = "moduleName";
    private static final String PROJECT_DESCRIPTION = "projectDescription";
    private static final String PROJECT_VERSION = "projectVersion";
    private static final String PROJECT_LICENSE_NAME = "projectLicenseName";

    protected String projectName;
    protected String moduleName;
    protected String projectDescription;
    protected String projectVersion;

    protected String sourceFolder = "src";
    protected String localVariablePrefix = "";

    public JavascriptClientCodegen() {
        super();
        outputFolder = "generated-code/js";
        modelTemplateFiles.put("model.mustache", ".js");
        apiTemplateFiles.put("api.mustache", ".js");
        templateDir = "Javascript";
        apiPackage = "api";
        modelPackage = "model";

        // reference: http://www.w3schools.com/js/js_reserved.asp
        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "abstract", "arguments", "boolean", "break", "byte",
                        "case", "catch", "char", "class", "const",
                        "continue", "debugger", "default", "delete", "do",
                        "double", "else", "enum", "eval", "export",
                        "extends", "false", "final", "finally", "float",
                        "for", "function", "goto", "if", "implements",
                        "import", "in", "instanceof", "int", "interface",
                        "let", "long", "native", "new", "null",
                        "package", "private", "protected", "public", "return",
                        "short", "static", "super", "switch", "synchronized",
                        "this", "throw", "throws", "transient", "true",
                        "try", "typeof", "var", "void", "volatile",
                        "while", "with", "yield",
                        "Array", "Date", "eval", "function", "hasOwnProperty",
                        "Infinity", "isFinite", "isNaN", "isPrototypeOf",
                        "Math", "NaN", "Number", "Object",
                        "prototype", "String", "toString", "undefined", "valueOf")
        );

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("String", "Boolean", "Integer", "Number", "Array", "Object", "Date", "File")
        );
        defaultIncludes = new HashSet<String>(languageSpecificPrimitives);

        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC).defaultValue("src"));
        cliOptions.add(new CliOption(CodegenConstants.LOCAL_VARIABLE_PREFIX, CodegenConstants.LOCAL_VARIABLE_PREFIX_DESC));
        cliOptions.add(new CliOption(PROJECT_NAME,
                "name of the project (Default: generated from info.title or \"swagger-js-client\")"));
        cliOptions.add(new CliOption(MODULE_NAME,
                "module name for AMD, Node or globals (Default: generated from <projectName>)"));
        cliOptions.add(new CliOption(PROJECT_DESCRIPTION,
                "description of the project (Default: using info.description or \"Client library of <projectName>\")"));
        cliOptions.add(new CliOption(PROJECT_VERSION,
                "version of the project (Default: using info.version or \"1.0.0\")"));
        cliOptions.add(new CliOption(PROJECT_LICENSE_NAME,
                "name of the license the project uses (Default: using info.license.name)"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "javascript";
    }

    @Override
    public String getHelp() {
        return "Generates a Javascript client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        typeMapping = new HashMap<String, String>();
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("map", "Object");
        typeMapping.put("object", "Object");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("char", "String");
        typeMapping.put("string", "String");
        typeMapping.put("short", "Integer");
        typeMapping.put("int", "Integer");
        typeMapping.put("integer", "Integer");
        typeMapping.put("long", "Integer");
        typeMapping.put("float", "Number");
        typeMapping.put("double", "Number");
        typeMapping.put("number", "Number");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("Date", "Date");
        // binary not supported in JavaScript client right now, using String as a workaround
        typeMapping.put("binary", "String");

        importMapping.clear();
    }

    @Override
    public void preprocessSwagger(Swagger swagger) {
        super.preprocessSwagger(swagger);

        if (additionalProperties.containsKey(PROJECT_NAME)) {
            projectName = ((String) additionalProperties.get(PROJECT_NAME));
        }
        if (additionalProperties.containsKey(MODULE_NAME)) {
            moduleName = ((String) additionalProperties.get(MODULE_NAME));
        }
        if (additionalProperties.containsKey(PROJECT_DESCRIPTION)) {
            projectDescription = ((String) additionalProperties.get(PROJECT_DESCRIPTION));
        }
        if (additionalProperties.containsKey(PROJECT_VERSION)) {
            projectVersion = ((String) additionalProperties.get(PROJECT_VERSION));
        }
        if (additionalProperties.containsKey(CodegenConstants.LOCAL_VARIABLE_PREFIX)) {
            localVariablePrefix = (String) additionalProperties.get(CodegenConstants.LOCAL_VARIABLE_PREFIX);
        }
        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            sourceFolder = (String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER);
        }

        if (swagger.getInfo() != null) {
            Info info = swagger.getInfo();
            if (StringUtils.isBlank(projectName) && info.getTitle() != null) {
                // when projectName is not specified, generate it from info.title
                projectName = dashize(info.getTitle());
            }
            if (StringUtils.isBlank(projectVersion)) {
                // when projectVersion is not specified, use info.version
                projectVersion = info.getVersion();
            }
            if (projectDescription == null) {
                // when projectDescription is not specified, use info.description
                projectDescription = info.getDescription();
            }
            if (info.getLicense() != null) {
                License license = info.getLicense();
                if (additionalProperties.get(PROJECT_LICENSE_NAME) == null) {
                    additionalProperties.put(PROJECT_LICENSE_NAME, license.getName());
                }
            }
        }

        // default values
        if (StringUtils.isBlank(projectName)) {
            projectName = "swagger-js-client";
        }
        if (StringUtils.isBlank(moduleName)) {
            moduleName = camelize(underscore(projectName));
        }
        if (StringUtils.isBlank(projectVersion)) {
            projectVersion = "1.0.0";
        }
        if (projectDescription == null) {
            projectDescription = "Client library of " + projectName;
        }

        additionalProperties.put(PROJECT_NAME, projectName);
        additionalProperties.put(MODULE_NAME, moduleName);
        additionalProperties.put(PROJECT_DESCRIPTION, escapeText(projectDescription));
        additionalProperties.put(PROJECT_VERSION, projectVersion);
        additionalProperties.put(CodegenConstants.LOCAL_VARIABLE_PREFIX, localVariablePrefix);
        additionalProperties.put(CodegenConstants.SOURCE_FOLDER, sourceFolder);

        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("index.mustache", sourceFolder, "index.js"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache", sourceFolder, "ApiClient.js"));
    }

    @Override
    public String escapeReservedWord(String name) {
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
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);  // FIXME parameter should not be assigned. Also declare it as "final"

        if("_".equals(name)) {
          name = "_u";
        }

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (reservedWords.contains(name) || name.matches("^\\d.*")) {
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
    public String toModelName(String name) {
        name = sanitizeName(name);  // FIXME parameter should not be assigned. Also declare it as "final"

        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String toModelImport(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiImport(String name) {
        return toApiName(name);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "[" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return "{String: " + getTypeDeclaration(inner) + "}";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof ArrayProperty) {
            return "[]";
        } else if (p instanceof MapProperty) {
            return "{}";
        } else if (p instanceof RefProperty) {
            return "new " + getTypeDeclaration(p) + "()";
        }

        return super.toDefaultValue(p);
    }

    @Override
    public String toDefaultValueWithParam(String name, Property p) {
        if (p instanceof RefProperty) {
            return ".constructFromObject(data['" + name + "']);";
        } else {
          String type = normalizeType(getTypeDeclaration(p));
          return " = ApiClient.convertToType(data['" + name + "'], " + type + ");";
        }
    }

    /**
     * Normalize type by wrapping primitive types with single quotes.
     */
    public String normalizeType(String type) {
      return type.replaceAll("\\b(Boolean|Integer|Number|String|Date)\\b", "'$1'");
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (!needToImport(type)) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        if (null == type) {
            LOGGER.error("No Type defined for Property " + p);
        }
        return toModelName(type);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(operationId)) {
            operationId = escapeReservedWord(operationId);
        }

        return camelize(sanitizeName(operationId), true);
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger) {
      CodegenOperation op = super.fromOperation(path, httpMethod, operation, definitions, swagger);
      if (op.returnType != null) {
        op.returnType = normalizeType(op.returnType);
      }
      return op;
    }

    @Override
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);

        if (allDefinitions != null && codegenModel != null && codegenModel.parent != null && codegenModel.hasEnums) {
            final Model parentModel = allDefinitions.get(toModelName(codegenModel.parent));
            final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent, parentModel);
            codegenModel = JavascriptClientCodegen.reconcileInlineEnums(codegenModel, parentCodegenModel);
        }

        return codegenModel;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            for (CodegenProperty var : cm.vars) {
                Map<String, Object> allowableValues = var.allowableValues;

                // handle ArrayProperty
                if (var.items != null) {
                    allowableValues = var.items.allowableValues;
                }

                if (allowableValues == null) {
                    continue;
                }
                List<String> values = (List<String>) allowableValues.get("values");
                if (values == null) {
                    continue;
                }

                // put "enumVars" map into `allowableValues", including `name` and `value`
                List<Map<String, String>> enumVars = new ArrayList<Map<String, String>>();
                String commonPrefix = findCommonPrefixOfVars(values);
                int truncateIdx = commonPrefix.length();
                for (String value : values) {
                    Map<String, String> enumVar = new HashMap<String, String>();
                    String enumName;
                    if (truncateIdx == 0) {
                        enumName = value;
                    } else {
                        enumName = value.substring(truncateIdx);
                        if ("".equals(enumName)) {
                            enumName = value;
                        }
                    }
                    enumVar.put("name", toEnumVarName(enumName));
                    enumVar.put("value", value);
                    enumVars.add(enumVar);
                }
                allowableValues.put("enumVars", enumVars);
            }
        }
        return objs;
    }

    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type)
            && !languageSpecificPrimitives.contains(type);
    }

    private static String findCommonPrefixOfVars(List<String> vars) {
        String prefix = StringUtils.getCommonPrefix(vars.toArray(new String[vars.size()]));
        // exclude trailing characters that should be part of a valid variable
        // e.g. ["status-on", "status-off"] => "status-" (not "status-o")
        return prefix.replaceAll("[a-zA-Z0-9]+\\z", "");
    }

    private static String toEnumVarName(String value) {
        String var = value.replaceAll("\\W+", "_").toUpperCase();
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    private static CodegenModel reconcileInlineEnums(CodegenModel codegenModel, CodegenModel parentCodegenModel) {
        // This generator uses inline classes to define enums, which breaks when
        // dealing with models that have subTypes. To clean this up, we will analyze
        // the parent and child models, look for enums that match, and remove
        // them from the child models and leave them in the parent.
        // Because the child models extend the parents, the enums will be available via the parent.

        // Only bother with reconciliation if the parent model has enums.
        if (parentCodegenModel.hasEnums) {

            // Get the properties for the parent and child models
            final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
            List<CodegenProperty> codegenProperties = codegenModel.vars;

            // Iterate over all of the parent model properties
            boolean removedChildEnum = false;
            for (CodegenProperty parentModelCodegenPropery : parentModelCodegenProperties) {
                // Look for enums
                if (parentModelCodegenPropery.isEnum) {
                    // Now that we have found an enum in the parent class,
                    // and search the child class for the same enum.
                    Iterator<CodegenProperty> iterator = codegenProperties.iterator();
                    while (iterator.hasNext()) {
                        CodegenProperty codegenProperty = iterator.next();
                        if (codegenProperty.isEnum && codegenProperty.equals(parentModelCodegenPropery)) {
                            // We found an enum in the child class that is
                            // a duplicate of the one in the parent, so remove it.
                            iterator.remove();
                            removedChildEnum = true;
                        }
                    }
                }
            }

            if(removedChildEnum) {
                // If we removed an entry from this model's vars, we need to ensure hasMore is updated
                int count = 0, numVars = codegenProperties.size();
                for(CodegenProperty codegenProperty : codegenProperties) {
                    count += 1;
                    codegenProperty.hasMore = (count < numVars) ? true : null;
                }
                codegenModel.vars = codegenProperties;
            }
        }

        return codegenModel;
    }

    private static String sanitizePackageName(String packageName) { // FIXME parameter should not be assigned. Also declare it as "final"
        packageName = packageName.trim();
        packageName = packageName.replaceAll("[^a-zA-Z0-9_\\.]", "_");
        if(Strings.isNullOrEmpty(packageName)) {
            return "invalidPackageName";
        }
        return packageName;
    }

}
