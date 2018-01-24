package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.*;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.parameters.QueryParameter;
import io.swagger.models.properties.*;


import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ZendExpressivePathHandlerServerCodegen extends AbstractPhpCodegen {

    public static final String VEN_FROM_QUERY = "internal.ze-ph.fromQuery";
    public static final String VEN_COLLECTION_FORMAT = "internal.ze-ph.collectionFormat";
    public static final String VEN_QUERY_DATA_TYPE = "internal.ze-ph.queryDataType";
    public static final String VEN_HAS_QUERY_DATA = "internal.ze-ph.hasQueryData";

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "ze-ph";
    }

    @Override
    public String getHelp() {
        return "Generates PHP server stub using Zend Expressive ( https://zendframework.github.io/zend-expressive ) and Path Handler ( https://github.com/Articus/PathHandler ).";
    }

    public ZendExpressivePathHandlerServerCodegen() {
        super();
        //no point to use double - http://php.net/manual/en/language.types.float.php , especially because of PHP 7+ float type declaration
        typeMapping.put("double", "float");

        embeddedTemplateDir = templateDir = "ze-ph";
        invokerPackage = "App";
        packagePath = "";
        srcBasePath = "src" + File.separator + "App";
        apiDirName = "Handler";
        modelDirName = "DTO";
        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;

        apiTestTemplateFiles.clear();
        modelTestTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        modelDocTemplateFiles.clear();

        supportingFiles.add(new SupportingFile("README.md.mustache", packagePath, "README.md"));
        supportingFiles.add(new SupportingFile("composer.json.mustache", packagePath, "composer.json"));
        supportingFiles.add(new SupportingFile("index.php", packagePath + File.separator + "public", "index.php"));
        supportingFiles.add(new SupportingFile("container.php", packagePath + File.separator + "application", "container.php"));
        supportingFiles.add(new SupportingFile("config.yml", packagePath + File.separator + "application", "config.yml"));
        supportingFiles.add(new SupportingFile("app.yml.mustache", packagePath + File.separator + "application" + File.separator + "config", "app.yml"));
        supportingFiles.add(new SupportingFile("path_handler.yml.mustache", packagePath + File.separator + "application" + File.separator + "config", "path_handler.yml"));
        supportingFiles.add(new SupportingFile("data_transfer.yml.mustache", packagePath + File.separator + "application" + File.separator + "config", "data_transfer.yml"));
        supportingFiles.add(new SupportingFile("ErrorMiddleware.php.mustache", packagePath + File.separator + srcBasePath, "ErrorMiddleware.php"));
        supportingFiles.add(new SupportingFile("Date.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "Date.php"));
        supportingFiles.add(new SupportingFile("DateTime.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "DateTime.php"));
        supportingFiles.add(new SupportingFile("QueryParameter.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "QueryParameter.php"));
        supportingFiles.add(new SupportingFile("QueryParameterArray.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "QueryParameterArray.php"));
        supportingFiles.add(new SupportingFile("Type.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Validator", "Type.php"));
        supportingFiles.add(new SupportingFile("QueryParameterType.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Validator", "QueryParameterType.php"));
        supportingFiles.add(new SupportingFile("QueryParameterArrayType.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Validator", "QueryParameterArrayType.php"));

        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, "1.0.0");
    }

    /**
     * Add operation to group
     * Override of default grouping - group by resource path, not tag
     *
     * @param tag          name of the tag
     * @param resourcePath path of the resource
     * @param operation    Swagger Operation object
     * @param co           Codegen Operation object
     * @param operations   map of Codegen operations
     */
    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(resourcePath);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(resourcePath, opList);
        }
        //ignore duplicate operation ids - that means that operation has several tags
        int counter = 0;
        for (CodegenOperation op : opList) {
            if (co.operationId.equals(op.operationId)) {
                counter++;
            }
        }
        if (counter == 0) {
            co.operationIdLowerCase = co.operationId.toLowerCase();
            opList.add(co);
            co.baseName = tag;
        }
    }

    /**
     * Return the file name of the Api Test
     *
     * @param name the file name of the Api
     * @return the file name of the Api
     */
    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    /**
     * Output the API (class) name (capitalized) ending with "Api"
     * Return DefaultApi if name is empty
     *
     * @param name the name of the Api
     * @return capitalized Api name ending with "Api"
     */
    @Override
    public String toApiName(String name) {
        //Remove }
        name = name.replaceAll("[\\}]", "");
        return super.toModelName(name);
    }

    /**
     * Generate additional model definitions from query parameters
     *
     * @param swagger
     */
    @Override
    public void preprocessSwagger(Swagger swagger) {
        super.preprocessSwagger(swagger);
        for (String pathKey : swagger.getPaths().keySet())
        {
            Path path = swagger.getPath(pathKey);
            Map<HttpMethod, Operation> operations = path.getOperationMap();
            for (HttpMethod method : operations.keySet())
            {
                Operation operation = operations.get(method);
                Map<String, Property> properties = new HashMap<>();
                for (Parameter parameter : operation.getParameters())
                {
                    Property property = convertParameterToProperty(parameter);
                    if (property != null)
                    {
                        properties.put(property.getName(), property);
                    }
                }
                if (!properties.isEmpty())
                {
                    Model model = new ModelImpl();
                    String operationId = getOrGenerateOperationId(operation, pathKey, method.name());
                    model.setDescription("Query parameters for " + operationId);
                    model.setProperties(properties);
                    model.getVendorExtensions().put(VEN_FROM_QUERY, Boolean.TRUE);
                    String definitionName = generateUniqueDefinitionName(operationId + "QueryData", swagger);
                    swagger.addDefinition(definitionName, model);
                    String definitionModel = "\\" + modelPackage + "\\" + toModelName(definitionName);
                    operation.getVendorExtensions().put(VEN_QUERY_DATA_TYPE, definitionModel);
                    operation.getVendorExtensions().put(VEN_HAS_QUERY_DATA, Boolean.TRUE);
                }
            }
        }
    }

    protected Property convertParameterToProperty(Parameter parameter) {
        Property property = null;
        if (parameter instanceof QueryParameter)
        {
            QueryParameter queryParameter = (QueryParameter) parameter;
            switch (queryParameter.getType())
            {
                case "string":
                    StringProperty stringProperty = new StringProperty();
                    stringProperty.setMinLength(queryParameter.getMinLength());
                    stringProperty.setMaxLength(queryParameter.getMaxLength());
                    stringProperty.setPattern(queryParameter.getPattern());
                    stringProperty.setEnum(queryParameter.getEnum());
                    property = stringProperty;
                    break;
                case "integer":
                    IntegerProperty integerProperty = new IntegerProperty();
                    integerProperty.setMinimum(queryParameter.getMinimum());
                    integerProperty.setMaximum(queryParameter.getMaximum());
                    property = integerProperty;
                    break;
                case "number":
                    FloatProperty floatProperty = new FloatProperty();
                    floatProperty.setMinimum(queryParameter.getMinimum());
                    floatProperty.setMaximum(queryParameter.getMaximum());
                    property = floatProperty;
                    break;
                case "boolean":
                    property = new BooleanProperty();
                    break;
                case "array":
                    ArrayProperty arrayProperty = new ArrayProperty();
                    arrayProperty.setMinItems(queryParameter.getMinItems());
                    arrayProperty.setMaxItems(queryParameter.getMaxItems());
                    arrayProperty.setItems(queryParameter.getItems());
                    String collectionFormat = queryParameter.getCollectionFormat();
                    if (collectionFormat == null) {
                        collectionFormat = "csv";
                    }
                    arrayProperty.getVendorExtensions().put(VEN_COLLECTION_FORMAT, collectionFormat);
                    property = arrayProperty;
                    break;
                case "date":
                    property = new DateProperty();
                    break;
                case "date-time":
                    property = new DateTimeProperty();
                    break;
            }
            if (property != null)
            {
                property.setName(queryParameter.getName());
                property.setDescription(queryParameter.getDescription());
                property.setRequired(queryParameter.getRequired());
                property.getVendorExtensions().put(VEN_FROM_QUERY, Boolean.TRUE);
            }
        }
        return property;
    }

    protected String generateUniqueDefinitionName(String name, Swagger swagger)
    {
        String result = name;
        if (swagger.getDefinitions() != null) {
            int count = 1;
            while (swagger.getDefinitions().containsKey(result))
            {
                result = name + "_" + count;
                count += 1;
            }
        }
        return result;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        objs = super.postProcessOperations(objs);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        String interfaceToImplement;
        StringBuilder interfacesToImplement = new StringBuilder();
        String classMethod;
        String pathPattern = null;
        for (CodegenOperation op : operationList) {
            switch (op.httpMethod) {
                case "GET":
                    interfaceToImplement = "Operation\\GetInterface";
                    classMethod = "handleGet";
                    break;
                case "POST":
                    interfaceToImplement = "Operation\\PostInterface";
                    classMethod = "handlePost";
                    break;
                case "PATCH":
                    interfaceToImplement = "Operation\\PatchInterface";
                    classMethod = "handlePatch";
                    break;
                case "PUT":
                    interfaceToImplement = "Operation\\PutInterface";
                    classMethod = "handlePut";
                    break;
                case "DELETE":
                    interfaceToImplement = "Operation\\DeleteInterface";
                    classMethod = "handleDelete";
                    break;
                default:
                    throw new RuntimeException("Unknown HTTP Method " + op.httpMethod + " not allowed");
            }
            if (interfacesToImplement.length() > 0) {
                interfacesToImplement.append(", ");
            }
            interfacesToImplement.append(interfaceToImplement);
            op.httpMethod = classMethod;
            //All operations have same path because of custom operation grouping, so path pattern can be calculated only once
            if (pathPattern == null) {
                pathPattern = generatePathPattern(op);
            }
        }
        operations.put("interfacesToImplement", interfacesToImplement.toString());
        operations.put("pathPattern", pathPattern);

        return objs;
    }

    protected String generatePathPattern(CodegenOperation op) {
        String result = op.path;
        for (CodegenParameter pp : op.pathParams) {
            StringBuilder replacement = new StringBuilder( "{" + pp.paramName);
            if (pp.isEnum) {
                StringBuilder enumRegExp = new StringBuilder();
                for (String enumValue : pp._enum) {
                    if (enumRegExp.length() > 0) {
                        enumRegExp.append("|");
                    }
                    enumRegExp.append(enumValue.replaceAll("[\\Q<>()[]{}|^$-=!?*+.\\\\E]", "\\\\$0"));
                }
                replacement.append(":");
                replacement.append(enumRegExp);
            } else if (pp.isInteger) {
                replacement.append(":0|(?:-?[1-9][0-9]*)");
            } else if (pp.isString && (pp.pattern != null) && (!pp.pattern.isEmpty())) {
                replacement.append(":");
                replacement.append(pp.pattern);
            }
            //TODO add regular expressions for other types if they are actually used for path parameters
            replacement.append("}");
            result = result.replace("{" + pp.paramName + "}", replacement);
        }
        return result;
    }
}
