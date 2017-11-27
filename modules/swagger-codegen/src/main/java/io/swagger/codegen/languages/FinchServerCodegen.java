package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Model;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

public class FinchServerCodegen extends DefaultCodegen implements CodegenConfig {
    protected String invokerPackage = "io.swagger.client";
    protected String groupId = "io.swagger";
    protected String artifactId = "finch-server";
    protected String artifactVersion = "1.0.0";
    protected String sourceFolder = "src/main/scala";
    protected String packageName = "io.swagger";

    public FinchServerCodegen() {
        super();
        outputFolder = "generated-code/finch";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "finch";

        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        setReservedWordsLowerCase(
                Arrays.asList(
                        // Scala
                        "abstract", "case", "catch", "class", "def",
                        "do", "else", "extends", "false", "final",
                        "finally", "for", "forSome", "if", "implicit",
                        "import", "lazy", "match", "new", "null",
                        "object", "override", "package", "private", "protected",
                        "return", "sealed", "super", "this", "throw",
                        "trait", "try", "true", "type", "val",
                        "var", "while", "with", "yield",
                        // Scala-interop languages keywords
                        "abstract", "continue", "switch", "assert",
                        "default", "synchronized", "goto",
                        "break", "double", "implements", "byte",
                        "public", "throws", "enum", "instanceof", "transient",
                        "int", "short", "char", "interface", "static",
                        "void", "finally", "long", "strictfp", "volatile", "const", "float",
                        "native")
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList("double",
                        "Int",
                        "Long",
                        "Float",
                        "Double",
                        "char",
                        "float",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float",
                        "List",
                        "Set",
                        "Map")
        );

        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "String");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("integer", "Int");
        typeMapping.put("float", "Float");
        typeMapping.put("long", "Long");
        typeMapping.put("double", "Double");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("date-time", "ZonedDateTime");
        typeMapping.put("date", "LocalDateTime");
        typeMapping.put("file", "File");
        typeMapping.put("array", "Seq");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("object", "Object");
        typeMapping.put("binary", "Array[Byte]");
        typeMapping.put("Date", "LocalDateTime");
        typeMapping.put("DateTime", "ZonedDateTime");

        additionalProperties.put("modelPackage", modelPackage());
        additionalProperties.put("apiPackage", apiPackage());
        additionalProperties.put("appName", "Swagger Sample");
        additionalProperties.put("appDescription", "A sample swagger server");
        additionalProperties.put("infoUrl", "http://swagger.io");
        additionalProperties.put("infoEmail", "apiteam@swagger.io");
        additionalProperties.put("licenseInfo", "Apache 2.0");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt", "", "build.sbt"));
        supportingFiles.add(new SupportingFile("Server.mustache", sourceFolder, "Server.scala"));
        supportingFiles.add(new SupportingFile("DataAccessor.mustache", sourceFolder, "DataAccessor.scala"));

        supportingFiles.add(new SupportingFile("project/build.properties", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("project/plugins.sbt", "project", "plugins.sbt"));
        supportingFiles.add(new SupportingFile("sbt", "", "sbt"));

        supportingFiles.add(new SupportingFile("endpoint.mustache", sourceFolder, "endpoint.scala"));
        supportingFiles.add(new SupportingFile("errors.mustache", sourceFolder, "errors.scala"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "Boolean",
                        "Double",
                        "Int",
                        "Integer",
                        "Long",
                        "Float",
                        "Any",
                        "AnyVal",
                        "AnyRef",
                        "Object")
        );
        instantiationTypes.put("array", "ArrayList");
        instantiationTypes.put("map", "HashMap");

        importMapping = new HashMap<String, String>();
        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "scala.collection.immutable.Map");
        importMapping.put("HashMap", "scala.collection.immutable.HashMap");
        importMapping.put("Seq", "scala.collection.immutable.Seq");
        importMapping.put("ArrayBuffer", "scala.collection.mutable.ArrayBuffer");
        importMapping.put("DateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");
        importMapping.put("ZonedDateTime", "java.time.ZonedDateTime");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Finch package name (e.g. io.swagger).")
                .defaultValue(this.packageName));
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "finch";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala server application with Finch.";
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    /**
     * Convert Swagger Model object to Codegen Model object
     *
     * @param name           the name of the model
     * @param model          Swagger Model object
     * @param allDefinitions a map of all Swagger models from the spec
     * @return Codegen Model object
     */
    @Override
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);
        return codegenModel;
    }




    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {

            // Converts GET /foo/bar => get("foo" :: "bar")
            generateScalaPath(op);

            // Generates e.g. uuid :: header("boo") :: params("baa") under key "x-codegen-pathParams"
            // Generates e.g. (id: UUID, headerBoo: String, paramBaa: String) under key "x-codegen-typedInputParams"
            // Generates e.g. (id, headerBoo, paramBaa) under key "x-codegen-inputParams"
            generateInputParameters(op);

            //Generate Auth parameters using security: definition
            //Results in header("apiKey") or param("apiKey")
            authParameters(op);

            //Concatenates all parameters
            concatParameters(op);
        }

        return objs;
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
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }


    /**
     *
     * @param prim
     * @param isRequired
     * @param canBeOptional
     * @return
     */
    private String toPrimitive(String prim, Boolean isRequired, Boolean canBeOptional) {

        String converter = ".map(_.to" + prim + ")";
        return  (canBeOptional ? (isRequired ? converter : ".map(_" + converter +")") : "");
    }

    //All path parameters are String initially, for primitives these need to be converted
    private String toPathParameter(CodegenParameter p, String paramType, Boolean canBeOptional ) {

        Boolean isNotAString = !p.dataType.equals("String");

        return  paramType + (canBeOptional && !p.required ? "Option" : "") + "(\""+ p.baseName + "\")" + (isNotAString ? toPrimitive(p.dataType,p.required,canBeOptional) : "") ;
    }

    private String toInputParameter(CodegenParameter p){
        return (p.required ? "" : "Option[")+p.dataType+(p.required ? "" : "]");
    }

    private String concat(String original, String addition, String op) {
        return original + (original.isEmpty() ? "" : (addition.isEmpty() ? "" : op)) + addition;
    }

    // a, b
    private String csvConcat(String original, String addition) {
        return concat(original, addition,", ");
    }
    // a :: b
    private String colConcat(String original, String addition) {
        return concat(original, addition," :: ");
    }

    private void authParameters(CodegenOperation op) {

        String authParams = "";
        String authInputParams = "";
        String typedAuthInputParams = "";
        //Append apikey security to path params and create input parameters for functions
        if(op.authMethods != null){

            for(CodegenSecurity s : op.authMethods) {
                if(s.isApiKey && s.isKeyInHeader){
                    authParams = colConcat(authParams, "header(\""+ s.keyParamName + "\")");
                } else if(s.isApiKey && s.isKeyInQuery){
                    authParams = colConcat(authParams, "param(\""+ s.keyParamName + "\")");
                }
                if(s.isApiKey) {
                    typedAuthInputParams = csvConcat(typedAuthInputParams, "authParam"+ s.name + ": String");
                    authInputParams = csvConcat(authInputParams,"authParam"+ s.name);
                }
            }
        }

        op.vendorExtensions.put("x-codegen-authParams", authParams);
        op.vendorExtensions.put("x-codegen-authInputParams", authInputParams);
        op.vendorExtensions.put("x-codegen-typedAuthInputParams", typedAuthInputParams);

    }

    private void generateScalaPath(CodegenOperation op) {
        op.httpMethod = op.httpMethod.toLowerCase();

        String path = new String(op.path);

        // remove first /
        if (path.startsWith("/")) {
            path = path.substring(1);
        }

        // remove last /
        if (path.endsWith("/")) {
            path = path.substring(0, path.length()-1);
        }

        String[] items = path.split("/", -1);
        String scalaPath = "";
        Integer pathParamIndex = 0;

        for (int i = 0; i < items.length; ++i) {

            if (items[i].matches("^\\{(.*)\\}$")) { // wrap in {}
                // find the datatype of the parameter
                final CodegenParameter cp = op.pathParams.get(pathParamIndex);

                // TODO: Handle non-primitives…
                scalaPath = colConcat(scalaPath, cp.dataType.toLowerCase());

                pathParamIndex++;
            } else {
                scalaPath = colConcat(scalaPath, "\"" + items[i] + "\"");
            }
        }

        op.vendorExtensions.put("x-codegen-path", scalaPath);

    }


    private void concatParameters(CodegenOperation op) {

        String path = colConcat(colConcat(op.vendorExtensions.get("x-codegen-path").toString(),op.vendorExtensions.get("x-codegen-pathParams").toString()), op.vendorExtensions.get("x-codegen-authParams").toString());
        String parameters = csvConcat(op.vendorExtensions.get("x-codegen-inputParams").toString(), op.vendorExtensions.get("x-codegen-authInputParams").toString());
        String typedParameters = csvConcat(op.vendorExtensions.get("x-codegen-typedInputParams").toString(), op.vendorExtensions.get("x-codegen-typedAuthInputParams").toString());

        // The input parameters for functions
        op.vendorExtensions.put("x-codegen-paths",path);
        op.vendorExtensions.put("x-codegen-params", parameters);
        op.vendorExtensions.put("x-codegen-typedParams", typedParameters);

    }


    private void generateInputParameters(CodegenOperation op) {

        String inputParams = "";
        String typedInputParams = "";
        String pathParams = "";

        for (CodegenParameter p : op.allParams) {
            // TODO: This hacky, should be converted to mappings if possible to keep it clean.
            // This could also be done using template imports

            if(p.isBodyParam) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", "jsonBody["+ p.dataType + "]");
                p.vendorExtensions.put("x-codegen-normalized-input-type", p.dataType);
            } else if(p.isContainer || p.isListContainer) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", toPathParameter(p,"params", false));
                p.vendorExtensions.put("x-codegen-normalized-input-type", p.dataType.replaceAll("^[^\\[]+", "Seq"));
            } else if(p.isQueryParam) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", toPathParameter(p, "param",true));
                p.vendorExtensions.put("x-codegen-normalized-input-type", toInputParameter(p));
            } else if(p.isHeaderParam) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", toPathParameter(p,"header", true));
                p.vendorExtensions.put("x-codegen-normalized-input-type", toInputParameter(p));
            } else if(p.isFile) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", "fileUpload(\""+ p.paramName + "\")");
                p.vendorExtensions.put("x-codegen-normalized-input-type", "FileUpload");
            } else if(p.isPrimitiveType && !p.isPathParam) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", p.dataType.toLowerCase());
                p.vendorExtensions.put("x-codegen-normalized-input-type", toInputParameter(p));
            } else {
                //Path paremeters are handled in generateScalaPath()
                p.vendorExtensions.put("x-codegen-normalized-input-type", p.dataType);
            }
            if(p.vendorExtensions.get("x-codegen-normalized-path-type") != null){
                pathParams = colConcat(pathParams , p.vendorExtensions.get("x-codegen-normalized-path-type").toString());
            }
            inputParams = csvConcat(inputParams, p.paramName);
            typedInputParams = csvConcat(typedInputParams , p.paramName + ": " + p.vendorExtensions.get("x-codegen-normalized-input-type"));

        }

        // All body, path, query and header parameters
        op.vendorExtensions.put("x-codegen-pathParams", pathParams);

        // The input parameters for functions
        op.vendorExtensions.put("x-codegen-inputParams", inputParams);
        op.vendorExtensions.put("x-codegen-typedInputParams", typedInputParams);

    }

}
