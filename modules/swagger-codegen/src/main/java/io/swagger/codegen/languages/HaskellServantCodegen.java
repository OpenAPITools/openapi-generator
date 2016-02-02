package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.*;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;

import java.util.*;
import java.io.File;

public class HaskellServantCodegen extends DefaultCodegen implements CodegenConfig {

  // source folder where to write the files
  protected String sourceFolder = "src";
  protected String apiVersion = "0.0.1";

  /**
   * Configures the type of generator.
   * 
   * @return  the CodegenType for this generator
   * @see     io.swagger.codegen.CodegenType
   */
  public CodegenType getTag() {
    return CodegenType.SERVER;
  }

  /**
   * Configures a friendly name for the generator.  This will be used by the generator
   * to select the library with the -l flag.
   * 
   * @return the friendly name for the generator
   */
  public String getName() {
    return "haskell-servant";
  }

  /**
   * Returns human-friendly help for the generator.  Provide the consumer with help
   * tips, parameters here
   * 
   * @return A string value for the help message
   */
  public String getHelp() {
    return "Generates a HaskellServantCodegen library.";
  }

  public HaskellServantCodegen() {
    super();

    // set the output folder here
    outputFolder = "generated-code/HaskellServantCodegen";

    /**
     * Models.  You can write model files using the modelTemplateFiles map.
     * if you want to create one template for file, you can do so here.
     * for multiple files for model, just put another entry in the `modelTemplateFiles` with
     * a different extension
     */
    modelTemplateFiles.put(
      "model.mustache", // the template to use
      ".hs");       // the extension for each file to write

    /**
     * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
     * as with models, add multiple entries with different extensions for multiple files per
     * class
     */
    apiTemplateFiles.put(
      "api.mustache",   // the template to use
      ".hs");       // the extension for each file to write

    /**
     * Template Location.  This is the location which templates will be read from.  The generator
     * will use the resource stream to attempt to read the templates.
     */
    embeddedTemplateDir = templateDir = "haskell-servant";

    /**
     * Api Package.  Optional, if needed, this can be used in templates
     */
    apiPackage = "Api";

    /**
     * Model Package.  Optional, if needed, this can be used in templates
     */
    modelPackage = "Model";

    /**
     * Reserved words.  Override this with reserved words specific to your language
     */
    // from https://wiki.haskell.org/Keywords
    reservedWords = new HashSet<String>(
        Arrays.asList(
            "as", "case", "of",
            "class", "data", // "data family", "data instance",
            "default", "deriving", // "deriving instance",
            "do",
            "forall", "foreign", "hiding",
            "id",
            "if", "then", "else",
            "import", "infix", "infixl", "infixr",
            "instance", "let", "in",
            "mdo", "module", "newtype",
            "proc", "qualified", "rec",
            "type", // "type family", "type instance",
            "where"
        )
    );

    /**
     * Additional Properties.  These values can be passed to the templates and
     * are available in models, apis, and supporting files
     */
    additionalProperties.put("apiVersion", apiVersion);

    /**
     * Supporting Files.  You can write single files for the generator with the
     * entire object tree available.  If the input file has a suffix of `.mustache
     * it will be processed by the template engine.  Otherwise, it will be copied
     */
    supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    supportingFiles.add(new SupportingFile("stack.mustache", "", "stack.yaml"));
    supportingFiles.add(new SupportingFile("haskell-servant-codegen.mustache", "", "haskell-servant-codegen.cabal"));
    supportingFiles.add(new SupportingFile("Setup.mustache", "", "Setup.hs"));
    supportingFiles.add(new SupportingFile("LICENSE", "", "LICENSE"));
    supportingFiles.add(new SupportingFile("Apis.mustache", "lib", "Apis.hs"));
    supportingFiles.add(new SupportingFile("Utils.mustache", "lib", "Utils.hs"));
    supportingFiles.add(new SupportingFile("Client.mustache", "client", "Main.hs"));
    supportingFiles.add(new SupportingFile("Server.mustache", "server", "Main.hs"));

    /**
     * Language Specific Primitives.  These types will not trigger imports by
     * the client generator
     */
    languageSpecificPrimitives = new HashSet<String>(
      Arrays.asList(
        "Bool",
        "String",
        "Int",
        "Integer",
        "Float",
        "Char",
        "Double",
        "List",
        "FilePath"
        )
    );

    typeMapping.clear();
    // typeMapping.put("enum", "NSString");
    typeMapping.put("array", "List");
    typeMapping.put("set", "Set");
    typeMapping.put("boolean", "Bool");
    typeMapping.put("string", "String");
    typeMapping.put("int", "Int");
    typeMapping.put("long", "Integer");
    typeMapping.put("float", "Float");
    // typeMapping.put("byte", "Byte");
    typeMapping.put("short", "Int");
    typeMapping.put("char", "Char");
    typeMapping.put("double", "Double");
    typeMapping.put("DateTime", "Integer");
    // typeMapping.put("object", "Map");
    typeMapping.put("file", "FilePath");

    importMapping.clear();
    importMapping.put("Map", "qualified Data.Map as Map");

    cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
    cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
  }

  /**
   * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
   * those terms here.  This logic is only called if a variable matches the reseved words
   * 
   * @return the escaped term
   */
  @Override
  public String escapeReservedWord(String name) {
    return name + "_";
  }

  /**
   * Location to write model files.  You can use the modelPackage() as defined when the class is
   * instantiated
   */
  public String modelFileFolder() {
    return outputFolder + File.separatorChar + "lib" + File.separatorChar + modelPackage().replace('.', File.separatorChar);
  }

  /**
   * Location to write api files.  You can use the apiPackage() as defined when the class is
   * instantiated
   */
  @Override
  public String apiFileFolder() {
    return outputFolder + File.separatorChar + "lib" + File.separatorChar + apiPackage().replace('.', File.separatorChar);
  }

  /**
   * Optional - type declaration.  This is a String which is used by the templates to instantiate your
   * types.  There is typically special handling for different property types
   *
   * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
   */
  @Override
  public String getTypeDeclaration(Property p) {
    if(p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return "[" + getTypeDeclaration(inner) + "]";
    }
    else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();
      return "Map.Map String " + getTypeDeclaration(inner);
    }
    return super.getTypeDeclaration(p);
  }

  /**
   * Optional - swagger type conversion.  This is used to map swagger types in a `Property` into 
   * either language specific types via `typeMapping` or into complex models if there is not a mapping.
   *
   * @return a string value of the type or complex model for this property
   * @see io.swagger.models.properties.Property
   */
  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if(typeMapping.containsKey(swaggerType)) {
      type = typeMapping.get(swaggerType);
      if(languageSpecificPrimitives.contains(type))
        return toModelName(type);
    }
    else
      type = swaggerType;
    return toModelName(type);
  }

  private String capturePath(String path, List<CodegenParameter> pathParams) {
    for (CodegenParameter p : pathParams) {
      String pName = "{"+p.baseName+"}";
      if (path.indexOf(pName) >= 0) { 
        path = path.replace(pName, "Capture " + "\""+p.baseName+"\" " + p.dataType);
      }
    }
    return path;
  }

  private String queryPath(String path, List<CodegenParameter> queryParams) {
    for (CodegenParameter p : queryParams) {
      path += " :> QueryParam \"" + p.baseName + "\" " + p.dataType;
    }
    return path;
  }

  private String bodyPath(String path, List<CodegenParameter> bodyParams) {
    for (CodegenParameter p : bodyParams) {
      path += " :> ReqBody '[JSON] " + p.dataType;
    }
    return path;
  }

  private String formPath(String path, List<CodegenParameter> formParams) {
    String names = "Form";
    for (CodegenParameter p : formParams) {
      if(p.dataType.equals("FilePath")){
        // file data processing
      } 
      names += p.baseName;
    }
    if(formParams.size() > 0){
      path += " :> ReqBody '[FormUrlEncoded] " + names;
    }
    return path;
  }

  private String headerPath(String path, List<CodegenParameter> headerParams) {
    for (CodegenParameter p : headerParams) {
      path += " :> Header \"" + p.baseName + "\" " + p.dataType;
    }
    return path;
  }


  private String filterReturnType(String rt) {
    if (rt == null || rt.equals("null")) {
      return "()";
    } else if (rt.indexOf(" ") >= 0) {
      return "(" + rt + ")";
    }
    return rt;
  }

  private String addReturnPath(String path, String httpMethod, String returnType) {
    return path + " :> " + upperCaseFirst(httpMethod) + " '[JSON] " + filterReturnType(returnType);
  }

  private String joinStrings(String sep, List<String> ss) {
    StringBuilder sb = new StringBuilder();
    for (String s : ss) {
      if (sb.length() > 0) {
        sb.append(sep);
      }
      sb.append(s);
    }
    return sb.toString();
  }

  private String replacePathSplitter(String path) {
    String[] ps = path.replaceFirst("/", "").split("/", 0);
    List<String> rs = new ArrayList<String>();
    for (String p : ps) {
      if (p.indexOf("{") < 0) { 
        rs.add("\"" + p + "\"");
      } else {
        rs.add(p);
      }
    }
    return joinStrings(" :> ", rs);
  }

  private String upperCaseFirst(String str) {
    char[] array = str.toLowerCase().toCharArray();
    array[0] = Character.toUpperCase(array[0]);
    return new String(array);
  }

  private String parseScheme(String basePath) {
    return "Http";
  }

  @Override
  public CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger){
    CodegenOperation op = super.fromOperation(resourcePath, httpMethod, operation, definitions, swagger);
    String path = op.path;
    op.nickname = addReturnPath(headerPath(formPath(bodyPath(queryPath(capturePath(replacePathSplitter(path), op.pathParams), op.queryParams), op.bodyParams), op.formParams), op.headerParams), op.httpMethod, op.returnType);
    return op;
  }

}
