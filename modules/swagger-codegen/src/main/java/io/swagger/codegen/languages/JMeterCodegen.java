package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.Operation;
import io.swagger.oas.models.PathItem;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.Schema;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

public class JMeterCodegen extends DefaultCodegen implements CodegenConfig {

  // source folder where to write the files
  protected String sourceFolder = "";
  protected String apiVersion = "1.0.0";

  /**
   * Configures the type of generator.
   * 
   * @return  the CodegenType for this generator
   * @see     io.swagger.codegen.CodegenType
   */
  @Override
  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  /**
   * Configures a friendly name for the generator.  This will be used by the generator
   * to select the library with the -l flag.
   * 
   * @return the friendly name for the generator
   */
  @Override
  public String getName() {
    return "jmeter";
  }

  /**
   * Returns human-friendly help for the generator.  Provide the consumer with help
   * tips, parameters here
   * 
   * @return A string value for the help message
   */
  @Override
  public String getHelp() {
    return "Generates a JMeter .jmx file.";
  }

  public JMeterCodegen() {
    super();

    // set the output folder here
    outputFolder = "generated-code/JMeterCodegen";

    /*
     * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
     * as with models, add multiple entries with different extensions for multiple files per
     * class
     */
    apiTemplateFiles.put(
      "api.mustache",   // the template to use
      ".jmx");       // the extension for each file to write

    apiTemplateFiles.put("testdata-localhost.mustache", ".csv");

    /*
     * Template Location.  This is the location which templates will be read from.  The generator
     * will use the resource stream to attempt to read the templates.
     */
    embeddedTemplateDir = templateDir = "JMeter";

    /*
     * Api Package.  Optional, if needed, this can be used in templates
     */
    apiPackage = "";

    /*
     * Model Package.  Optional, if needed, this can be used in templates
     */
    modelPackage = "";

    /*
     * Reserved words.  Override this with reserved words specific to your language
     */
    reservedWords = new HashSet<String>(
      Arrays.asList(
        "sample1",  // replace with static values
        "sample2")
    );

    /*
     * Additional Properties.  These values can be passed to the templates and
     * are available in models, apis, and supporting files
     */
    additionalProperties.put("apiVersion", apiVersion);

//    supportingFiles.add(new SupportingFile("testdata-localhost.mustache", "input", "testdata-localhost.csv"));
  }

  @Override
  public void preprocessOpenAPI(OpenAPI openAPI) {
    if (openAPI != null && openAPI.getPaths() != null) {
      for (String pathname : openAPI.getPaths().keySet()) {
        PathItem path = openAPI.getPaths().get(pathname);
        List<Operation> operations = path.readOperations();
        if (operations != null && !operations.isEmpty()) {
          for (Operation operation : operations) {
            String pathWithDollars = pathname.replaceAll("\\{", "\\$\\{");
            operation.addExtension("x-path", pathWithDollars);
          }
        }
      }
    }
  }

  /**
   * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
   * those terms here.  This logic is only called if a variable matches the reseved words
   * 
   * @return the escaped term
   */
    @Override
    public String escapeReservedWord(String name) {           
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

  /**
   * Location to write model files.  You can use the modelPackage() as defined when the class is
   * instantiated
   */
  @Override
  public String modelFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
  }

  /**
   * Location to write api files.  You can use the apiPackage() as defined when the class is
   * instantiated
   */
  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
  }

  /**
   * Optional - type declaration.  This is a String which is used by the templates to instantiate your
   * types.  There is typically special handling for different property types
   *
   * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
   */
  @Override
  public String getTypeDeclaration(Schema propertySchema) {
    if (propertySchema instanceof ArraySchema) {
      Schema inner = ((ArraySchema) propertySchema).getItems();
      return String.format("%s[%s]", getSchemaType(propertySchema), getTypeDeclaration(inner));
    } else if (propertySchema instanceof MapSchema) {
      Schema inner = propertySchema.getAdditionalProperties();
      return String.format("%s[String, %s]", getSchemaType(propertySchema), getTypeDeclaration(inner));
    }
    return super.getTypeDeclaration(propertySchema);
  }

  /**
   * Optional - swagger type conversion.  This is used to map swagger types in a `Property` into 
   * either language specific types via `typeMapping` or into complex models if there is not a mapping.
   *
   * @return a string value of the type or complex model for this property
   * @see io.swagger.oas.models.media.Schema
   */
  @Override
  public String getSchemaType(Schema propertySchema) {
    String schemaType = super.getSchemaType(propertySchema);
    String type = null;
    if (typeMapping.containsKey(schemaType)) {
      type = typeMapping.get(schemaType);
      if (languageSpecificPrimitives.contains(type)) {
        return toModelName(type);
      }
    } else {
      type = schemaType;
    }
    return toModelName(type);
  }

  @Override
  public String escapeQuotationMark(String input) {
    // remove ' to avoid code injection
    return input.replace("'", "");
  }

  @Override
  public String escapeUnsafeCharacters(String input) {
    return input.replace("*/", "*_/").replace("/*", "/_*");
  }

}
