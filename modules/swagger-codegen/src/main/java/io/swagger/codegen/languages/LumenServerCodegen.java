package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class LumenServerCodegen extends DefaultCodegen implements CodegenConfig {

    // source folder where to write the files
    protected String sourceFolder = "src";
    protected String apiVersion = "1.0.0";

    /**
     * Configures the type of generator.
     * 
     * @return  the CodegenType for this generator
     * @see     io.swagger.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -l flag.
     * 
     * @return the friendly name for the generator
     */
    public String getName() {
        return "lumen";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     * 
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a LumenServerCodegen client library.";
    }

    public LumenServerCodegen() {
        super();

        // set the output folder here
        outputFolder = "generated-code/lumen";
        String packagePath = "lumen";

        // modelPackage = packagePath + "\\lib\\Models";
        // apiPackage = packagePath + "\\lib";
        // // outputFolder = "generated-code" + File.separator + "slim";
        // modelTemplateFiles.put("model.mustache", ".php");

        /**
         * Models.  You can write model files using the modelTemplateFiles map.
         * if you want to create one template for file, you can do so here.
         * for multiple files for model, just put another entry in the `modelTemplateFiles` with
         * a different extension
         */
        // modelTemplateFiles.put(
        //     "model.mustache", // the template to use
        //     ".sample");       // the extension for each file to write

        /**
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        // apiTemplateFiles.put(
        //     "api.mustache",   // the template to use
        //     ".sample");       // the extension for each file to write
        

        // no api files
        apiTemplateFiles.clear();

        // embeddedTemplateDir = templateDir = "slim";

        /**
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        templateDir = "lumen";

        /**
         * Api Package.  Optional, if needed, this can be used in templates
         */
        apiPackage = "io.swagger.client.api";

        /**
         * Model Package.  Optional, if needed, this can be used in templates
         */
        modelPackage = "io.swagger.client.model";

        /**
         * Reserved words.  Override this with reserved words specific to your language
         */
        reservedWords = new HashSet<String> (
            Arrays.asList(
                "sample1",  // replace with static values
                "sample2")
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
        // supportingFiles.add(new SupportingFile("index.mustache", packagePath, "index.php"));
        // supportingFiles.add(new SupportingFile("routes.mustache", packagePath, "routes.php"));

        supportingFiles.add(new SupportingFile("composer.json", packagePath, "composer.json"));
        supportingFiles.add(new SupportingFile("readme.md", packagePath, "readme.md"));
        supportingFiles.add(new SupportingFile("artisan", packagePath, "artisan"));
        // supportingFiles.add(new SupportingFile("server.php", packagePath, "server.php"));

        supportingFiles.add(new SupportingFile("bootstrap" + File.separator + "app.php", packagePath + File.separator + "bootstrap", "app.php"));

        supportingFiles.add(new SupportingFile("public" + File.separator + "index.php", packagePath + File.separator + "public", "index.php"));

        supportingFiles.add(new SupportingFile("app" + File.separator + "User.php", packagePath + File.separator + "app", "User.php"));
        supportingFiles.add(new SupportingFile("app" + File.separator + "Console" + File.separator + "Kernel.php", packagePath + File.separator + "app"  + File.separator + "Console", "Kernel.php"));
        supportingFiles.add(new SupportingFile("app" + File.separator + "Exceptions" + File.separator + "Handler.php", packagePath + File.separator + "app"  + File.separator + "Exceptions", "Handler.php"));
        // supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Kernel.php", packagePath + File.separator + "app"  + File.separator + "Http", "Kernel.php"));
        supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "routes.mustache", packagePath + File.separator + "app"  + File.separator + "Http", "routes.php"));
        
        supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Controllers" + File.separator + "Controller.php", packagePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Controllers" + File.separator, "Controller.php"));
        supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Controllers" + File.separator + "ExampleController.php", packagePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Controllers" + File.separator, "ExampleController.php"));
        // supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Controllers" + File.separator + "Auth" + File.separator + "AuthController.php", packagePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Controllers" + File.separator + "Auth" + File.separator, "AuthController.php"));
        // supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Controllers" + File.separator + "Auth" + File.separator + "PasswordController.php", packagePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Controllers" + File.separator + "Auth" + File.separator, "PasswordController.php"));
        
        supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Middleware" + File.separator + "Authenticate.php", packagePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Middleware" + File.separator, "Authenticate.php"));
        supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Middleware" + File.separator + "ExampleMiddleware.php", packagePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Middleware" + File.separator, "ExampleMiddleware.php"));
        // supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Middleware" + File.separator + "RedirectIfAuthenticated.php", packagePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Middleware" + File.separator, "RedirectIfAuthenticated.php"));
        // supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Middleware" + File.separator + "VerifyCsrfToken.php", packagePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Middleware" + File.separator, "VerifyCsrfToken.php"));
        
        // supportingFiles.add(new SupportingFile("app" + File.separator + "Http" + File.separator + "Requests" + File.separator + "Request.php", packagePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Requests" + File.separator, "Request.php"));
                
        supportingFiles.add(new SupportingFile("app" + File.separator + "Providers" + File.separator + "AppServiceProvider.php", packagePath + File.separator + "app"  + File.separator + "Providers", "AppServiceProvider.php"));
        supportingFiles.add(new SupportingFile("app" + File.separator + "Providers" + File.separator + "AuthServiceProvider.php", packagePath + File.separator + "app"  + File.separator + "Providers", "AuthServiceProvider.php"));
        supportingFiles.add(new SupportingFile("app" + File.separator + "Providers" + File.separator + "EventServiceProvider.php", packagePath + File.separator + "app"  + File.separator + "Providers", "EventServiceProvider.php"));
        // supportingFiles.add(new SupportingFile("app" + File.separator + "Providers" + File.separator + "RouteServiceProvider.php", packagePath + File.separator + "app"  + File.separator + "Providers", "RouteServiceProvider.php"));

        // supportingFiles.add(new SupportingFile("config" + File.separator + "app.php", packagePath + File.separator + "config"  + File.separator, "app.php"));

        /**
         * Language Specific Primitives.  These types will not trigger imports by
         * the client generator
         */
        languageSpecificPrimitives = new HashSet<String>(
            Arrays.asList(
                "Type1",      // replace these with your types
                "Type2")
        );
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reseved words
     * 
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;  // add an underscore to the name
    }

    /**
     * Location to write model files.  You can use the modelPackage() as defined when the class is
     * instantiated
     */
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
    public String getTypeDeclaration(Property p) {
        if(p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
        }
        else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "[String, " + getTypeDeclaration(inner) + "]";
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
}