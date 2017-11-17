package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.*;

import java.util.*;
import java.io.File;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

public class LumenServerCodegen extends AbstractPhpCodegen 
{
     @SuppressWarnings("hiding")
    protected String apiVersion = "1.0.0";
        
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
        return "lumen";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     * 
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a LumenServerCodegen server library.";
    }

    public LumenServerCodegen() {
        super();

        embeddedTemplateDir = templateDir = "lumen";

        /*
         * packPath
         */
        invokerPackage = "lumen";
        packagePath = "";

        /*
         * Api Package.  Optional, if needed, this can be used in templates
         */
        apiPackage = "app.Http.Controllers";

        /*
         * Model Package.  Optional, if needed, this can be used in templates
         */
        modelPackage = "models";

        // template files want to be ignored
        modelTemplateFiles.clear();
        apiTestTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        modelDocTemplateFiles.clear();

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("composer.mustache", packagePath + File.separator + srcBasePath, "composer.json"));
        supportingFiles.add(new SupportingFile("readme.md", packagePath + File.separator + srcBasePath, "readme.md"));
        supportingFiles.add(new SupportingFile("app.php", packagePath + File.separator + srcBasePath + File.separator + "bootstrap", "app.php"));
        supportingFiles.add(new SupportingFile("index.php", packagePath + File.separator + srcBasePath + File.separator + "public", "index.php"));
        supportingFiles.add(new SupportingFile("User.php", packagePath + File.separator + srcBasePath + File.separator + "app", "User.php"));
        supportingFiles.add(new SupportingFile("Kernel.php", packagePath + File.separator + srcBasePath + File.separator + "app"  + File.separator + "Console", "Kernel.php"));
        supportingFiles.add(new SupportingFile("Handler.php", packagePath + File.separator + srcBasePath + File.separator + "app"  + File.separator + "Exceptions", "Handler.php"));
        supportingFiles.add(new SupportingFile("routes.mustache", packagePath + File.separator + srcBasePath + File.separator + "app"  + File.separator + "Http", "routes.php"));
        
        supportingFiles.add(new SupportingFile("Controller.php", packagePath + File.separator + srcBasePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Controllers" + File.separator, "Controller.php"));
        supportingFiles.add(new SupportingFile("Authenticate.php", packagePath + File.separator + srcBasePath + File.separator + "app"  + File.separator + "Http" + File.separator + "Middleware" + File.separator, "Authenticate.php"));
        
    }

    // override with any special post-processing
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");

        for (CodegenOperation op : operations) {
            op.httpMethod = op.httpMethod.toLowerCase();
            // check to see if the path contains ".", which is not supported by Lumen
            // ref: https://github.com/swagger-api/swagger-codegen/issues/6897
            if (op.path != null && op.path.contains(".")) {
                throw new IllegalArgumentException("'.' (dot) is not supported by PHP Lumen. Please refer to https://github.com/swagger-api/swagger-codegen/issues/6897 for more info.");
            }
        }
        
        // sort the endpoints in ascending to avoid the route priority issure. 
        // https://github.com/swagger-api/swagger-codegen/issues/2643
        Collections.sort(operations, new Comparator<CodegenOperation>() {
            @Override
            public int compare(CodegenOperation lhs, CodegenOperation rhs) {
                return lhs.path.compareTo(rhs.path);
            }
        });

        return objs;
    }
}
