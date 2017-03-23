package io.swagger.codegen.languages;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ElixirClientCodegen extends DefaultCodegen implements CodegenConfig {
    // source folder where to write the files
    protected String sourceFolder = "lib";
    protected String apiVersion = "1.0.0";

    String supportedElixirVersion = "1.4";
    List<String> extraApplications = Arrays.asList(":logger");
    List<String> deps = Arrays.asList(
            "{:tesla, \"~> 0.5.0\"}",
            "{:poison, \">= 1.0.0\"}"
    );


    public ElixirClientCodegen() {
        super();

        // set the output folder here
        outputFolder = "generated-code/elixir";

        /**
         * Models.  You can write model files using the modelTemplateFiles map.
         * if you want to create one template for file, you can do so here.
         * for multiple files for model, just put another entry in the `modelTemplateFiles` with
         * a different extension
         */
        modelTemplateFiles.put(
                "model.mustache", // the template to use
                ".ex");       // the extension for each file to write

        /**
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        apiTemplateFiles.put(
                "api.mustache",   // the template to use
                ".ex");       // the extension for each file to write

        /**
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        templateDir = "elixir";

        /**
         * Reserved words.  Override this with reserved words specific to your language
         */
        reservedWords = new HashSet<String>(
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
        supportingFiles.add(new SupportingFile("README.md.mustache",   // the input template or file
                "",                                                       // the destination folder, relative `outputFolder`
                "README.md")                                          // the output file
        );
        supportingFiles.add(new SupportingFile("config.exs.mustache",
                "config",
                "config.exs")
        );
        supportingFiles.add(new SupportingFile("mix.exs.mustache",
                "",
                "mix.exs")
        );
        supportingFiles.add(new SupportingFile("test_helper.exs.mustache",
                "test",
                "test_helper.exs")
        );

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
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see io.swagger.codegen.CodegenType
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
        return "elixir";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates an elixir client library (alpha).";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        additionalProperties.put("supportedElixirVersion", supportedElixirVersion);
        additionalProperties.put("extraApplications", join(",", extraApplications));
        additionalProperties.put("deps", deps);
        additionalProperties.put("underscored", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                writer.write(underscored(fragment.execute()));
            }
        });
        additionalProperties.put("modulized", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                writer.write(modulized(fragment.execute()));
            }
        });
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) super.postProcessOperations(objs).get("operations");
        List<CodegenOperation> os = (List<CodegenOperation>) operations.get("operation");
        List<ExtendedCodegenOperation> newOs = new ArrayList<ExtendedCodegenOperation>();
        Pattern pattern = Pattern.compile("(.*)\\{([^\\}]+)\\}(.*)");
        for (CodegenOperation o : os) {
            ArrayList<String> pathTemplateNames = new ArrayList<String>();
            Matcher matcher = pattern.matcher(o.path);
            StringBuffer buffer = new StringBuffer();
            while (matcher.find()) {
                String pathTemplateName = matcher.group(2);
                matcher.appendReplacement(buffer, "$1" + "#{" + underscore(pathTemplateName) + "}" + "$3");
                pathTemplateNames.add(pathTemplateName);
            }
            ExtendedCodegenOperation eco = new ExtendedCodegenOperation(o);
            if (buffer.toString().isEmpty()) {
                eco.setReplacedPathName(o.path);
            } else {
                eco.setReplacedPathName(buffer.toString());
            }
            eco.setPathTemplateNames(pathTemplateNames);
            newOs.add(eco);
        }
        operations.put("operation", newOs);
        return objs;
    }

    // We should use String.join if we can use Java8
    String join(CharSequence charSequence, Iterable<String> iterable) {
        StringBuilder buf = new StringBuilder();
        for (String str : iterable) {
            if (0 < buf.length()) {
                buf.append((charSequence));
            }
            buf.append(str);
        }
        return buf.toString();
    }

    String underscored(String words) {
        ArrayList<String> underscoredWords = new ArrayList<String>();
        for (String word : words.split(" ")) {
            underscoredWords.add(underscore(word));
        }
        return join("_", underscoredWords);
    }

    String modulized(String words) {
        ArrayList<String> modulizedWords = new ArrayList<String>();
        for (String word : words.split(" ")) {
            modulizedWords.add(camelize(word));
        }
        return join("", modulizedWords);
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
        return outputFolder + "/" + sourceFolder + "/" + underscored((String) additionalProperties.get("appName")) + "/" + "model";
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + underscored((String) additionalProperties.get("appName")) + "/" + "api";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "Default";
        }
        return initialCaps(name);
    }

    @Override
    public String toApiFilename(String name) {
        return snakeCase(name);
    }

    @Override
    public String toModelFilename(String name) {
        return snakeCase(name);
    }

    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
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
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type))
                return toModelName(type);
        } else
            type = swaggerType;
        return toModelName(type);
    }

    class ExtendedCodegenOperation extends CodegenOperation {
        private List<String> pathTemplateNames = new ArrayList<String>();
        private String replacedPathName;

        public ExtendedCodegenOperation(CodegenOperation o) {
            super();

            // Copy all fields of CodegenOperation
            this.responseHeaders.addAll(o.responseHeaders);
            this.hasAuthMethods = o.hasAuthMethods;
            this.hasConsumes = o.hasConsumes;
            this.hasProduces = o.hasProduces;
            this.hasParams = o.hasParams;
            this.hasOptionalParams = o.hasOptionalParams;
            this.returnTypeIsPrimitive = o.returnTypeIsPrimitive;
            this.returnSimpleType = o.returnSimpleType;
            this.subresourceOperation = o.subresourceOperation;
            this.isMapContainer = o.isMapContainer;
            this.isListContainer = o.isListContainer;
            this.isMultipart = o.isMultipart;
            this.hasMore = o.hasMore;
            this.isResponseBinary = o.isResponseBinary;
            this.hasReference = o.hasReference;
            this.isRestfulIndex = o.isRestfulIndex;
            this.isRestfulShow = o.isRestfulShow;
            this.isRestfulCreate = o.isRestfulCreate;
            this.isRestfulUpdate = o.isRestfulUpdate;
            this.isRestfulDestroy = o.isRestfulDestroy;
            this.isRestful = o.isRestful;
            this.path = o.path;
            this.operationId = o.operationId;
            this.returnType = o.returnType;
            this.httpMethod = o.httpMethod;
            this.returnBaseType = o.returnBaseType;
            this.returnContainer = o.returnContainer;
            this.summary = o.summary;
            this.unescapedNotes = o.unescapedNotes;
            this.notes = o.notes;
            this.baseName = o.baseName;
            this.defaultResponse = o.defaultResponse;
            this.discriminator = o.discriminator;
            this.consumes = o.consumes;
            this.produces = o.produces;
            this.bodyParam = o.bodyParam;
            this.allParams = o.allParams;
            this.bodyParams = o.bodyParams;
            this.pathParams = o.pathParams;
            this.queryParams = o.queryParams;
            this.headerParams = o.headerParams;
            this.formParams = o.formParams;
            this.authMethods = o.authMethods;
            this.tags = o.tags;
            this.responses = o.responses;
            this.imports = o.imports;
            this.examples = o.examples;
            this.externalDocs = o.externalDocs;
            this.vendorExtensions = o.vendorExtensions;
            this.nickname = o.nickname;
            this.operationIdLowerCase = o.operationIdLowerCase;
            this.operationIdCamelCase = o.operationIdCamelCase;
        }

        public List<String> getPathTemplateNames() {
            return pathTemplateNames;
        }

        public void setPathTemplateNames(List<String> pathTemplateNames) {
            this.pathTemplateNames = pathTemplateNames;
        }

        public String getReplacedPathName() {
            return replacedPathName;
        }

        public void setReplacedPathName(String replacedPathName) {
            this.replacedPathName = replacedPathName;
        }
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // no need to escape as Elixir does not support multi-line comments
        return input;
    }
}
