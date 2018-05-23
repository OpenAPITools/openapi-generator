/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.ModelUtils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.*;

import org.apache.commons.lang3.StringEscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class BashClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(BashClientCodegen.class);

    protected String apiVersion = "1.0.0";

    protected String curlOptions;
    protected boolean processMarkdown = false;
    protected String scriptName = "client.sh";
    protected boolean generateBashCompletion = false;
    protected boolean generateZshCompletion = false;
    protected String hostEnvironmentVariable;
    protected String basicAuthEnvironmentVariable;
    protected String apiKeyAuthEnvironmentVariable;
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    public static final String CURL_OPTIONS = "curlOptions";
    public static final String PROCESS_MARKDOWN = "processMarkdown";
    public static final String SCRIPT_NAME = "scriptName";
    public static final String
            GENERATE_BASH_COMPLETION = "generateBashCompletion";
    public static final String
            GENERATE_ZSH_COMPLETION = "generateZshCompletion";
    public static final String
            HOST_ENVIRONMENT_VARIABLE_NAME = "hostEnvironmentVariable";
    public static final String
            BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME = "basicAuthEnvironmentVariable";
    public static final String
            APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME = "apiKeyAuthEnvironmentVariable";

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     */
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by
     * the generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "bash";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with
     * help tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a Bash client script based on cURL.";
    }

    public BashClientCodegen() {
        super();

        /**
         * Set the output folder here
         */
        outputFolder = "generated-code/bash";

        /**
         * No model files.
         */
        modelTemplateFiles.clear();


        /**
         * No API files.
         */
        apiTemplateFiles.clear();


        /**
         * docs files.
         */
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");


        /**
         * Templates location for client script and bash completion template.
         */
        embeddedTemplateDir = templateDir = "bash";


        /**
         * Allow the user to force the script to always include certain cURL
         * comamnds
         */
        cliOptions.add(CliOption.newString(CURL_OPTIONS, "Default cURL options"));
        cliOptions.add(CliOption.newBoolean(PROCESS_MARKDOWN,
                "Convert all Markdown Markup into terminal formatting"));
        cliOptions.add(CliOption.newString(SCRIPT_NAME,
                "The name of the script that will be generated " +
                        "(e.g. petstore-cli)"));
        cliOptions.add(CliOption.newBoolean(GENERATE_BASH_COMPLETION,
                "Whether to generate the Bash completion script"));
        cliOptions.add(CliOption.newBoolean(GENERATE_ZSH_COMPLETION,
                "Whether to generate the Zsh completion script"));
        cliOptions.add(CliOption.newString(HOST_ENVIRONMENT_VARIABLE_NAME,
                "Name of environment variable where host can be defined " +
                        "(e.g. PETSTORE_HOST='http://api.openapitools.org:8080')"));
        cliOptions.add(CliOption.newString(BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME,
                "Name of environment variable where username and password "
                        +
                        "can be defined (e.g. PETSTORE_CREDS='username:password')"));
        cliOptions.add(CliOption.newBoolean(APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME,
                "Name of environment variable where API key "
                        +
                        "can be defined (e.g. PETSTORE_APIKEY='kjhasdGASDa5asdASD')"));

        /**
         * Bash reserved words.
         */
        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "case",
                        "do",
                        "done",
                        "elif",
                        "else",
                        "esac",
                        "fi",
                        "for",
                        "function",
                        "if",
                        "in",
                        "select",
                        "then",
                        "time",
                        "until",
                        "while")
        );

        typeMapping.clear();
        typeMapping.put("array", "array");
        typeMapping.put("map", "map");
        typeMapping.put("List", "array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "integer");
        typeMapping.put("float", "float");
        typeMapping.put("number", "integer");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "string");
        typeMapping.put("long", "integer");
        typeMapping.put("short", "integer");
        typeMapping.put("char", "string");
        typeMapping.put("double", "float");
        typeMapping.put("object", "map");
        typeMapping.put("integer", "integer");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("file", "binary");
        typeMapping.put("binary", "binary");
        typeMapping.put("UUID", "string");

        /**
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files.
         */
        additionalProperties.put("apiVersion", apiVersion);
        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        /**
         * Language Specific Primitives.  These types will not trigger imports by
         * the client generator
         */
        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("array");
        languageSpecificPrimitives.add("map");
        languageSpecificPrimitives.add("boolean");
        languageSpecificPrimitives.add("integer");
        languageSpecificPrimitives.add("float");
        languageSpecificPrimitives.add("string");
        languageSpecificPrimitives.add("binary");
    }


    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CURL_OPTIONS)) {
            setCurlOptions(additionalProperties.get(CURL_OPTIONS).toString());
            additionalProperties.put("x-codegen-curl-options", this.curlOptions);
        }

        if (additionalProperties.containsKey(PROCESS_MARKDOWN)) {
            setProcessMarkdown(convertPropertyToBooleanAndWriteBack(PROCESS_MARKDOWN));
        }

        if (additionalProperties.containsKey(GENERATE_BASH_COMPLETION)) {
            setGenerateBashCompletion(convertPropertyToBooleanAndWriteBack(GENERATE_BASH_COMPLETION));
        }

        if (additionalProperties.containsKey(GENERATE_ZSH_COMPLETION)) {
            setGenerateZshCompletion(convertPropertyToBooleanAndWriteBack(GENERATE_ZSH_COMPLETION));
        }

        if (additionalProperties.containsKey(SCRIPT_NAME)) {
            setScriptName(additionalProperties.get(SCRIPT_NAME).toString());
        }
        additionalProperties.put("x-codegen-script-name", scriptName);

        if (additionalProperties.containsKey(HOST_ENVIRONMENT_VARIABLE_NAME)) {
            setHostEnvironmentVariable(
                    additionalProperties.get(HOST_ENVIRONMENT_VARIABLE_NAME).toString());
            additionalProperties.put("x-codegen-host-env", hostEnvironmentVariable);
        }

        if (additionalProperties.containsKey(BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME)) {
            setBasicAuthEnvironmentVariable(
                    additionalProperties.get(BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME).toString());
            additionalProperties.put("x-codegen-basicauth-env", basicAuthEnvironmentVariable);
        }

        if (additionalProperties.containsKey(APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME)) {
            setApiKeyAuthEnvironmentVariable(
                    additionalProperties.get(APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME).toString());
            additionalProperties.put("x-codegen-apikey-env", apiKeyAuthEnvironmentVariable);
        }

        supportingFiles.add(new SupportingFile(
                "client.mustache", "", scriptName));
        supportingFiles.add(new SupportingFile(
                "bash-completion.mustache", "", scriptName + ".bash-completion"));
        supportingFiles.add(new SupportingFile(
                "zsh-completion.mustache", "", "_" + scriptName));
        supportingFiles.add(new SupportingFile(
                "README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile(
                "Dockerfile.mustache", "", "Dockerfile"));
    }

    public void setCurlOptions(String curlOptions) {
        this.curlOptions = curlOptions;
    }

    public void setProcessMarkdown(boolean processMarkdown) {
        this.processMarkdown = processMarkdown;
    }

    public void setScriptName(String scriptName) {
        this.scriptName = scriptName;
    }

    public void setGenerateBashCompletion(boolean generateBashCompletion) {
        this.generateBashCompletion = generateBashCompletion;
    }

    public void setGenerateZshCompletion(boolean generateZshCompletion) {
        this.generateZshCompletion = generateZshCompletion;
    }

    public void setHostEnvironmentVariable(String hostEnvironmentVariable) {
        this.hostEnvironmentVariable = hostEnvironmentVariable;
    }

    public void setBasicAuthEnvironmentVariable(String
                                                        basicAuthEnvironmentVariable) {
        this.basicAuthEnvironmentVariable = basicAuthEnvironmentVariable;
    }

    public void setApiKeyAuthEnvironmentVariable(String
                                                         apiKeyAuthEnvironmentVariable) {
        this.apiKeyAuthEnvironmentVariable = apiKeyAuthEnvironmentVariable;
    }


    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle
     * escaping those terms here. This logic is only called if a variable
     * matches the reserved words.
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;  // add an underscore to the name
    }

    /**
     * Location to write model files.  You can use the modelPackage() as defined
     * when the class is instantiated.
     */
    public String modelFileFolder() {
        return outputFolder;
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when
     * the class is instantiated.
     */
    @Override
    public String apiFileFolder() {
        return outputFolder;
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    /**
     * Optional - type declaration. This is a String which is used by the
     * templates to instantiate your types. There is typically special handling
     * for different property types
     *
     * @return a string value used as the `dataType` field for model templates,
     * `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = (Schema) p.getAdditionalProperties();
            return getSchemaType(p) + "[String, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    /**
     * Optional - schema type conversion. This is used to map OpenAPI types in
     * a `Property` into either language specific types via `typeMapping` or into
     * complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     * @see io.swagger.v3.oas.models.media.Schema
     */
    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type))
                return type;
        } else {
            type = schemaType;
        }
        return toModelName(type);
    }


    /**
     * Convert OpenAPI Parameter object to Codegen Parameter object
     *
     * @param param   OpenAPI parameter object
     * @param imports set of imports for library/package/module
     * @return Codegen Parameter object
     */
    @Override
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {

        CodegenParameter p = super.fromParameter(param, imports);

        if (p.isContainer) { // array or map
            /**
             * Currently it's not possible to specify in the codegen other collection
             * formats than 'multi'
             */
            if (!StringUtils.isEmpty(p.collectionFormat)) {
                if (Boolean.TRUE.equals(p.exclusiveMaximum)) {
                    p.vendorExtensions.put("x-codegen-collection-max-items",
                            p.maxItems);
                }

                if (Boolean.TRUE.equals(p.exclusiveMinimum)) {
                    p.vendorExtensions.put("x-codegen-collection-min-items",
                            p.minItems);
                }

                if ("multi".equals(p.collectionFormat) && Boolean.TRUE.equals(p.isQueryParam)) {
                    //'multi' is only supported for query parameters
                    p.vendorExtensions.put("x-codegen-collection-multi", true);
                } else if ("csv".equals(p.collectionFormat)) {
                    p.vendorExtensions.put("x-codegen-collection-csv", true);
                } else if ("ssv".equals(p.collectionFormat)) {
                    p.vendorExtensions.put("x-codegen-collection-ssv", true);
                } else if ("tsv".equals(p.collectionFormat)) {
                    p.vendorExtensions.put("x-codegen-collection-tsv", true);
                } else if ("pipes".equals(p.collectionFormat)) {
                    p.vendorExtensions.put("x-codegen-collection-pipes", true);
                } else {
                    LOGGER.warn("Unsupported collection format in Bash generator: " + p.collectionFormat);
                }
            }
        }

        return p;

    }

    /**
     * Override with any special text escaping logic
     */
    @SuppressWarnings("static-method")
    public String escapeText(String input) {
        if (input == null) {
            return input;
        }

        /**
         * Trim the input text always.
         */
        String result = input.trim();

        /**
         * remove standalone '\'
         *
         * replace " with \"
         * outter unescape to retain the original multi-byte characters
         */
        result = escapeUnsafeCharacters(
                StringEscapeUtils.unescapeJava(
                        StringEscapeUtils.escapeJava(result).replace("\\/", "/"))
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\""));

        if (this.processMarkdown) {

            /**
             * Convert markdown strong **Bold text**  and __Bold text__
             * to bash bold control sequences (tput bold)
             */
            result = result.replaceAll("(?m)(^|\\s)\\*{2}([\\w\\d ]+)\\*{2}($|\\s)",
                    "\\$\\(tput bold\\) $2 \\$\\(tput sgr0\\)");

            result = result.replaceAll("(?m)(^|\\s)_{2}([\\w\\d ]+)_{2}($|\\s)",
                    "\\$\\(tput bold\\) $2 \\$\\(tput sgr0\\)");
            /**
             * Convert markdown *Italics text* and _Italics text_ to bash dim
             * control sequences (tput dim)
             */
            result = result.replaceAll("(?m)(^|\\s)\\*{1}([\\w\\d ]+)\\*{1}($|\\s)",
                    "\\$\\(tput dim\\) $2 \\$\\(tput sgr0\\)");

            result = result.replaceAll("(?m)(^|\\s)_{1}([\\w\\d ]+)_{1}($|\\s)",
                    "\\$\\(tput dim\\) $2 \\$\\(tput sgr0\\)");


            /**
             * Convert all markdown section 1 level headers with bold
             */
            result = result.replaceAll("(?m)^\\#\\s+(.+)$",
                    "\n\\$\\(tput bold\\)\\$\\(tput setaf 7\\)"
                            + "$1\\$\\(tput sgr0\\)");

            /**
             * Convert all markdown section 2 level headers with bold
             */
            result = result.replaceAll("(?m)^\\#\\#\\s+(.+)$",
                    "\n\\$\\(tput bold\\)\\$\\(tput setaf 7\\)"
                            + "$1\\$\\(tput sgr0\\)");

            /**
             * Convert all markdown section 3 level headers with bold
             */
            result = result.replaceAll("(?m)^\\#\\#\\#\\s+(.+)$",
                    "\n\\$\\(tput bold\\)\\$\\(tput setaf 7\\)"
                            + "$1\\$\\(tput sgr0\\)");

            /**
             * Convert all markdown code blocks into --- delimited sections
             */
            result = result.replaceAll("(?m)\\s*```.*$",
                    "\n---");

            result = result.replaceAll("(?m)\\s*\\'\\'\\'.*$",
                    "\n---");

            /**
             * Remove any trailing new line at the end of the string
             */
            result = result.replaceAll("\\s+$", "");
        }

        return result;
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input;
    }

    /**
     * Override with any special text escaping logic to handle unsafe
     * characters so as to avoid code injection.
     *
     * @param input String to be cleaned up
     * @return string with unsafe characters removed or escaped
     */
    public String escapeUnsafeCharacters(String input) {

        /**
         * Replace backticks with normal single quotes.
         */
        String result = input.replaceAll("`", "'");

        return result;
    }


    @Override
    public CodegenOperation fromOperation(String path, String httpMethod,
                                          Operation operation,
                                          Map<String, Schema> definitions,
                                          OpenAPI openAPI) {

        CodegenOperation op = super.fromOperation(path, httpMethod, operation,
                definitions, openAPI);

        /**
         * Check if the operation has a Bash codegen specific description
         * for help
         */
        if (op.vendorExtensions.containsKey("x-bash-codegen-description")) {
            String bash_description
                    = (String) op.vendorExtensions.get("x-bash-codegen-description");

            op.vendorExtensions.put("x-bash-codegen-description",
                    escapeText(bash_description));
        }

        /**
         * Check if operation has an 'x-code-samples' vendor extension with
         * Shell example
         */
        if (op.vendorExtensions.containsKey("x-code-samples")) {

            List codesamples = (List) op.vendorExtensions.get("x-code-samples");

            for (Object codesample : codesamples) {
                if (codesample instanceof ObjectNode) {
                    ObjectNode codesample_object = (ObjectNode) codesample;

                    if ((codesample_object.get("lang").asText()).equals("Shell")) {

                        op.vendorExtensions.put("x-bash-codegen-sample",
                                escapeUnsafeCharacters(
                                        codesample_object.get("source").asText()));

                    }
                }
            }
        }

        for (CodegenParameter p : op.bodyParams) {
            if (p.dataType != null && definitions.get(p.dataType) != null) {
                /**
                 * If the operation produces Json and has nonempty example
                 * try to reformat it.
                 */
                if (getConsumesInfo(openAPI, operation) != null
                        && getConsumesInfo(openAPI, operation).contains("application/json")
                        && definitions.get(p.dataType).getExample() != null) {

                    ObjectMapper mapper = new ObjectMapper();
                    try {
                        p.vendorExtensions.put(
                                "x-codegen-body-example",
                                mapper.writerWithDefaultPrettyPrinter().writeValueAsString(
                                        definitions.get(p.dataType).getExample()));
                    } catch (JsonProcessingException e) {
                        e.printStackTrace();
                    }
                } else {
                    /**
                     * Otherwise present whatever is provided as example
                     */
                    p.vendorExtensions.put(
                            "x-codegen-body-example",
                            definitions.get(p.dataType).getExample());
                }
            }
        }

        return op;

    }

    /**
     * Preprocess original properties from the OpenAPI definition where necessary.
     *
     * @param openAPI [description]
     */
    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

      /* TODO need to revise the logic below
      if ("/".equals(openAPI.getServers())) {
          openAPI.setBasePath("");
      }
      */

      /* there should not be a need to get the vendor extension this way
      if(openAPI.getInfo() != null
         && openAPI.getInfo().getVendorExtensions()!=null) {
        String bash_codegen_app_description
          = (String)openAPI.getInfo().getVendorExtensions()
                                            .get("x-bash-codegen-description");

        if (bash_codegen_app_description != null) {
          bash_codegen_app_description
            = escapeText(bash_codegen_app_description);

          additionalProperties.put("x-bash-codegen-app-description",
            bash_codegen_app_description);

        }
      }*/
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            p.example = p.defaultValue;
            return;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("string".equalsIgnoreCase(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("integer".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if ("float".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "3.4";
            }
        } else if ("boolean".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "True";
            }
        } else if ("file".equalsIgnoreCase(type) || "binary".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "BINARY_DATA_HERE";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("date".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("datetime".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "'" + escapeText(example) + "'";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = type;
        } else if ("array".equalsIgnoreCase(type) || "map".equalsIgnoreCase(type)) {
            // skip map/array as it will be handled below
        } else {
            LOGGER.warn("Type " + type + " not handled properly in setParameterExampleValue");
        }

        if (example == null) {
            example = "NULL";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            example = "[" + example + "]";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            example = "{'key': " + example + "}";
        }

        p.example = example;
    }

    @Override
    public String toModelFilename(String name) {
        return initialCaps(name);
    }


}
