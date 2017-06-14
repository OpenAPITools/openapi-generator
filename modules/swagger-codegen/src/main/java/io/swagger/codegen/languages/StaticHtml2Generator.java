package io.swagger.codegen.languages;

import com.samskivert.mustache.Mustache;
import io.swagger.codegen.*;
import io.swagger.models.Info;
import org.yaml.snakeyaml.error.Mark;
import io.swagger.codegen.utils.Markdown;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

public class StaticHtml2Generator extends DefaultCodegen implements CodegenConfig {
    protected String invokerPackage = "io.swagger.client"; // default for Java and Android
    protected String phpInvokerPackage = "Swagger\\Client"; // default for PHP
    protected String packageName = "IO.Swagger"; // default for C#
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-client";
    protected String artifactVersion = "1.0.0";
    protected String jsProjectName;
    protected String jsModuleName;
    protected String perlModuleName = "WWW::SwaggerClient";
    protected String pythonPackageName = "swagger_client";

    public StaticHtml2Generator() {
        super();
        outputFolder = "docs";
        embeddedTemplateDir = templateDir = "htmlDocs2";

        defaultIncludes = new HashSet<String>();

        cliOptions.add(new CliOption("appName", "short name of the application"));
        cliOptions.add(new CliOption("appDescription", "description of the application"));
        cliOptions.add(new CliOption("infoUrl", "a URL where users can get more information about the application"));
        cliOptions.add(new CliOption("infoEmail", "an email address to contact for inquiries about the application"));
        cliOptions.add(new CliOption("licenseInfo", "a short description of the license"));
        cliOptions.add(new CliOption("licenseUrl", "a URL pointing to the full license"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.PHP_INVOKER_PACKAGE, CodegenConstants.PHP_INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.PERL_MODULE_NAME, CodegenConstants.PERL_MODULE_NAME_DESC));
        cliOptions.add(new CliOption(CodegenConstants.PYTHON_PACKAGE_NAME, CodegenConstants.PYTHON_PACKAGE_NAME_DESC));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "C# package name"));
        cliOptions.add(new CliOption(CodegenConstants.GROUP_ID, CodegenConstants.GROUP_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_ID, CodegenConstants.ARTIFACT_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, CodegenConstants.ARTIFACT_VERSION_DESC));
        
        additionalProperties.put("appName", "Swagger Sample");
        additionalProperties.put("appDescription", "A sample swagger server");
        additionalProperties.put("infoUrl", "https://helloreverb.com");
        additionalProperties.put("infoEmail", "hello@helloreverb.com");
        additionalProperties.put("licenseInfo", "All rights reserved");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.PHP_INVOKER_PACKAGE, phpInvokerPackage);
        additionalProperties.put(CodegenConstants.PERL_MODULE_NAME, perlModuleName);
        additionalProperties.put(CodegenConstants.PYTHON_PACKAGE_NAME, pythonPackageName);
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        supportingFiles.add(new SupportingFile("index.mustache", "", "index.html"));
        reservedWords = new HashSet<String>();

        languageSpecificPrimitives = new HashSet<String>();
        importMapping = new HashMap<String, String>();
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "html2";
    }

    @Override
    public String escapeText(String input) {
        // newline escaping disabled for HTML documentation for markdown to work correctly
        return input;
    }

    @Override
    public String getHelp() {
        return "Generates a static HTML file.";
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
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            op.httpMethod = op.httpMethod.toLowerCase();
            for (CodegenResponse response : op.responses){
                if ("0".equals(response.code)){
                    response.code = "default";
                }
            }
            op.formParams = postProcessParameterEnum(op.formParams);
        }
        return objs;
    }

    @Override
    public void preprocessSwagger(Swagger swagger) {
        super.preprocessSwagger(swagger);

        if (swagger.getInfo() != null) {
            Info info = swagger.getInfo();
            if (StringUtils.isBlank(jsProjectName) && info.getTitle() != null) {
                // when jsProjectName is not specified, generate it from info.title
                jsProjectName = sanitizeName(dashize(info.getTitle()));
            }
        }

        // default values
        if (StringUtils.isBlank(jsProjectName)) {
            jsProjectName = "swagger-js-client";
        }
        if (StringUtils.isBlank(jsModuleName)) {
            jsModuleName = camelize(underscore(jsProjectName));
        }

        additionalProperties.put("jsProjectName", jsProjectName);
        additionalProperties.put("jsModuleName", jsModuleName);

        preparHtmlForGlobalDescription(swagger);
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, definitions, swagger);
        if (op.returnType != null) {
            op.returnType = normalizeType(op.returnType);
        }

        //path is an unescaped variable in the mustache template api.mustache line 82 '<&path>'
        op.path = sanitizePath(op.path);

        // Set vendor-extension to be used in template:
        //     x-codegen-hasMoreRequired
        //     x-codegen-hasMoreOptional
        //     x-codegen-hasRequiredParams
        CodegenParameter lastRequired = null;
        CodegenParameter lastOptional = null;
        for (CodegenParameter p : op.allParams) {
            if (p.required) {
                lastRequired = p;
            } else {
                lastOptional = p;
            }
        }
        for (CodegenParameter p : op.allParams) {
            if (p == lastRequired) {
                p.vendorExtensions.put("x-codegen-hasMoreRequired", false);
            } else if (p == lastOptional) {
                p.vendorExtensions.put("x-codegen-hasMoreOptional", false);
            } else {
                p.vendorExtensions.put("x-codegen-hasMoreRequired", true);
                p.vendorExtensions.put("x-codegen-hasMoreOptional", true);
            }
        }
        op.vendorExtensions.put("x-codegen-hasRequiredParams", lastRequired != null);

        op.vendorExtensions.put("x-codegen-httpMethodUpperCase", httpMethod.toUpperCase());

        return op;
    }

    /**
     * Parse Markdown to HTML for the main "Description" attribute
     *
     * @param swagger The base object containing the global description through "Info" class
     * @return Void
     */
    private void preparHtmlForGlobalDescription(Swagger swagger) {
        String currentDescription = swagger.getInfo().getDescription();
        if (currentDescription != null && !currentDescription.isEmpty()) {
            Markdown markInstance = new Markdown();
            swagger.getInfo().setDescription( markInstance.toHtml(currentDescription) );
        } else {
            LOGGER.error("Swagger object description is empty [" + swagger.getInfo().getTitle() + "]");
        }
    }

    /**
     * Format to HTML the enums contained in every operations
     *
     * @param parameterList The whole parameters contained in one operation
     * @return String | Html formated enum
     */
    public List<CodegenParameter> postProcessParameterEnum(List<CodegenParameter> parameterList) {
        String enumFormatted = "";
        for(CodegenParameter parameter : parameterList) {
            if (parameter.isEnum) {
                for (int i = 0; i < parameter._enum.size(); i++) {
                    String spacer = (i == (parameter._enum.size() - 1)) ? " " : ", ";

                    if (parameter._enum.get(i) != null)
                        enumFormatted += "`" + parameter._enum.get(i) + "`" + spacer;
                }
                Markdown markInstance = new Markdown();
                if (!enumFormatted.isEmpty())
                    parameter.vendorExtensions.put("x-eumFormatted", markInstance.toHtml(enumFormatted));
            }
        }
        return parameterList;
    }

    private String sanitizePath(String p) {
        //prefer replace a ', instead of a fuLL URL encode for readability
        return p.replaceAll("'", "%27");
    }

    /**
     * Normalize type by wrapping primitive types with single quotes.
     *
     * @param type Primitive type
     * @return Normalized type
     */
    public String normalizeType(String type) {
        return type.replaceAll("\\b(Boolean|Integer|Number|String|Date)\\b", "'$1'");
    }

    @Override
    public String escapeQuotationMark(String input) {
        // just return the original string
        return input;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // just return the original string
        return input;
    }   
}
