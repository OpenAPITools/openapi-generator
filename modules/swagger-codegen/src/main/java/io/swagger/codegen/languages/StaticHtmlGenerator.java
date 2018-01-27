package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenResponse;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Info;
import io.swagger.models.Model;
import io.swagger.models.Swagger;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.codegen.utils.Markdown;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import com.samskivert.mustache.Escapers;
import com.samskivert.mustache.Mustache.Compiler;

import io.swagger.codegen.utils.Markdown;

public class StaticHtmlGenerator extends DefaultCodegen implements CodegenConfig {
    protected String invokerPackage = "io.swagger.client";
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-client";
    protected String artifactVersion = "1.0.0";

    public StaticHtmlGenerator() {
        super();
        outputFolder = "docs";
        embeddedTemplateDir = templateDir = "htmlDocs";

        defaultIncludes = new HashSet<String>();

        cliOptions.add(new CliOption("appName", "short name of the application"));
        cliOptions.add(new CliOption("appDescription", "description of the application"));
        cliOptions.add(new CliOption("infoUrl", "a URL where users can get more information about the application"));
        cliOptions.add(new CliOption("infoEmail", "an email address to contact for inquiries about the application"));
        cliOptions.add(new CliOption("licenseInfo", "a short description of the license"));
        cliOptions.add(new CliOption("licenseUrl", "a URL pointing to the full license"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
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
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        supportingFiles.add(new SupportingFile("index.mustache", "", "index.html"));
        reservedWords = new HashSet<String>();

        languageSpecificPrimitives = new HashSet<String>();
        importMapping = new HashMap<String, String>();
    }


    
    /**
     * Convert Markdown (CommonMark) to HTML. This class also disables normal HTML
     * escaping in the Mustache engine (see {@link DefaultCodegen#processCompiler(Compiler)} above.)
     */
   @Override
    public String escapeText(String input) {
        // newline escaping disabled for HTML documentation for markdown to work correctly
        return toHtml(input);
    }

  @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "html";
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
            for (CodegenResponse response : op.responses) {
                if ("0".equals(response.code)) {
                    response.code = "default";
                }
            }
        }
        return objs;
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

      /**
     * Markdown conversion emits HTML and by default, the Mustache
     * {@link Compiler} will escape HTML. For example a summary
     * <code>"Text with **bold**"</code> is converted from Markdown to HTML as
     * <code>"Text with &lt;strong&gt;bold&lt;/strong&gt; text"</code> and then
     * the default compiler with HTML escaping on turns this into
     * <code>"Text with &amp;lt;strong&amp;gt;bold&amp;lt;/strong&amp;gt; text"</code>.
     * Here, we disable escaping by setting the compiler to {@link Escapers#NONE
     * Escapers.NONE}
     */
    @Override
    public Compiler processCompiler(Compiler compiler) {
        return compiler.withEscaper(Escapers.NONE);
    }

    private Markdown markdownConverter = new Markdown();

    private static final boolean CONVERT_TO_MARKDOWN_VIA_ESCAPE_TEXT = false;

    /**
     * Convert Markdown text to HTML
     * @param input text in Markdown; may be null.
     * @return the text, converted to Markdown. For null input, "" is returned.
     */
    public String toHtml(String input) {
        if (input == null)
            return "";
        return markdownConverter.toHtml(input);
    }

    // DefaultCodegen converts model names to UpperCamelCase
    // but for static HTML, we want the names to be preserved as coded in the OpenApi
    // so HTML links work
    @Override
    public String toModelName(final String name) {
        return name;
    }

    public void preprocessSwagger(Swagger swagger) {
        Info info = swagger.getInfo();
        info.setDescription(toHtml(info.getDescription()));
        info.setTitle(toHtml(info.getTitle()));
        Map<String, Model> models = swagger.getDefinitions();
        for (Model model : models.values()) {
            model.setDescription(toHtml(model.getDescription()));
            model.setTitle(toHtml(model.getTitle()));
        }
    }

    // override to post-process any parameters
    public void postProcessParameter(CodegenParameter parameter) {
        parameter.description = toHtml(parameter.description);
        parameter.unescapedDescription = toHtml(
                parameter.unescapedDescription);
    }

    // override to post-process any model properties
    public void postProcessModelProperty(CodegenModel model,
            CodegenProperty property) {
        property.description = toHtml(property.description);
        property.unescapedDescription = toHtml(
                property.unescapedDescription);
    }

}
