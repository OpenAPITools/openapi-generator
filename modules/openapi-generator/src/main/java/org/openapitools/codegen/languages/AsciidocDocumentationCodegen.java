package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import io.swagger.v3.oas.models.OpenAPI;

/** basic asciidoc markup generator.
 * @see <a href="https://asciidoctor.org">asciidoctor</a> 
  */
public class AsciidocDocumentationCodegen extends DefaultCodegen implements CodegenConfig {

	private static final Logger LOGGER = LoggerFactory.getLogger(AsciidocDocumentationCodegen.class);
    
	/**
	 * Lambda emitting an asciidoc "include::filename.adoc[]" if file is found in path. 
	 * Use:
	 * <pre>{{#includeMarkup}}{{name}}/description.adoc{{/includeMarkup}}</pre>
	 */
	public class IncludeMarkupLambda implements Mustache.Lambda {

		/** {{includeMarkup}} */
	    public static final String includeMarkupTag = "includemarkup";
	    
		private CodegenConfig config;
		
		private long includeCount = 0;
		private long notFoundCount = 0;
	    
	    public IncludeMarkupLambda(final CodegenConfig config) {
	    	this.config = config;
		}
	    
	    public String resetCounter( ) {
	    	String msg = "included:" + includeCount + " notFound: " + notFoundCount;
	    	includeCount = 0;
	    	notFoundCount = 0;
	    	return msg;
	    }

	    @Override
	    public void execute(final Template.Fragment frag, final Writer out) throws IOException {
	        
	    	String basePath = config.templateDir() != null ?  config.templateDir() : ".";
	    	if( !Files.isDirectory(Paths.get(config.templateDir()))) {
	    		LOGGER.warn("base part for include markup lambda not found: " + basePath);
	    	};
	    	
	    	final String relativeFileName = frag.execute();
	    	final Path filePathToInclude = Paths.get(basePath, relativeFileName);
	        
	        if(Files.isRegularFile(filePathToInclude)) {
		        LOGGER.debug("including " + ++includeCount + ". file into markup: " + filePathToInclude.toAbsolutePath().toString());
		        out.write("\ninclude::" + relativeFileName + "[]\n");	
	        } else {
		        LOGGER.debug(++notFoundCount + ". file not found, skip include into markup: " + filePathToInclude.toAbsolutePath().toString());
		        out.write("\n// markup not included, not found: include::" + relativeFileName + "[]\n");	
	        }	        
	    }
	}
	
    protected String invokerPackage = "org.openapitools.client";
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";
    
    private IncludeMarkupLambda includeMarkupLambda;

    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    public String getName() {
        return "asciidoc";
    }

    public String getHelp() {
        return "Generates asciidoc markup based documentation.";
    }

    public AsciidocDocumentationCodegen() {
        super();

        LOGGER.trace("start asciidoc codegen");
        
        outputFolder = "generated-code" + File.separator + "asciidoc";
        embeddedTemplateDir = templateDir = "asciidoc-documentation";
                
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

        additionalProperties.put("appName", "OpenAPI Sample description");
        additionalProperties.put("appDescription", "A sample OpenAPI documentation");
        additionalProperties.put("infoUrl", "https://openapi-generator.tech");
        additionalProperties.put("infoEmail", "team@openapitools.org");
        additionalProperties.put("licenseInfo", "All rights reserved");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        supportingFiles.add(new SupportingFile("index.mustache", "", "index.adoc"));
        reservedWords = new HashSet<String>();

        languageSpecificPrimitives = new HashSet<String>();
        importMapping = new HashMap<String, String>();
        
    }
    
    @Override
    public String escapeQuotationMark(String input) {
        return input; // just return the original string
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input; // just return the original string
    }
    
    @Override
    public void processOpts() {
        super.processOpts();
        // addMustacheLambdas() does not work.
        this.includeMarkupLambda = new IncludeMarkupLambda(this);
        additionalProperties.put(IncludeMarkupLambda.includeMarkupTag, this.includeMarkupLambda );
    }

    @Override
    public void processOpenAPI(OpenAPI openAPI) {
    	if(this.includeMarkupLambda != null) {
    		LOGGER.info(
    				IncludeMarkupLambda.includeMarkupTag + 
    				": " + 
    				this.includeMarkupLambda.resetCounter());
    	}
		super.processOpenAPI(openAPI);
	}

}
