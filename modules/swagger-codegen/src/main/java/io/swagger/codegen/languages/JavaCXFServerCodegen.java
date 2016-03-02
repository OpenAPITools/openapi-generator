
package io.swagger.codegen.languages;

import java.io.File;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.models.Operation;

public class JavaCXFServerCodegen extends AbstractJavaJAXRSServerCodegen
{	
	public JavaCXFServerCodegen()
	{
        super();
        supportsInheritance = true;
        sourceFolder = "src/gen/java";
        invokerPackage = "io.swagger.api";
        artifactId = "swagger-jaxrs-server";
        outputFolder = "generated-code/JavaJaxRS-CXF";

        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        apiPackage = "io.swagger.api";
        modelPackage = "io.swagger.model";

        additionalProperties.put("title", title);

        typeMapping.put("date", "LocalDate");
        typeMapping.put("DateTime", "javax.xml.datatype.XMLGregorianCalendar"); // Map DateTime fields to Java standart class 'XMLGregorianCalendar'

        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        super.embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "cxf";

        for ( int i = 0; i < cliOptions.size(); i++ ) {
            if ( CodegenConstants.LIBRARY.equals(cliOptions.get(i).getOpt()) ) {
                cliOptions.remove(i);
                break;
            }
        }
                
        CliOption library = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        library.setDefault(DEFAULT_LIBRARY);

        Map<String, String> supportedLibraries = new LinkedHashMap<String,String>();

        supportedLibraries.put(DEFAULT_LIBRARY, "CXF");
        library.setEnum(supportedLibraries);

        cliOptions.add(library);
        cliOptions.add(new CliOption(CodegenConstants.IMPL_FOLDER, CodegenConstants.IMPL_FOLDER_DESC));	
        cliOptions.add(new CliOption("title", "a title describing the application"));
	}
	
	@Override
	public void processOpts()
	{
		super.processOpts();
		sourceFolder = "gen" + File.separator + "java";
		supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen
      
		//TODO
		//final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");
        //supportingFiles.add(new SupportingFile("CXF2InterfaceComparator.mustache", invokerFolder, "CXF2InterfaceComparator.java"));
	} 

	@Override
	public String getName()
	{
		return "jaxrs-cxf";
	}

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);        
        co.subresourceOperation = !co.path.isEmpty();
    }
    
    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        model.imports.remove("ApiModelProperty");
        model.imports.remove("ApiModel");
        model.imports.remove("JsonSerialize");
        model.imports.remove("ToStringSerializer");
        model.imports.remove("JsonValue");
        model.imports.remove("JsonProperty");
    }
    
    @Override
    public String getHelp()
    {
        return "Generates a Java JAXRS Server application based on Apache CXF framework.";
    }
}
