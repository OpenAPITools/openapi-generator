package io.swagger.codegen.languages;

import java.io.File;
import java.io.IOException;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.util.Json;
import org.apache.commons.io.FileUtils;

public class JavaJAXRSSpecServerCodegen extends AbstractJavaJAXRSServerCodegen
{	
	public JavaJAXRSSpecServerCodegen()
	{
        super();
        invokerPackage = "io.swagger.api";
        artifactId = "swagger-jaxrs-server";
        outputFolder = "generated-code/JavaJaxRS-Spec";

        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        apiPackage = "io.swagger.api";
        modelPackage = "io.swagger.model";

        apiTestTemplateFiles.clear(); // TODO: add api test template
        modelTestTemplateFiles.clear(); // TODO: add model test template

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        additionalProperties.put("title", title);

        typeMapping.put("date", "LocalDate");
        typeMapping.put("DateTime", "javax.xml.datatype.XMLGregorianCalendar"); // Map DateTime fields to Java standart class 'XMLGregorianCalendar'

        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        super.embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "spec";

        for ( int i = 0; i < cliOptions.size(); i++ ) {
            if ( CodegenConstants.LIBRARY.equals(cliOptions.get(i).getOpt()) ) {
                cliOptions.remove(i);
                break;
            }
        }
                
        CliOption library = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        library.setDefault(DEFAULT_LIBRARY);

        Map<String, String> supportedLibraries = new LinkedHashMap<String,String>();

        supportedLibraries.put(DEFAULT_LIBRARY, "JAXRS");
        library.setEnum(supportedLibraries);

        cliOptions.add(library);
	}
	
	@Override
	public void processOpts()
	{
		super.processOpts();

		supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen
        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        
        writeOptional(outputFolder, new SupportingFile("RestApplication.mustache",
                (sourceFolder + '/' + invokerPackage).replace(".", "/"), "RestApplication.java"));
        
	} 

	@Override
	public String getName()
	{
		return "jaxrs-spec";
	}

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        String basePath = resourcePath;
        if (basePath.startsWith("/")) {
            basePath = basePath.substring(1);
        }
        int pos = basePath.indexOf("/");
        if (pos > 0) {
            basePath = basePath.substring(0, pos);
        }

        if (basePath == "") {
            basePath = "default";
        } else {
            if (co.path.startsWith("/" + basePath)) {
                co.path = co.path.substring(("/" + basePath).length());
            }
            co.subresourceOperation = !co.path.isEmpty();
        }
        List<CodegenOperation> opList = operations.get(basePath);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(basePath, opList);
        }
        opList.add(co);
        co.baseName = basePath;
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
    public void preprocessSwagger(Swagger swagger) {
		//copy input swagger to output folder 
    	try {
			String swaggerJson = Json.pretty(swagger);
            FileUtils.writeStringToFile(new File(outputFolder + File.separator + "swagger.json"), swaggerJson);
		} catch (IOException e) {
            throw new RuntimeException(e.getMessage(), e.getCause());
		}
		super.preprocessSwagger(swagger);

    }
    @Override
    public String getHelp()
    {
        return "Generates a Java JAXRS Server according to JAXRS 2.0 specification.";
    }
}
