
package io.swagger.codegen.languages;

import java.io.File;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.models.Operation;

public class JavaCXFServerCodegen extends AbstractJavaJAXRSServerCodegen
{	
	public JavaCXFServerCodegen()
	{
        super();
        sourceFolder = "gen" + File.separator + "java";
        outputFolder = "generated-code/JavaJaxRS-CXF";
        apiTestTemplateFiles.clear(); // TODO: add test template

        //TODO add auto-generated pom.xml for maven
        //apiTemplateFiles.put("pom.mustache", "pom.xml");

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");


        typeMapping.put("date", "LocalDate");
        typeMapping.put("DateTime", "javax.xml.datatype.XMLGregorianCalendar"); // Map DateTime fields to Java standart class 'XMLGregorianCalendar'

        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "cxf";

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
    }
    
    @Override
    public String getHelp()
    {
        return "Generates a Java JAXRS Server application based on Apache CXF framework.";
    }
}
