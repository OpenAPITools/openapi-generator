
package io.swagger.codegen.languages;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CodegenOperation;
import io.swagger.models.Operation;

// TODO: create an abstract JavaJAXRSServerCodegen that both Jersey1 & CXF will extends.
public class JavaJaxRSCXFServerCodegen extends JavaJaxRSJersey1ServerCodegen
{
	public JavaJaxRSCXFServerCodegen()
	{
		super();
		super.embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "cxf";
		// We decided to just provide the CXF interface and let the user create his own files to implement it
		super.apiTemplateFiles.remove("apiService.mustache");
		super.apiTemplateFiles.remove("apiServiceImpl.mustache");
		super.apiTemplateFiles.remove("apiServiceFactory.mustache");
	}

	@Override
	public void processOpts()
	{
		super.processOpts();
		sourceFolder = "gen" + File.separator + "java";

		modelTemplateFiles.clear();
		modelTemplateFiles.put("entityModel.mustache", ".java");

		supportingFiles.clear();
	} 

	@Override
	public String getName()
	{
		return "cxf";
	}
	

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);        
        co.subresourceOperation = !co.path.isEmpty();
    }
}
