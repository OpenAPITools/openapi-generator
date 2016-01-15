
package io.swagger.codegen.languages;

import java.io.File;

import org.apache.commons.lang.WordUtils;

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
	public String toApiName(String name)
	{
		String computedName = name;
		computedName = computedName.replace('-', ' ');
		computedName = WordUtils.capitalize(computedName);
		computedName = computedName.replaceAll("\\s", "");
		computedName = super.toApiName(computedName);
		return computedName;
	}

	@Override
	public String getName()
	{
		return "cxf";
	}
}
