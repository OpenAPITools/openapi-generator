package org.openapitools.codegen.languages;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.CodegenType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MicroprofileServerCodegen extends JavaClientCodegen {
	public static final String PROJECT_NAME = "projectName";
	public static final String MICROPROFILE_SERVER = "microprofileServer";
	private final Logger LOGGER = LoggerFactory.getLogger(MicroprofileServerCodegen.class);
	protected boolean microprofileServer = true;

	public MicroprofileServerCodegen() {
		super();

		outputFolder = "generated-code" + File.separator + "microprofile";
		invokerPackage = "org.openapitools.server";
		artifactId = "openapi-microprofile-server";
		//embeddedTemplateDir = templateDir = "microprofile";
		apiPackage = "org.openapitools.server.api";
		modelPackage = "org.openapitools.server.model";
		setLibrary("microprofile");
	}

	public CodegenType getTag() {
		return CodegenType.SERVER;
	}

	public String getName() {
		return "microprofile";
	}

	public String getHelp() {
		return "Generates a microprofile server.";
	}
	@Override
	public void processOpts() {
		super.processOpts();
		additionalProperties.put(MICROPROFILE_SERVER, microprofileServer);
	}

	@Override
	public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
		super.postProcessOperationsWithModels(objs, allModels);
		Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
		if (operations != null) {
			List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
			for (CodegenOperation operation : ops) {
				List<CodegenResponse> responses = operation.responses;
				if (multiple2xxResponsesSpecified(responses)) {
					operation.vendorExtensions.put("x-multiple-2xx-response-operation", true);
				}
			}
		}
		return objs;
	}

	private boolean multiple2xxResponsesSpecified(List<CodegenResponse> responses) {
		int responseIs2xxCount = 0;
		for (CodegenResponse response : responses) {
			if (response.is2xx) {
				responseIs2xxCount++;
			}
		}
		return responseIs2xxCount > 1;
	}
}
