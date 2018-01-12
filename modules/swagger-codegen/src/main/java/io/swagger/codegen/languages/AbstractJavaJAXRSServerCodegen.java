package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenResponse;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.languages.features.BeanValidationFeatures;
import io.swagger.codegen.utils.ModelUtils;
import io.swagger.codegen.utils.URLPathUtil;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public abstract class AbstractJavaJAXRSServerCodegen extends AbstractJavaCodegen implements BeanValidationFeatures {
    /**
     * Name of the sub-directory in "src/main/resource" where to find the
     * Mustache template for the JAX-RS Codegen.
     */
    protected static final String JAXRS_TEMPLATE_DIRECTORY_NAME = "JavaJaxRS";
    protected String implFolder = "src/main/java";
    protected String testResourcesFolder = "src/test/resources";
    protected String title = "Swagger Server";

    protected boolean useBeanValidation = true;

    static Logger LOGGER = LoggerFactory.getLogger(AbstractJavaJAXRSServerCodegen.class);

    public AbstractJavaJAXRSServerCodegen() {
        super();

        sourceFolder = "src/gen/java";
        invokerPackage = "io.swagger.api";
        artifactId = "swagger-jaxrs-server";
        dateLibrary = "legacy"; //TODO: add joda support to all jax-rs

        apiPackage = "io.swagger.api";
        modelPackage = "io.swagger.model";

        additionalProperties.put("title", title);
        // java inflector uses the jackson lib
        additionalProperties.put("jackson", "true");

        cliOptions.add(new CliOption(CodegenConstants.IMPL_FOLDER, CodegenConstants.IMPL_FOLDER_DESC));
        cliOptions.add(new CliOption("title", "a title describing the application"));

        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));
        cliOptions.add(new CliOption("serverPort", "The port on which the server should be started"));
    }


    // ===============
    // COMMONS METHODS
    // ===============

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.IMPL_FOLDER)) {
            implFolder = (String) additionalProperties.get(CodegenConstants.IMPL_FOLDER);
        }

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }

        if (useBeanValidation) {
            writePropertyBack(USE_BEANVALIDATION, useBeanValidation);
        }

    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        if (!this.additionalProperties.containsKey("serverPort")) {
            final URL urlInfo = URLPathUtil.getServerURL(openAPI);
            String port = "8080"; // Default value for a JEE Server
            if ( urlInfo != null && urlInfo.getPort() != 0) {
                port = String.valueOf(urlInfo.getPort());
            }
            this.additionalProperties.put("serverPort", port);
        }

        if (openAPI.getPaths() != null) {
            for (String pathname : openAPI.getPaths().keySet()) {
                PathItem pathItem = openAPI.getPaths().get(pathname);
                final Operation[] operations = ModelUtils.createOperationArray(pathItem);
                for (Operation operation : operations) {
                    if (operation != null && operation.getTags() != null) {
                        List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
                        for (String tag : operation.getTags()) {
                            Map<String, String> value = new HashMap<String, String>();
                            value.put("tag", tag);
                            value.put("hasMore", "true");
                            tags.add(value);
                        }
                        if (tags.size() > 0) {
                            tags.get(tags.size() - 1).remove("hasMore");
                        }
                        if (operation.getTags().size() > 0) {
                            String tag = operation.getTags().get(0);
                            operation.setTags(Arrays.asList(tag));
                        }
                        operation.addExtension("x-tags", tags);
                    }
                }
            }
        }
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        @SuppressWarnings("unchecked")
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if ( operations != null ) {
            @SuppressWarnings("unchecked")
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for ( CodegenOperation operation : ops ) {
                boolean hasConsumes = getBooleanValue(operation, CodegenConstants.HAS_CONSUMES_EXT_NAME);
                if (hasConsumes) {
                    Map<String, String> firstType = operation.consumes.get(0);
                    if (firstType != null) {
                        if ("multipart/form-data".equals(firstType.get("mediaType"))) {
                            operation.getVendorExtensions().put(CodegenConstants.IS_MULTIPART_EXT_NAME, Boolean.TRUE);
                        }
                    }
                }

                boolean isMultipartPost = false;
                List<Map<String, String>> consumes = operation.consumes;
                if(consumes != null) {
                    for(Map<String, String> consume : consumes) {
                        String mt = consume.get("mediaType");
                        if(mt != null) {
                            if(mt.startsWith("multipart/form-data")) {
                                isMultipartPost = true;
                            }
                        }
                    }
                }

                for(CodegenParameter parameter : operation.allParams) {
                    if(isMultipartPost) {
                        parameter.vendorExtensions.put("x-multipart", "true");
                    }
                }

                List<CodegenResponse> responses = operation.responses;
                if ( responses != null ) {
                    for ( CodegenResponse resp : responses ) {
                        if ( "0".equals(resp.code) ) {
                            resp.code = "200";
                        }

                        if (resp.baseType == null) {
                            resp.dataType = "void";
                            resp.baseType = "Void";
                            // set vendorExtensions.x-java-is-response-void to true as baseType is set to "Void"
                            resp.vendorExtensions.put("x-java-is-response-void", true);
                        }

                        if ("array".equals(resp.containerType)) {
                            resp.containerType = "List";
                        } else if ("map".equals(resp.containerType)) {
                            resp.containerType = "Map";
                        }
                    }
                }

                if ( operation.returnBaseType == null ) {
                    operation.returnType = "void";
                    operation.returnBaseType = "Void";
                    // set vendorExtensions.x-java-is-response-void to true as returnBaseType is set to "Void"
                    operation.vendorExtensions.put("x-java-is-response-void", true);
                }

                if ("array".equals(operation.returnContainer)) {
                    operation.returnContainer = "List";
                } else if ("map".equals(operation.returnContainer)) {
                    operation.returnContainer = "Map";
                }
            }
        }
        return objs;
    }

    @Override
    public String toApiName(final String name) {
        String computed = name;
        if ( computed.length() == 0 ) {
            return "DefaultApi";
        }
        computed = sanitizeName(computed);
        return camelize(computed) + "Api";
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if ( templateName.endsWith("Impl.mustache") ) {
            int ix = result.lastIndexOf('/');
            result = result.substring(0, ix) + "/impl" + result.substring(ix, result.length() - 5) + "ServiceImpl.java";
            result = result.replace(apiFileFolder(), implFileFolder(implFolder));
        } else if ( templateName.endsWith("Factory.mustache") ) {
            int ix = result.lastIndexOf('/');
            result = result.substring(0, ix) + "/factories" + result.substring(ix, result.length() - 5) + "ServiceFactory.java";
            result = result.replace(apiFileFolder(), implFileFolder(implFolder));
        } else if ( templateName.endsWith("Service.mustache") ) {
            int ix = result.lastIndexOf('.');
            result = result.substring(0, ix) + "Service.java";
        }
        return result;
    }

    private String implFileFolder(String output) {
        return outputFolder + "/" + output + "/" + apiPackage().replace('.', '/');
    }

    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }


}
