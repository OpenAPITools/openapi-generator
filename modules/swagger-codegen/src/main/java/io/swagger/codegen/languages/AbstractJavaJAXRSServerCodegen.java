package io.swagger.codegen.languages;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenResponse;
import io.swagger.codegen.CodegenType;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;

public abstract class AbstractJavaJAXRSServerCodegen extends JavaClientCodegen
{
    /**
     * Name of the sub-directory in "src/main/resource" where to find the
     * Mustache template for the JAX-RS Codegen.
     */
    protected static final String JAXRS_TEMPLATE_DIRECTORY_NAME = "JavaJaxRS";
    protected String implFolder = "src/main/java";
    protected String title = "Swagger Server";

    public AbstractJavaJAXRSServerCodegen()
    {
        super();
    }

    // ================
    // ABSTRACT METHODS
    // ================

    @Override
    public abstract String getHelp();

    @Override
    public abstract String getName();

    // ===============
    // COMMONS METHODS
    // ===============

    @Override
    public CodegenType getTag()
    {
        return CodegenType.SERVER;
    }

    @Override
    public void preprocessSwagger(Swagger swagger)
    {
        if ( "/".equals(swagger.getBasePath()) ) {
            swagger.setBasePath("");
        }

        String host = swagger.getHost();
        String port = "8080"; // Default value for a JEE Server
        if ( host != null ) {
            String[] parts = host.split(":");
            if ( parts.length > 1 ) {
                port = parts[1];
            }
        }
        this.additionalProperties.put("serverPort", port);
        if ( swagger.getPaths() != null ) {
            for ( String pathname : swagger.getPaths().keySet() ) {
                Path path = swagger.getPath(pathname);
                if ( path.getOperations() != null ) {
                    for ( Operation operation : path.getOperations() ) {
                        if ( operation.getTags() != null ) {
                            List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
                            for ( String tag : operation.getTags() ) {
                                Map<String, String> value = new HashMap<String, String>();
                                value.put("tag", tag);
                                value.put("hasMore", "true");
                                tags.add(value);
                            }
                            if ( tags.size() > 0 ) {
                                tags.get(tags.size() - 1).remove("hasMore");
                            }
                            if ( operation.getTags().size() > 0 ) {
                                String tag = operation.getTags().get(0);
                                operation.setTags(Arrays.asList(tag));
                            }
                            operation.setVendorExtension("x-tags", tags);
                        }
                    }
                }
            }
        }
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs)
    {
        @SuppressWarnings("unchecked")
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if ( operations != null ) {
            @SuppressWarnings("unchecked")
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for ( CodegenOperation operation : ops ) {
                List<CodegenResponse> responses = operation.responses;
                if ( responses != null ) {
                    for ( CodegenResponse resp : responses ) {
                        if ( "0".equals(resp.code) ) {
                            resp.code = "200";
                        }
                    }
                }
                if ( operation.returnType == null ) {
                    operation.returnType = "void";
                } else if ( operation.returnType.startsWith("List") ) {
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        operation.returnType = rt.substring("List<".length(), end).trim();
                        operation.returnContainer = "List";
                    }
                } else if ( operation.returnType.startsWith("Map") ) {
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        operation.returnType = rt.substring("Map<".length(), end).split(",")[1].trim();
                        operation.returnContainer = "Map";
                    }
                } else if ( operation.returnType.startsWith("Set") ) {
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        operation.returnType = rt.substring("Set<".length(), end).trim();
                        operation.returnContainer = "Set";
                    }
                }
            }
        }
        return objs;
    }

    @Override
    public String toApiName(final String name)
    {
        String computed = name;
        if ( computed.length() == 0 ) {
            return "DefaultApi";
        }
        computed = sanitizeName(computed);
        return camelize(computed) + "Api";
    }

    @Override
    public String apiFilename(String templateName, String tag)
    {
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

    private String implFileFolder(String output)
    {
        return outputFolder + "/" + output + "/" + apiPackage().replace('.', '/');
    }

    @Override
    public boolean shouldOverwrite(String filename)
    {
        return super.shouldOverwrite(filename) && !filename.endsWith("ServiceImpl.java") && !filename.endsWith("ServiceFactory.java");
    }

}
