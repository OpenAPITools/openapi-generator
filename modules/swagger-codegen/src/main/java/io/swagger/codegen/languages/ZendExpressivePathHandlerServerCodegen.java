package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Operation;
import org.apache.commons.lang3.StringUtils;


import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ZendExpressivePathHandlerServerCodegen extends AbstractPhpCodegen {
    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "ze-ph";
    }

    @Override
    public String getHelp() {
        return "Generates PHP server stub using Zend Expressive ( https://zendframework.github.io/zend-expressive ) and Path Handler ( https://github.com/Articus/PathHandler ).";
    }

    public ZendExpressivePathHandlerServerCodegen() {
        super();

        embeddedTemplateDir = templateDir = "ze-ph";
        invokerPackage = "App";
        packagePath = "";
        srcBasePath = "src" + File.separator + "App";
        apiDirName = "Handler";
        modelDirName = "DTO";
        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;

        apiTestTemplateFiles.clear();
        modelTestTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        modelDocTemplateFiles.clear();

        supportingFiles.add(new SupportingFile("README.md.mustache", packagePath, "README.md"));
        supportingFiles.add(new SupportingFile("composer.json.mustache", packagePath, "composer.json"));
        supportingFiles.add(new SupportingFile("index.php", packagePath + File.separator + "public", "index.php"));
        supportingFiles.add(new SupportingFile("container.php", packagePath + File.separator + "application", "container.php"));
        supportingFiles.add(new SupportingFile("config.yml", packagePath + File.separator + "application", "config.yml"));
        supportingFiles.add(new SupportingFile("app.yml.mustache", packagePath + File.separator + "application" + File.separator + "config", "app.yml"));
        supportingFiles.add(new SupportingFile("path_handler.yml.mustache", packagePath + File.separator + "application" + File.separator + "config", "path_handler.yml"));
        supportingFiles.add(new SupportingFile("data_transfer.yml.mustache", packagePath + File.separator + "application" + File.separator + "config", "data_transfer.yml"));
        supportingFiles.add(new SupportingFile("Date.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "Date.php"));
        supportingFiles.add(new SupportingFile("DateTime.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Strategy", "DateTime.php"));
        supportingFiles.add(new SupportingFile("Type.php.mustache", packagePath + File.separator + srcBasePath + File.separator + "Validator", "Type.php"));

        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, "1.0.0");
    }

    /**
     * Add operation to group
     * Override of default grouping - group by resource path, not tag
     *
     * @param tag          name of the tag
     * @param resourcePath path of the resource
     * @param operation    Swagger Operation object
     * @param co           Codegen Operation object
     * @param operations   map of Codegen operations
     */
    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(resourcePath);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(resourcePath, opList);
        }
        //ignore duplicate operation ids - that means that operation has several tags
        int counter = 0;
        for (CodegenOperation op : opList) {
            if (co.operationId.equals(op.operationId)) {
                counter++;
            }
        }
        if (counter == 0) {
            co.operationIdLowerCase = co.operationId.toLowerCase();
            opList.add(co);
            co.baseName = tag;
        }
    }

    /**
     * Return the file name of the Api Test
     *
     * @param name the file name of the Api
     * @return the file name of the Api
     */
    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    /**
     * Output the API (class) name (capitalized) ending with "Api"
     * Return DefaultApi if name is empty
     *
     * @param name the name of the Api
     * @return capitalized Api name ending with "Api"
     */
    @Override
    public String toApiName(String name) {
        //Remove }
        name = name.replaceAll("[\\}]", "");
        return super.toModelName(name);
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        objs = super.postProcessOperations(objs);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        String interfaceToImplement;
        StringBuilder interfacesToImplement = new StringBuilder();
        String classMethod;
        for (CodegenOperation op : operationList) {
            switch (op.httpMethod) {
                case "GET":
                    interfaceToImplement = "Operation\\GetInterface";
                    classMethod = "handleGet";
                    break;
                case "POST":
                    interfaceToImplement = "Operation\\PostInterface";
                    classMethod = "handlePost";
                    break;
                case "PATCH":
                    interfaceToImplement = "Operation\\PatchInterface";
                    classMethod = "handlePatch";
                    break;
                case "PUT":
                    interfaceToImplement = "Operation\\PutInterface";
                    classMethod = "handlePut";
                    break;
                case "DELETE":
                    interfaceToImplement = "Operation\\DeleteInterface";
                    classMethod = "handleDelete";
                    break;
                default:
                    throw new RuntimeException("Unknown HTTP Method " + op.httpMethod + " not allowed");
            }
            if (interfacesToImplement.length() > 0) {
                interfacesToImplement.append(", ");
            }
            interfacesToImplement.append(interfaceToImplement);
            op.httpMethod = classMethod;
        }
        operations.put("interfacesToImplement", interfacesToImplement.toString());

        return objs;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs = super.postProcessSupportingFileData(objs);

        Map<String, Object> apiInfo = (Map<String, Object>) objs.get("apiInfo");
        List<Map<String, Object>> apis = (List<Map<String, Object>>) apiInfo.get("apis");

        List<Map<String, Object>> routes = new ArrayList<Map<String, Object>>();
        for (Map<String, Object> api : apis) {
            String handler = (String) api.get("classname");
            String url = (String) api.get("baseName");
            if (url.charAt(0) == '/') {
                url = url.substring(1);
            }
            insertRoute(routes, url.split("/"), 0, handler);
        }

        objs.put("routes", routes);
        return objs;
    }

    private void insertRoute(List<Map<String, Object>> routes, String[] urlParts, int currentUrlPartIndex, String handler) {
        if (urlParts.length > currentUrlPartIndex) {
            String urlPart = urlParts[currentUrlPartIndex];
            //List<Map<String, Object>> subRoutes = null;
            Map<String, Object> currentRoute = null;
            for (Map<String, Object> route : routes) {
                if (urlPart.equals(route.get("name"))) {
                    currentRoute = route;
                    break;
                }
            }
            if (currentRoute == null) {
                currentRoute = new HashMap<String, Object>();

                String routePart = urlPart.replaceAll("^\\{(\\w+)\\}$", ":$1");
                boolean isLastUrlPart = currentUrlPartIndex == urlParts.length - 1;

                currentRoute.put("name", urlPart);
                currentRoute.put("route", "/" + routePart);
                currentRoute.put("type", (urlPart == routePart) ? "Literal" : "Segment");
                currentRoute.put("handler", isLastUrlPart ? handler : null);
                currentRoute.put("hasChildren", false);
                currentRoute.put("children", new ArrayList<Map<String, Object>>());
                currentRoute.put("padding", StringUtils.repeat(' ', 4 * currentUrlPartIndex));

                routes.add(currentRoute);
            }
            List<Map<String, Object>> subRoutes = (List<Map<String, Object>>) currentRoute.get("children");
            insertRoute(subRoutes, urlParts, currentUrlPartIndex + 1, handler);
            currentRoute.put("hasChildren", !subRoutes.isEmpty());
        }
    }
}
