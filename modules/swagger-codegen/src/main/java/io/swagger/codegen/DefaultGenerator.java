package io.swagger.codegen;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.models.Contact;
import io.swagger.models.Info;
import io.swagger.models.License;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;
import io.swagger.models.auth.OAuth2Definition;
import io.swagger.models.auth.SecuritySchemeDefinition;
import io.swagger.util.Json;
import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.apache.commons.lang3.StringUtils.capitalize;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

public class DefaultGenerator extends AbstractGenerator implements Generator {
    protected CodegenConfig config;
    protected ClientOptInput opts = null;
    protected Swagger swagger = null;

    public Generator opts(ClientOptInput opts) {
        this.opts = opts;

        this.swagger = opts.getSwagger();
        this.config = opts.getConfig();
        this.config.additionalProperties().putAll(opts.getOpts().getProperties());

        return this;
    }

    public List<File> generate() {
        if (swagger == null || config == null) {
            throw new RuntimeException("missing swagger input or config!");
        }
        if (System.getProperty("debugSwagger") != null) {
            Json.prettyPrint(swagger);
        }
        List<File> files = new ArrayList<File>();
        try {
            config.processOpts();
            if (swagger.getInfo() != null) {
                Info info = swagger.getInfo();
                if (info.getTitle() != null) {
                    config.additionalProperties().put("appName", info.getTitle());
                }
                if (info.getVersion() != null) {
                    config.additionalProperties().put("appVersion", info.getVersion());
                }
                if (info.getDescription() != null) {
                    config.additionalProperties().put("appDescription",
                            config.escapeText(info.getDescription()));
                }
                if (info.getContact() != null) {
                    Contact contact = info.getContact();
                    config.additionalProperties().put("infoUrl", contact.getUrl());
                    if (contact.getEmail() != null) {
                        config.additionalProperties().put("infoEmail", contact.getEmail());
                    }
                }
                if (info.getLicense() != null) {
                    License license = info.getLicense();
                    if (license.getName() != null) {
                        config.additionalProperties().put("licenseInfo", license.getName());
                    }
                    if (license.getUrl() != null) {
                        config.additionalProperties().put("licenseUrl", license.getUrl());
                    }
                }
                if (info.getVersion() != null) {
                    config.additionalProperties().put("version", info.getVersion());
                }
            }

            StringBuilder hostBuilder = new StringBuilder();
            String scheme;
            if (swagger.getSchemes() != null && swagger.getSchemes().size() > 0) {
                scheme = swagger.getSchemes().get(0).toValue();
            } else {
                scheme = "https";
            }
            hostBuilder.append(scheme);
            hostBuilder.append("://");
            if (swagger.getHost() != null) {
                hostBuilder.append(swagger.getHost());
            } else {
                hostBuilder.append("localhost");
            }
            if (swagger.getBasePath() != null) {
                hostBuilder.append(swagger.getBasePath());
            } else {
                hostBuilder.append("/");
            }
            String contextPath = swagger.getBasePath() == null ? "/" : swagger.getBasePath();
            String basePath = hostBuilder.toString();


            List<Object> allOperations = new ArrayList<Object>();
            List<Object> allModels = new ArrayList<Object>();

            // models
            Map<String, Model> definitions = swagger.getDefinitions();
            if (definitions != null) {
                for (String name : definitions.keySet()) {
                    Model model = definitions.get(name);
                    Map<String, Model> modelMap = new HashMap<String, Model>();
                    modelMap.put(name, model);
                    Map<String, Object> models = processModels(config, modelMap, definitions);
                    models.putAll(config.additionalProperties());

                    allModels.add(((List<Object>) models.get("models")).get(0));

                    for (String templateName : config.modelTemplateFiles().keySet()) {
                        String suffix = config.modelTemplateFiles().get(templateName);
                        String filename = config.modelFileFolder() + File.separator + config.toModelFilename(name) + suffix;
                        if (!config.shouldOverwrite(filename)) {
                            continue;
                        }
                        String template = readTemplate(config.templateDir() + File.separator + templateName);
                        Template tmpl = Mustache.compiler()
                                .withLoader(new Mustache.TemplateLoader() {
                                    public Reader getTemplate(String name) {
                                        return getTemplateReader(config.templateDir() + File.separator + name + ".mustache");
                                    }
                                })
                                .defaultValue("")
                                .compile(template);
                        writeToFile(filename, tmpl.execute(models));
                        files.add(new File(filename));
                    }
                }
            }
            if (System.getProperty("debugModels") != null) {
                System.out.println("############ Model info ############");
                Json.prettyPrint(allModels);
            }

            // apis
            Map<String, List<CodegenOperation>> paths = processPaths(swagger.getPaths());
            for (String tag : paths.keySet()) {
                List<CodegenOperation> ops = paths.get(tag);
                Map<String, Object> operation = processOperations(config, tag, ops);

                operation.put("basePath", basePath);
                operation.put("contextPath", contextPath);
                operation.put("baseName", tag);
                operation.put("modelPackage", config.modelPackage());
                operation.putAll(config.additionalProperties());
                operation.put("classname", config.toApiName(tag));
                operation.put("classVarName", config.toApiVarName(tag));
                operation.put("importPath", config.toApiImport(tag));

                processMimeTypes(swagger.getConsumes(), operation, "consumes");
                processMimeTypes(swagger.getProduces(), operation, "produces");

                allOperations.add(new HashMap<String, Object>(operation));
                for (int i = 0; i < allOperations.size(); i++) {
                    Map<String, Object> oo = (Map<String, Object>) allOperations.get(i);
                    if (i < (allOperations.size() - 1)) {
                        oo.put("hasMore", "true");
                    }
                }

                for (String templateName : config.apiTemplateFiles().keySet()) {

                    String filename = config.apiFilename(templateName, tag);
                    if (!config.shouldOverwrite(filename)) {
                        continue;
                    }

                    String template = readTemplate(config.templateDir() + File.separator + templateName);
                    Template tmpl = Mustache.compiler()
                            .withLoader(new Mustache.TemplateLoader() {
                                public Reader getTemplate(String name) {
                                    return getTemplateReader(config.templateDir() + File.separator + name + ".mustache");
                                }
                            })
                            .defaultValue("")
                            .compile(template);

                    writeToFile(filename, tmpl.execute(operation));
                    files.add(new File(filename));
                }
            }
            if (System.getProperty("debugOperations") != null) {
                System.out.println("############ Operation info ############");
                Json.prettyPrint(allOperations);
            }

            // supporting files
            Map<String, Object> bundle = new HashMap<String, Object>();
            bundle.putAll(config.additionalProperties());
            bundle.put("apiPackage", config.apiPackage());

            Map<String, Object> apis = new HashMap<String, Object>();
            apis.put("apis", allOperations);
            if (swagger.getHost() != null) {
                bundle.put("host", swagger.getHost());
            }
            bundle.put("basePath", basePath);
            bundle.put("scheme", scheme);
            bundle.put("contextPath", contextPath);
            bundle.put("apiInfo", apis);
            bundle.put("models", allModels);
            bundle.put("apiFolder", config.apiPackage().replace('.', File.separatorChar));
            bundle.put("modelPackage", config.modelPackage());
            List<CodegenSecurity> authMethods = config.fromSecurity(swagger.getSecurityDefinitions());
            if (authMethods != null && !authMethods.isEmpty()) {
                bundle.put("authMethods", authMethods);
                bundle.put("hasAuthMethods", true);
            }
            if (swagger.getExternalDocs() != null) {
                bundle.put("externalDocs", swagger.getExternalDocs());
            }
            for (int i = 0; i < allModels.size() - 1; i++) {
                HashMap<String, CodegenModel> cm = (HashMap<String, CodegenModel>) allModels.get(i);
                CodegenModel m = cm.get("model");
                m.hasMoreModels = true;
            }

            config.postProcessSupportingFileData(bundle);

            if (System.getProperty("debugSupportingFiles") != null) {
                System.out.println("############ Supporting file info ############");
                Json.prettyPrint(bundle);
            }

            for (SupportingFile support : config.supportingFiles()) {
                String outputFolder = config.outputFolder();
                if (isNotEmpty(support.folder)) {
                    outputFolder += File.separator + support.folder;
                }
                File of = new File(outputFolder);
                if (!of.isDirectory()) {
                    of.mkdirs();
                }
                String outputFilename = outputFolder + File.separator + support.destinationFilename;
                if (!config.shouldOverwrite(outputFilename)) {
                    continue;
                }

                if (support.templateFile.endsWith("mustache")) {
                    String template = readTemplate(config.templateDir() + File.separator + support.templateFile);
                    Template tmpl = Mustache.compiler()
                            .withLoader(new Mustache.TemplateLoader() {
                                public Reader getTemplate(String name) {
                                    return getTemplateReader(config.templateDir() + File.separator + name + ".mustache");
                                }
                            })
                            .defaultValue("")
                            .compile(template);

                    writeToFile(outputFilename, tmpl.execute(bundle));
                    files.add(new File(outputFilename));
                } else {
                    InputStream in = null;

                    try {
                        in = new FileInputStream(config.templateDir() + File.separator + support.templateFile);
                    } catch (Exception e) {
                        // continue
                    }
                    if (in == null) {
                        in = this.getClass().getClassLoader().getResourceAsStream(getCPResourcePath(config.templateDir() + File.separator + support.templateFile));
                    }
                    File outputFile = new File(outputFilename);
                    OutputStream out = new FileOutputStream(outputFile, false);
                    if (in != null && out != null) {
                        System.out.println("writing file " + outputFile);
                        IOUtils.copy(in, out);
                    } else {
                        if (in == null) {
                            System.out.println("can't open " + config.templateDir() + File.separator + support.templateFile + " for input");
                        }
                        if (out == null) {
                            System.out.println("can't open " + outputFile + " for output");
                        }
                    }

                    files.add(outputFile);
                }
            }

            config.processSwagger(swagger);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return files;
    }

    private void processMimeTypes(List<String> mimeTypeList, Map<String, Object> operation, String source) {
        if (mimeTypeList != null && mimeTypeList.size() > 0) {
            List<Map<String, String>> c = new ArrayList<Map<String, String>>();
            int count = 0;
            for (String key : mimeTypeList) {
                Map<String, String> mediaType = new HashMap<String, String>();
                mediaType.put("mediaType", key);
                count += 1;
                if (count < mimeTypeList.size()) {
                    mediaType.put("hasMore", "true");
                } else {
                    mediaType.put("hasMore", null);
                }
                c.add(mediaType);
            }
            operation.put(source, c);
            String flagFieldName = "has" + source.substring(0, 1).toUpperCase() + source.substring(1);
            operation.put(flagFieldName, true);
        }
    }

    public Map<String, List<CodegenOperation>> processPaths(Map<String, Path> paths) {
        Map<String, List<CodegenOperation>> ops = new HashMap<String, List<CodegenOperation>>();

        for (String resourcePath : paths.keySet()) {
            Path path = paths.get(resourcePath);
            processOperation(resourcePath, "get", path.getGet(), ops);
            processOperation(resourcePath, "put", path.getPut(), ops);
            processOperation(resourcePath, "post", path.getPost(), ops);
            processOperation(resourcePath, "delete", path.getDelete(), ops);
            processOperation(resourcePath, "patch", path.getPatch(), ops);
            processOperation(resourcePath, "options", path.getOptions(), ops);
        }
        return ops;
    }

    public SecuritySchemeDefinition fromSecurity(String name) {
        Map<String, SecuritySchemeDefinition> map = swagger.getSecurityDefinitions();
        if (map == null) {
            return null;
        }
        return map.get(name);
    }


    public void processOperation(String resourcePath, String httpMethod, Operation operation, Map<String, List<CodegenOperation>> operations) {
        if (operation != null) {
            List<String> tags = operation.getTags();
            if (tags == null) {
                tags = new ArrayList<String>();
                tags.add("default");
            }

            for (String tag : tags) {
                CodegenOperation co = config.fromOperation(resourcePath, httpMethod, operation, swagger.getDefinitions());
                co.tags = new ArrayList<String>();
                co.tags.add(sanitizeTag(tag));
                config.addOperationToGroup(sanitizeTag(tag), resourcePath, operation, co, operations);

                List<Map<String, List<String>>> securities = operation.getSecurity();
                if (securities == null) {
                    continue;
                }
                Map<String, SecuritySchemeDefinition> authMethods = new HashMap<String, SecuritySchemeDefinition>();
                for (Map<String, List<String>> security : securities) {
                    if (security.size() != 1) {
                        //Not sure what to do
                        continue;
                    }
                    String securityName = security.keySet().iterator().next();
                    SecuritySchemeDefinition securityDefinition = fromSecurity(securityName);
                    if (securityDefinition != null) {
                    	if(securityDefinition instanceof OAuth2Definition) {
                    		OAuth2Definition oauth2Definition = (OAuth2Definition) securityDefinition;
                    		OAuth2Definition oauth2Operation = new OAuth2Definition();
                    		oauth2Operation.setType(oauth2Definition.getType());
                    		oauth2Operation.setAuthorizationUrl(oauth2Definition.getAuthorizationUrl());
                    		oauth2Operation.setFlow(oauth2Definition.getFlow());
                    		oauth2Operation.setTokenUrl(oauth2Definition.getTokenUrl());
                    		for (String scope : security.values().iterator().next()) {
                    			if (oauth2Definition.getScopes().containsKey(scope)) {
                    				oauth2Operation.addScope(scope, oauth2Definition.getScopes().get(scope));
                    			}
                    		}
                    		authMethods.put(securityName, oauth2Operation);
                    	} else {
                    		authMethods.put(securityName, securityDefinition);	
                    	}
                    }
                }
                if (!authMethods.isEmpty()) {
                    co.authMethods = config.fromSecurity(authMethods);
                }
            }
        }
    }

    protected String sanitizeTag(String tag) {
        // remove spaces and make strong case
        String[] parts = tag.split(" ");
        StringBuilder buf = new StringBuilder();
        for (String part : parts) {
            if (isNotEmpty(part)) {
                buf.append(capitalize(part));
            }
        }
        return buf.toString().replaceAll("[^a-zA-Z ]", "");
    }

    public Map<String, Object> processOperations(CodegenConfig config, String tag, List<CodegenOperation> ops) {
        Map<String, Object> operations = new HashMap<String, Object>();
        Map<String, Object> objs = new HashMap<String, Object>();
        objs.put("classname", config.toApiName(tag));
        objs.put("pathPrefix", config.toApiVarName(tag));

        // check for operationId uniqueness
        Set<String> opIds = new HashSet<String>();
        int counter = 0;
        for (CodegenOperation op : ops) {
            String opId = op.nickname;
            if (opIds.contains(opId)) {
                counter++;
                op.nickname += "_" + counter;
            }
            opIds.add(opId);
        }
        objs.put("operation", ops);

        operations.put("operations", objs);
        operations.put("package", config.apiPackage());


        Set<String> allImports = new LinkedHashSet<String>();
        for (CodegenOperation op : ops) {
            allImports.addAll(op.imports);
        }

        List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
        for (String nextImport : allImports) {
            Map<String, String> im = new LinkedHashMap<String, String>();
            String mapping = config.importMapping().get(nextImport);
            if (mapping == null) {
                mapping = config.toModelImport(nextImport);
            }
            if (mapping != null) {
                im.put("import", mapping);
                imports.add(im);
            }
        }

        operations.put("imports", imports);

        // add a flag to indicate whether there's any {{import}}
        if (imports.size() > 0) {
            operations.put("hasImport", true);
        }

        config.postProcessOperations(operations);
        if (objs.size() > 0) {
            List<CodegenOperation> os = (List<CodegenOperation>) objs.get("operation");

            if (os != null && os.size() > 0) {
                CodegenOperation op = os.get(os.size() - 1);
                op.hasMore = null;
            }
        }
        return operations;
    }

    public Map<String, Object> processModels(CodegenConfig config, Map<String, Model> definitions, Map<String, Model> allDefinitions) {
        Map<String, Object> objs = new HashMap<String, Object>();
        objs.put("package", config.modelPackage());
        List<Object> models = new ArrayList<Object>();
        Set<String> allImports = new LinkedHashSet<String>();
        for (String key : definitions.keySet()) {
            Model mm = definitions.get(key);
            CodegenModel cm = config.fromModel(key, mm, allDefinitions);
            Map<String, Object> mo = new HashMap<String, Object>();
            mo.put("model", cm);
            mo.put("importPath", config.toModelImport(key));
            models.add(mo);
            allImports.addAll(cm.imports);
        }
        objs.put("models", models);

        List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
        for (String nextImport : allImports) {
            Map<String, String> im = new LinkedHashMap<String, String>();
            String mapping = config.importMapping().get(nextImport);
            if (mapping == null) {
                mapping = config.toModelImport(nextImport);
            }
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                im.put("import", mapping);
                imports.add(im);
            }
            // add instantiation types
            mapping = config.instantiationTypes().get(nextImport);
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                im.put("import", mapping);
                imports.add(im);
            }
        }

        objs.put("imports", imports);
        config.postProcessModels(objs);

        return objs;
    }
}
