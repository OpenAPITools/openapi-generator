package io.swagger.codegen;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.codegen.ignore.CodegenIgnoreProcessor;
import io.swagger.models.*;
import io.swagger.models.auth.OAuth2Definition;
import io.swagger.models.auth.SecuritySchemeDefinition;
import io.swagger.models.parameters.Parameter;
import io.swagger.util.Json;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

public class DefaultGenerator extends AbstractGenerator implements Generator {
    protected Logger LOGGER = LoggerFactory.getLogger(DefaultGenerator.class);

    protected CodegenConfig config;
    protected ClientOptInput opts;
    protected Swagger swagger;
    protected CodegenIgnoreProcessor ignoreProcessor;

    @Override
    public Generator opts(ClientOptInput opts) {
        this.opts = opts;

        this.swagger = opts.getSwagger();
        this.config = opts.getConfig();

        this.config.additionalProperties().putAll(opts.getOpts().getProperties());

        ignoreProcessor = new CodegenIgnoreProcessor(this.config.getOutputDir());

        return this;
    }

    @Override
    public List<File> generate() {
        Boolean generateApis = null;
        Boolean generateModels = null;
        Boolean generateSupportingFiles = null;
        Boolean generateApiTests = null;
        Boolean generateApiDocumentation = null;
        Boolean generateModelTests = null;
        Boolean generateModelDocumentation = null;

        Set<String> modelsToGenerate = null;
        Set<String> apisToGenerate = null;
        Set<String> supportingFilesToGenerate = null;

        // allows generating only models by specifying a CSV of models to generate, or empty for all
        if(System.getProperty("models") != null) {
            String modelNames = System.getProperty("models");
            generateModels = true;
            if(!modelNames.isEmpty()) {
                modelsToGenerate = new HashSet<String>(Arrays.asList(modelNames.split(",")));
            }
        }
        if(System.getProperty("apis") != null) {
            String apiNames = System.getProperty("apis");
            generateApis = true;
            if(!apiNames.isEmpty()) {
                apisToGenerate = new HashSet<String>(Arrays.asList(apiNames.split(",")));
            }
        }
        if(System.getProperty("supportingFiles") != null) {
            String supportingFiles = System.getProperty("supportingFiles");
            generateSupportingFiles = true;
            if(!supportingFiles.isEmpty()) {
                supportingFilesToGenerate = new HashSet<String>(Arrays.asList(supportingFiles.split(",")));
            }
        }
        if(System.getProperty("modelTests") != null) {
            generateModelTests = Boolean.valueOf(System.getProperty("modelTests"));
        }
        if(System.getProperty("modelDocs") != null) {
            generateModelDocumentation = Boolean.valueOf(System.getProperty("modelDocs"));
        }
        if(System.getProperty("apiTests") != null) {
            generateApiTests = Boolean.valueOf(System.getProperty("apiTests"));
        }
        if(System.getProperty("apiDocs") != null) {
            generateApiDocumentation = Boolean.valueOf(System.getProperty("apiDocs"));
        }

        if(generateApis == null && generateModels == null && generateSupportingFiles == null) {
            // no specifics are set, generate everything
            generateApis = true; generateModels = true; generateSupportingFiles = true;
        }
        else {
            if(generateApis == null) {
                generateApis = false;
            }
            if(generateModels == null) {
                generateModels = false;
            }
            if(generateSupportingFiles == null) {
                generateSupportingFiles = false;
            }
        }

        // model/api tests and documentation options rely on parent generate options (api or model) and no other options.
        // They default to true in all scenarios and can only be marked false explicitly
        if (generateModelTests == null) {
            generateModelTests = true;
        }
        if (generateModelDocumentation == null) {
            generateModelDocumentation = true;
        }
        if (generateApiTests == null) {
            generateApiTests = true;
        }
        if (generateApiDocumentation == null) {
            generateApiDocumentation = true;
        }

        // Additional properties added for tests to exclude references in project related files
        config.additionalProperties().put(CodegenConstants.GENERATE_API_TESTS, generateApiTests);
        config.additionalProperties().put(CodegenConstants.GENERATE_MODEL_TESTS, generateModelTests);
        if(Boolean.FALSE.equals(generateApiTests) && Boolean.FALSE.equals(generateModelTests)) {
            config.additionalProperties().put(CodegenConstants.EXCLUDE_TESTS, Boolean.TRUE);
        }

        if (swagger == null || config == null) {
            throw new RuntimeException("missing swagger input or config!");
        }
        if (System.getProperty("debugSwagger") != null) {
            Json.prettyPrint(swagger);
        }
        List<File> files = new ArrayList<File>();
        config.processOpts();
        config.preprocessSwagger(swagger);

        config.additionalProperties().put("generatedDate", DateTime.now().toString());
        config.additionalProperties().put("generatorClass", config.getClass().toString());
        config.additionalProperties().put("inputSpec", config.getInputSpec());

        if (swagger.getInfo() != null) {
            Info info = swagger.getInfo();
            if (info.getTitle() != null) {
                config.additionalProperties().put("appName", config.escapeText(info.getTitle()));
            }
            if (info.getVersion() != null) {
                config.additionalProperties().put("appVersion", config.escapeText(info.getVersion()));
            }

            if (StringUtils.isEmpty(info.getDescription())) {
                // set a default description if none if provided
                config.additionalProperties().put("appDescription",
                        "No description provided (generated by Swagger Codegen https://github.com/swagger-api/swagger-codegen)");
            } else {
                config.additionalProperties().put("appDescription",
                        config.escapeText(info.getDescription()));
            }

            if (info.getContact() != null) {
                Contact contact = info.getContact();
                config.additionalProperties().put("infoUrl", config.escapeText(contact.getUrl()));
                if (contact.getEmail() != null) {
                    config.additionalProperties().put("infoEmail", config.escapeText(contact.getEmail()));
                }
            }
            if (info.getLicense() != null) {
                License license = info.getLicense();
                if (license.getName() != null) {
                    config.additionalProperties().put("licenseInfo", config.escapeText(license.getName()));
                }
                if (license.getUrl() != null) {
                    config.additionalProperties().put("licenseUrl", config.escapeText(license.getUrl()));
                }
            }
            if (info.getVersion() != null) {
                config.additionalProperties().put("version", config.escapeText(info.getVersion()));
            }
            if (info.getTermsOfService() != null) {
                config.additionalProperties().put("termsOfService", config.escapeText(info.getTermsOfService()));
            }
        }

        if(swagger.getVendorExtensions() != null) {
        	config.vendorExtensions().putAll(swagger.getVendorExtensions());
        }

        StringBuilder hostBuilder = new StringBuilder();
        String scheme;
        if (swagger.getSchemes() != null && swagger.getSchemes().size() > 0) {
            scheme = config.escapeText(swagger.getSchemes().get(0).toValue());
        } else {
            scheme = "https";
        }
        scheme = config.escapeText(scheme);
        hostBuilder.append(scheme);
        hostBuilder.append("://");
        if (swagger.getHost() != null) {
            hostBuilder.append(swagger.getHost());
        } else {
            hostBuilder.append("localhost");
        }
        if (swagger.getBasePath() != null) {
            hostBuilder.append(swagger.getBasePath());
        }
        String contextPath = config.escapeText(swagger.getBasePath() == null ? "" : swagger.getBasePath());
        String basePath = config.escapeText(hostBuilder.toString());
        String basePathWithoutHost = config.escapeText(swagger.getBasePath());

        // resolve inline models
        InlineModelResolver inlineModelResolver = new InlineModelResolver();
        inlineModelResolver.flatten(swagger);

        List<Object> allOperations = new ArrayList<Object>();
        List<Object> allModels = new ArrayList<Object>();

        // models
        final Map<String, Model> definitions = swagger.getDefinitions();
        if (definitions != null) {
        	Set<String> modelKeys = definitions.keySet();

            if(generateModels) {
                if(modelsToGenerate != null && modelsToGenerate.size() > 0) {
                    Set<String> updatedKeys = new HashSet<String>();
                    for(String m : modelKeys) {
                        if(modelsToGenerate.contains(m)) {
                            updatedKeys.add(m);
                        }
                    }
                    modelKeys = updatedKeys;
                }

                // store all processed models
                Map<String,Object> allProcessedModels = new TreeMap<String, Object>(new Comparator<String>() {
                    @Override
                    public int compare(String o1, String o2) {
                        Model model1 = definitions.get(o1);
                        Model model2 = definitions.get(o2);

                        int model1InheritanceDepth = getInheritanceDepth(model1);
                        int model2InheritanceDepth = getInheritanceDepth(model2);

                        if (model1InheritanceDepth == model2InheritanceDepth) {
                            return ObjectUtils.compare(config.toModelName(o1), config.toModelName(o2));
                        } else if (model1InheritanceDepth > model2InheritanceDepth) {
                            return 1;
                        } else {
                            return -1;
                        }
                    }

                    private int getInheritanceDepth(Model model) {
                        int inheritanceDepth = 0;
                        Model parent = getParent(model);

                        while (parent != null) {
                            inheritanceDepth++;
                            parent = getParent(parent);
                        }

                        return inheritanceDepth;
                    }

                    private Model getParent(Model model) {
                        if (model instanceof ComposedModel) {
                            Model parent = ((ComposedModel) model).getParent();
                            if(parent != null) {
                                return definitions.get(parent.getReference());
                            }
                        }

                        return null;
                    }
                });

                // process models only
                for (String name : modelKeys) {
                    try {
                        //don't generate models that have an import mapping
                        if(config.importMapping().containsKey(name)) {
                            LOGGER.info("Model " + name + " not imported due to import mapping");
                            continue;
                        }

                        Model model = definitions.get(name);
                        Map<String, Model> modelMap = new HashMap<String, Model>();
                        modelMap.put(name, model);
                        Map<String, Object> models = processModels(config, modelMap, definitions);
                        models.put("classname", config.toModelName(name));
                        models.putAll(config.additionalProperties());

                        allProcessedModels.put(name, models);

                    } catch (Exception e) {
                        throw new RuntimeException("Could not process model '" + name + "'" + ".Please make sure that your schema is correct!", e);
                    }
                }

                // post process all processed models
                allProcessedModels = config.postProcessAllModels(allProcessedModels);

                // generate files based on processed models
                for (String name: allProcessedModels.keySet()) {
                	Map<String, Object> models = (Map<String, Object>)allProcessedModels.get(name);

                	try {
                        //don't generate models that have an import mapping
                        if(config.importMapping().containsKey(name)) {
                            continue;
                        }

                        allModels.add(((List<Object>) models.get("models")).get(0));

                        for (String templateName : config.modelTemplateFiles().keySet()) {
                            String suffix = config.modelTemplateFiles().get(templateName);
                            String filename = config.modelFileFolder() + File.separator + config.toModelFilename(name) + suffix;
                            if (!config.shouldOverwrite(filename)) {
                                LOGGER.info("Skipped overwriting " + filename);
                                continue;
                            }

                            File written = processTemplateToFile(models, templateName, filename);
                            if(written != null) {
                                files.add(written);
                            }
                        }

                        if(generateModelTests) {
                            // to generate model test files
                            for (String templateName : config.modelTestTemplateFiles().keySet()) {
                                String suffix = config.modelTestTemplateFiles().get(templateName);
                                String filename = config.modelTestFileFolder() + File.separator + config.toModelTestFilename(name) + suffix;
                                // do not overwrite test file that already exists
                                if (new File(filename).exists()) {
                                    LOGGER.info("File exists. Skipped overwriting " + filename);
                                    continue;
                                }

                                File written = processTemplateToFile(models, templateName, filename);
                                if (written != null) {
                                    files.add(written);
                                }
                            }
                        }

                        if(generateModelDocumentation) {
                            // to generate model documentation files
                            for (String templateName : config.modelDocTemplateFiles().keySet()) {
                                String suffix = config.modelDocTemplateFiles().get(templateName);
                                String filename = config.modelDocFileFolder() + File.separator + config.toModelDocFilename(name) + suffix;
                                if (!config.shouldOverwrite(filename)) {
                                    LOGGER.info("Skipped overwriting " + filename);
                                    continue;
                                }

                                File written = processTemplateToFile(models, templateName, filename);
                                if (written != null) {
                                    files.add(written);
                                }
                            }
                        }
                    } catch (Exception e) {
                        throw new RuntimeException("Could not generate model '" + name + "'", e);
                    }
                }
            }
        }
        if (System.getProperty("debugModels") != null) {
            LOGGER.info("############ Model info ############");
            Json.prettyPrint(allModels);
        }

        // apis
        Map<String, List<CodegenOperation>> paths = processPaths(swagger.getPaths());
        if(generateApis) {
            if(apisToGenerate != null && apisToGenerate.size() > 0) {
                Map<String, List<CodegenOperation>> updatedPaths = new TreeMap<String, List<CodegenOperation>>();
                for(String m : paths.keySet()) {
                    if(apisToGenerate.contains(m)) {
                        updatedPaths.put(m, paths.get(m));
                    }
                }
                paths = updatedPaths;
            }
            for (String tag : paths.keySet()) {
                try {
                    List<CodegenOperation> ops = paths.get(tag);
                    Collections.sort(ops, new Comparator<CodegenOperation>() {
                        @Override
                        public int compare(CodegenOperation one, CodegenOperation another) {
                            return ObjectUtils.compare(one.operationId, another.operationId);
                        }
                    });
                    Map<String, Object> operation = processOperations(config, tag, ops);

                    operation.put("basePath", basePath);
                    operation.put("basePathWithoutHost", basePathWithoutHost);
                    operation.put("contextPath", contextPath);
                    operation.put("baseName", tag);
                    operation.put("modelPackage", config.modelPackage());
                    operation.putAll(config.additionalProperties());
                    operation.put("classname", config.toApiName(tag));
                    operation.put("classVarName", config.toApiVarName(tag));
                    operation.put("importPath", config.toApiImport(tag));
                    operation.put("classFilename", config.toApiFilename(tag));

                    if(!config.vendorExtensions().isEmpty()) {
                    	operation.put("vendorExtensions", config.vendorExtensions());
                    }

                    // Pass sortParamsByRequiredFlag through to the Mustache template...
                    boolean sortParamsByRequiredFlag = true;
                    if (this.config.additionalProperties().containsKey(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG)) {
                        sortParamsByRequiredFlag = Boolean.valueOf(this.config.additionalProperties().get(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG).toString());
                    }
                    operation.put("sortParamsByRequiredFlag", sortParamsByRequiredFlag);

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
                        if (!config.shouldOverwrite(filename) && new File(filename).exists()) {
                            LOGGER.info("Skipped overwriting " + filename);
                            continue;
                        }

                        File written = processTemplateToFile(operation, templateName, filename);
                        if(written != null) {
                            files.add(written);
                        }
                    }

                    if(generateApiTests) {
                        // to generate api test files
                        for (String templateName : config.apiTestTemplateFiles().keySet()) {
                            String filename = config.apiTestFilename(templateName, tag);
                            // do not overwrite test file that already exists
                            if (new File(filename).exists()) {
                                LOGGER.info("File exists. Skipped overwriting " + filename);
                                continue;
                            }

                            File written = processTemplateToFile(operation, templateName, filename);
                            if (written != null) {
                                files.add(written);
                            }
                        }
                    }


                    if(generateApiDocumentation) {
                        // to generate api documentation files
                        for (String templateName : config.apiDocTemplateFiles().keySet()) {
                            String filename = config.apiDocFilename(templateName, tag);
                            if (!config.shouldOverwrite(filename) && new File(filename).exists()) {
                                LOGGER.info("Skipped overwriting " + filename);
                                continue;
                            }

                            File written = processTemplateToFile(operation, templateName, filename);
                            if (written != null) {
                                files.add(written);
                            }
                        }
                    }

                } catch (Exception e) {
                    throw new RuntimeException("Could not generate api file for '" + tag + "'", e);
                }
            }
        }

        if (System.getProperty("debugOperations") != null) {
            LOGGER.info("############ Operation info ############");
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
        bundle.put("swagger", this.swagger);
        bundle.put("basePath", basePath);
        bundle.put("basePathWithoutHost",basePathWithoutHost);
        bundle.put("scheme", scheme);
        bundle.put("contextPath", contextPath);
        bundle.put("apiInfo", apis);
        bundle.put("models", allModels);
        bundle.put("apiFolder", config.apiPackage().replace('.', File.separatorChar));
        bundle.put("modelPackage", config.modelPackage());
        List<CodegenSecurity> authMethods = config.fromSecurity(swagger.getSecurityDefinitions());
        if (authMethods != null && !authMethods.isEmpty()) {
            // sort auth methods to maintain the same order
            Collections.sort(authMethods, new Comparator<CodegenSecurity>() {
                @Override
                public int compare(CodegenSecurity one, CodegenSecurity another) {
                    return ObjectUtils.compare(one.name, another.name);
                }
            });
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
            LOGGER.info("############ Supporting file info ############");
            Json.prettyPrint(bundle);
        }

        if(generateSupportingFiles) {
            for (SupportingFile support : config.supportingFiles()) {
                try {
                    String outputFolder = config.outputFolder();
                    if (StringUtils.isNotEmpty(support.folder)) {
                        outputFolder += File.separator + support.folder;
                    }
                    File of = new File(outputFolder);
                    if (!of.isDirectory()) {
                        of.mkdirs();
                    }
                    String outputFilename = outputFolder + File.separator + support.destinationFilename;
                    if (!config.shouldOverwrite(outputFilename)) {
                        LOGGER.info("Skipped overwriting " + outputFilename);
                        continue;
                    }
                    String templateFile;
                    if( support instanceof GlobalSupportingFile) {
                        templateFile = config.getCommonTemplateDir() + File.separator +  support.templateFile;
                    } else {
                        templateFile = getFullTemplateFile(config, support.templateFile);
                    }
                    boolean shouldGenerate = true;
                    if(supportingFilesToGenerate != null && supportingFilesToGenerate.size() > 0) {
                        if(supportingFilesToGenerate.contains(support.destinationFilename)) {
                            shouldGenerate = true;
                        }
                        else {
                            shouldGenerate = false;
                        }
                    }
                    if(shouldGenerate) {
                        if(ignoreProcessor.allowsFile(new File(outputFilename))) {
                            if (templateFile.endsWith("mustache")) {
                                String template = readTemplate(templateFile);
                                Mustache.Compiler compiler = Mustache.compiler();
                                compiler = config.processCompiler(compiler);                                
                                Template tmpl = compiler
                                        .withLoader(new Mustache.TemplateLoader() {
                                            @Override
                                            public Reader getTemplate(String name) {
                                                return getTemplateReader(getFullTemplateFile(config, name + ".mustache"));
                                            }
                                        })
                                        .defaultValue("")
                                        .compile(template);

                                writeToFile(outputFilename, tmpl.execute(bundle));
                                files.add(new File(outputFilename));
                            } else {
                                InputStream in = null;

                                try {
                                    in = new FileInputStream(templateFile);
                                } catch (Exception e) {
                                    // continue
                                }
                                if (in == null) {
                                    in = this.getClass().getClassLoader().getResourceAsStream(getCPResourcePath(templateFile));
                                }
                                File outputFile = new File(outputFilename);
                                OutputStream out = new FileOutputStream(outputFile, false);
                                if (in != null) {
                                    LOGGER.info("writing file " + outputFile);
                                    IOUtils.copy(in, out);
                                } else {
                                    if (in == null) {
                                        LOGGER.error("can't open " + templateFile + " for input");
                                    }
                                }
                                files.add(outputFile);
                            }
                        } else {
                            LOGGER.info("Skipped generation of " + outputFilename + " due to rule in .swagger-codegen-ignore");
                        }
                    }
                } catch (Exception e) {
                    throw new RuntimeException("Could not generate supporting file '" + support + "'", e);
                }
            }

            // Consider .swagger-codegen-ignore a supporting file
            // Output .swagger-codegen-ignore if it doesn't exist and wasn't explicitly created by a generator
            final String swaggerCodegenIgnore = ".swagger-codegen-ignore";
            String ignoreFileNameTarget = config.outputFolder() + File.separator + swaggerCodegenIgnore;
            File ignoreFile = new File(ignoreFileNameTarget);
            if(!ignoreFile.exists()) {
                String ignoreFileNameSource = File.separator + config.getCommonTemplateDir() + File.separator +  swaggerCodegenIgnore;
                String ignoreFileContents = readResourceContents(ignoreFileNameSource);
                try {
                    writeToFile(ignoreFileNameTarget, ignoreFileContents);
                } catch (IOException e) {
                    throw new RuntimeException("Could not generate supporting file '" + swaggerCodegenIgnore + "'", e);
                }
                files.add(ignoreFile);
            }

            // Add default LICENSE (Apache-2.0) for all generators
            final String apache2License = "LICENSE";
            String licenseFileNameTarget = config.outputFolder() + File.separator + apache2License;
            File licenseFile = new File(licenseFileNameTarget);
            String licenseFileNameSource = File.separator + config.getCommonTemplateDir() + File.separator + apache2License;
            String licenseFileContents = readResourceContents(licenseFileNameSource);
            try {
                writeToFile(licenseFileNameTarget, licenseFileContents);
            } catch (IOException e) {
                throw new RuntimeException("Could not generate LICENSE file '" + apache2License + "'", e);
            }
            files.add(licenseFile);
        }
        config.processSwagger(swagger);
        return files;
    }

    private File processTemplateToFile(Map<String, Object> templateData, String templateName, String outputFilename) throws IOException {
        if(ignoreProcessor.allowsFile(new File(outputFilename.replaceAll("//", "/")))) {
            String templateFile = getFullTemplateFile(config, templateName);
            String template = readTemplate(templateFile);
            Mustache.Compiler compiler = Mustache.compiler();
            compiler = config.processCompiler(compiler);                                
            Template tmpl = compiler
                    .withLoader(new Mustache.TemplateLoader() {
                        @Override
                        public Reader getTemplate(String name) {
                            return getTemplateReader(getFullTemplateFile(config, name + ".mustache"));
                        }
                    })
                    .defaultValue("")
                    .compile(template);

            writeToFile(outputFilename, tmpl.execute(templateData));
            return new File(outputFilename);
        }

        LOGGER.info("Skipped generation of " + outputFilename + " due to rule in .swagger-codegen-ignore");
        return null;
    }

    private static void processMimeTypes(List<String> mimeTypeList, Map<String, Object> operation, String source) {
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
        Map<String, List<CodegenOperation>> ops = new TreeMap<String, List<CodegenOperation>>();

        for (String resourcePath : paths.keySet()) {
            Path path = paths.get(resourcePath);
            processOperation(resourcePath, "get", path.getGet(), ops, path);
            processOperation(resourcePath, "head", path.getHead(), ops, path);
            processOperation(resourcePath, "put", path.getPut(), ops, path);
            processOperation(resourcePath, "post", path.getPost(), ops, path);
            processOperation(resourcePath, "delete", path.getDelete(), ops, path);
            processOperation(resourcePath, "patch", path.getPatch(), ops, path);
            processOperation(resourcePath, "options", path.getOptions(), ops, path);
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

    public void processOperation(String resourcePath, String httpMethod, Operation operation, Map<String, List<CodegenOperation>> operations, Path path) {
        if (operation != null) {
            if (System.getProperty("debugOperations") != null) {
                LOGGER.info("processOperation: resourcePath= " + resourcePath + "\t;" + httpMethod + " " + operation
                        + "\n");
            }
            List<String> tags = operation.getTags();
            if (tags == null) {
                tags = new ArrayList<String>();
                tags.add("default");
            }

            /*
             build up a set of parameter "ids" defined at the operation level
             per the swagger 2.0 spec "A unique parameter is defined by a combination of a name and location"
              i'm assuming "location" == "in"
            */
            Set<String> operationParameters = new HashSet<String>();
            if (operation.getParameters() != null) {
                for (Parameter parameter : operation.getParameters()) {
                    operationParameters.add(generateParameterId(parameter));
                }
            }

            //need to propagate path level down to the operation
            if(path.getParameters() != null) {
                for (Parameter parameter : path.getParameters()) {
                    //skip propagation if a parameter with the same name is already defined at the operation level
                    if (!operationParameters.contains(generateParameterId(parameter))) {
                        operation.addParameter(parameter);
                    }
                }
            }

            for (String tag : tags) {
                CodegenOperation co = null;
                try {
                    co = config.fromOperation(resourcePath, httpMethod, operation, swagger.getDefinitions(), swagger);
                    co.tags = new ArrayList<String>();
                    co.tags.add(config.sanitizeTag(tag));
                    config.addOperationToGroup(config.sanitizeTag(tag), resourcePath, operation, co, operations);

                    List<Map<String, List<String>>> securities = operation.getSecurity();
                    if (securities == null && swagger.getSecurity() != null) {
                        securities = new ArrayList<Map<String, List<String>>>();
                        for (SecurityRequirement sr : swagger.getSecurity()) {
                            securities.add(sr.getRequirements());
                        }
                    }
                    if (securities == null || securities.isEmpty()) {
                        continue;
                    }
                    Map<String, SecuritySchemeDefinition> authMethods = new HashMap<String, SecuritySchemeDefinition>();
                    for (Map<String, List<String>> security: securities) {
                      for (String securityName : security.keySet()) {
                          SecuritySchemeDefinition securityDefinition = fromSecurity(securityName);
                          if (securityDefinition != null) {
                              if(securityDefinition instanceof OAuth2Definition) {
                                  OAuth2Definition oauth2Definition = (OAuth2Definition) securityDefinition;
                                  OAuth2Definition oauth2Operation = new OAuth2Definition();
                                  oauth2Operation.setType(oauth2Definition.getType());
                                  oauth2Operation.setAuthorizationUrl(oauth2Definition.getAuthorizationUrl());
                                  oauth2Operation.setFlow(oauth2Definition.getFlow());
                                  oauth2Operation.setTokenUrl(oauth2Definition.getTokenUrl());
                                  oauth2Operation.setScopes(new HashMap<String, String>());
                                  for (String scope : security.get(securityName)) {
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
                    }
                    if (!authMethods.isEmpty()) {
                        co.authMethods = config.fromSecurity(authMethods);
                        co.hasAuthMethods = true;
                    }
                }
                catch (Exception ex) {
                    String msg = "Could not process operation:\n" //
                        + "  Tag: " + tag + "\n"//
                        + "  Operation: " + operation.getOperationId() + "\n" //
                        + "  Resource: " + httpMethod + " " + resourcePath + "\n"//
                        + "  Definitions: " + swagger.getDefinitions() + "\n"  //
                        + "  Exception: " + ex.getMessage();
                    throw new RuntimeException(msg, ex);
                }
            }
        }
    }

    private static String generateParameterId(Parameter parameter) {
        return parameter.getName() + ":" + parameter.getIn();
    }

    @SuppressWarnings("static-method")
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

    @SuppressWarnings("static-method")
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
            mo.put("importPath", config.toModelImport(cm.classname));
            models.add(mo);

            allImports.addAll(cm.imports);
        }
        objs.put("models", models);

        Set<String> importSet = new TreeSet<String>();
        for (String nextImport : allImports) {
            String mapping = config.importMapping().get(nextImport);
            if (mapping == null) {
                mapping = config.toModelImport(nextImport);
            }
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                importSet.add(mapping);
            }
            // add instantiation types
            mapping = config.instantiationTypes().get(nextImport);
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                importSet.add(mapping);
            }
        }

        List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
        for(String s: importSet) {
            Map<String, String> item = new HashMap<String, String>();
            item.put("import", s);
            imports.add(item);
        }

        objs.put("imports", imports);
        config.postProcessModels(objs);

        return objs;
    }
}
