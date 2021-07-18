/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.Iterators;
import com.google.inject.Injector;
import es.us.isa.idl.IDLStandaloneSetupGenerated;
import es.us.isa.idl.idl.*;
import es.us.isa.idl.idl.impl.*;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.xbase.lib.IteratorExtensions;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.*;

public abstract class AbstractJavaJAXRSServerCodegen extends AbstractJavaCodegen implements BeanValidationFeatures {
    public static final String SERVER_PORT = "serverPort";
    public static final String USE_TAGS = "useTags";

    /**
     * Name of the sub-directory in "src/main/resource" where to find the
     * Mustache template for the JAX-RS Codegen.
     */
    protected static final String JAXRS_TEMPLATE_DIRECTORY_NAME = "JavaJaxRS";
    protected String implFolder = "src/main/java";
    protected String testResourcesFolder = "src/test/resources";
    protected String title = "OpenAPI Server";
    protected String serverPort = "8080";

    protected boolean useBeanValidation = true;
    protected boolean useTags = false;

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractJavaJAXRSServerCodegen.class);

    public AbstractJavaJAXRSServerCodegen() {
        super();

        sourceFolder = "src/gen/java";
        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-jaxrs-server";
        dateLibrary = "legacy"; //TODO: add joda support to all jax-rs
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";

        // clioOptions default redifinition need to be updated
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        updateOption(this.DATE_LIBRARY, this.getDateLibrary());

        additionalProperties.put("title", title);
        // java inflector uses the jackson lib
        additionalProperties.put("jackson", "true");

        cliOptions.add(new CliOption(CodegenConstants.IMPL_FOLDER, CodegenConstants.IMPL_FOLDER_DESC).defaultValue(implFolder));
        cliOptions.add(new CliOption("title", "a title describing the application").defaultValue(title));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations",useBeanValidation));
        cliOptions.add(new CliOption(SERVER_PORT, "The port on which the server should be started").defaultValue(serverPort));
        cliOptions.add(CliOption.newBoolean(USE_TAGS, "use tags for creating interface and controller classnames"));
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
            setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }

        if (additionalProperties.containsKey(USE_TAGS)) {
            setUseTags(convertPropertyToBoolean(USE_TAGS));
        }

        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        final String basePath = StringUtils.substringBefore(StringUtils.removeStart(resourcePath, "/"), "/");
        if (!StringUtils.isEmpty(basePath)) {
            co.subresourceOperation = !co.path.isEmpty();
        }
        if (useTags) {
            super.addOperationToGroup(tag, resourcePath, operation, co, operations);
        } else {
            co.baseName = basePath;
            if (StringUtils.isEmpty(co.baseName) || StringUtils.containsAny(co.baseName, "{", "}")) {
                co.baseName = "default";
            }
            final List<CodegenOperation> opList = operations.computeIfAbsent(co.baseName, k -> new ArrayList<>());
            opList.add(co);
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        /* TODO there should be no need for the following logic
        if ("/".equals(swagger.getBasePath())) {
            swagger.setBasePath("");
        }
        */

        if (!this.additionalProperties.containsKey(SERVER_PORT)) {
            URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
            // 8080 is the default value for a JEE Server:
            this.additionalProperties.put(SERVER_PORT, URLPathUtils.getPort(url, serverPort));
        }

        if (openAPI.getPaths() != null) {
            for (String pathname : openAPI.getPaths().keySet()) {
                PathItem path = openAPI.getPaths().get(pathname);
                if (path.readOperations() != null) {
                    for (Operation operation : path.readOperations()) {
                        if (operation.getTags() != null) {
                            List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
                            for (String tag : operation.getTags()) {
                                Map<String, String> value = new HashMap<String, String>();
                                value.put("tag", tag);
                                tags.add(value);
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
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        return jaxrsPostProcessOperations(objs);
    }

    static Map<String, Object> jaxrsPostProcessOperations(Map<String, Object> objs) {
        @SuppressWarnings("unchecked")
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        String commonPath = null;
        if (operations != null) {
            @SuppressWarnings("unchecked")
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.hasConsumes == Boolean.TRUE) {
                    Map<String, String> firstType = operation.consumes.get(0);
                    if (firstType != null) {
                        if ("multipart/form-data".equals(firstType.get("mediaType"))) {
                            operation.isMultipart = Boolean.TRUE;
                        }
                    }
                }

                boolean isMultipartPost = false;
                List<Map<String, String>> consumes = operation.consumes;
                if (consumes != null) {
                    for (Map<String, String> consume : consumes) {
                        String mt = consume.get("mediaType");
                        if (mt != null) {
                            if (mt.startsWith("multipart/form-data")) {
                                isMultipartPost = true;
                            }
                        }
                    }
                }

                for (CodegenParameter parameter : operation.allParams) {
                    if (isMultipartPost) {
                        parameter.vendorExtensions.put("x-multipart", "true");
                    }
                }

                List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    for (CodegenResponse resp : responses) {
                        if ("0".equals(resp.code)) {
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
                        } else if ("set".equals(resp.containerType)) {
                            resp.containerType = "Set";
                        } else if ("map".equals(resp.containerType)) {
                            resp.containerType = "Map";
                        }
                    }
                }

                if (operation.returnBaseType == null) {
                    operation.returnType = "void";
                    operation.returnBaseType = "Void";
                    // set vendorExtensions.x-java-is-response-void to true as returnBaseType is set to "Void"
                    operation.vendorExtensions.put("x-java-is-response-void", true);
                }

                if ("array".equals(operation.returnContainer)) {
                    operation.returnContainer = "List";
                } else if ("set".equals(operation.returnContainer)) {
                    operation.returnContainer = "Set";
                } else if ("map".equals(operation.returnContainer)) {
                    operation.returnContainer = "Map";
                }

                if (commonPath == null) {
                    commonPath = operation.path;
                } else {
                    commonPath = getCommonPath(commonPath, operation.path);
                }
            }
            for (CodegenOperation co : ops) {
                co.path = StringUtils.removeStart(co.path, commonPath);
                co.subresourceOperation = co.path.length() > 1;
            }
            objs.put("commonPath", "/".equals(commonPath) ? StringUtils.EMPTY : commonPath);
        }
        return objs;
    }

    @Override
    public String toApiName(final String name) {
        String computed = name;
        if (computed.length() > 0) {
            computed = sanitizeName(computed);
        }
        return super.toApiName(computed);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if (templateName.endsWith("Impl.mustache")) {
            int ix = result.lastIndexOf(File.separator);
            result = result.substring(0, ix) + "/impl" + result.substring(ix, result.length() - 5) + "ServiceImpl.java";
            result = result.replace(apiFileFolder(), implFileFolder(implFolder));
        } else if (templateName.endsWith("Factory.mustache")) {
            int ix = result.lastIndexOf(File.separator);
            result = result.substring(0, ix) + "/factories" + result.substring(ix, result.length() - 5) + "ServiceFactory.java";
            result = result.replace(apiFileFolder(), implFileFolder(implFolder));
        } else if (templateName.endsWith("Service.mustache")) {
            int ix = result.lastIndexOf('.');
            result = result.substring(0, ix) + "Service.java";
        }
        return result;
    }

    private static String getCommonPath(String path1, String path2) {
        final String[] parts1 = StringUtils.split(path1, "/");
        final String[] parts2 = StringUtils.split(path2, "/");
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < Math.min(parts1.length, parts2.length); i++) {
            if (!parts1[i].equals(parts2[i])) {
                break;
            }
            builder.append("/").append(parts1[i]);
        }
        return builder.toString();
    }

    private String implFileFolder(String output) {
        return outputFolder + "/" + output + "/" + apiPackage().replace('.', '/');
    }

    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    @VisibleForTesting
    public void setUseTags(boolean useTags) {
        this.useTags = useTags;
    }

    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        if(operation.getExtensions() != null && operation.getExtensions().containsKey("x-dependencies")){
            List<String> dependencies =(List<String>) operation.getExtensions().get("x-dependencies");
//          List<CodegenDependency> dependencyList = new ArrayList<>();
            List<Map<String,String>> dependencyList = new ArrayList<>();
            try {
                Injector injector = new IDLStandaloneSetupGenerated().createInjectorAndDoEMFRegistration();
                XtextResourceSet resourceSet = injector.getInstance(XtextResourceSet.class);
                Resource resource = resourceSet.createResource(URI.createURI("dummy:/example.idl"));

                for (String dep: dependencies){
                    resource.load(new ByteArrayInputStream(dep.getBytes()), resourceSet.getLoadOptions());
                    String assertOperation = writeDependency((Dependency) resource.getContents().get(0).eContents().get(0),op);

                    Map<String,String> dependency = new HashMap<>();
                    dependency.put("idlDependency",dep);
                    dependency.put("assertOperation",assertOperation);
                    dependencyList.add(dependency);
                    resource.unload();
                }

                op.vendorExtensions.put("x-dependencies",dependencyList);

            }catch (IOException e){
                LOGGER.error("Error while processing IDL dependencies for operation: " + op.operationId + ". They will not be included");
                op.vendorExtensions.remove("x-dependencies");
            }catch (IllegalArgumentException e){
                LOGGER.error("Error while processing IDL dependencies for operation: " + op.operationId + ": " + e.getMessage());
                op.vendorExtensions.remove("x-dependencies");
            }
        }

        return op;
    }


    public String writeDependency(Dependency dep, CodegenOperation operation){
        String assertion = "";
        if(dep.getDep() instanceof ConditionalDependencyImpl) {
            assertion = writeConditionalDependency((ConditionalDependency) dep.getDep(), operation, assertion);
        }else if(dep.getDep() instanceof ArithmeticDependencyImpl){
            assertion = writeArithmeticDependency((ArithmeticDependency) dep.getDep(), true, operation, assertion);
        } else if (dep.getDep() instanceof RelationalDependencyImpl){
            assertion = writeRelationalDependency((RelationalDependency) dep.getDep(), true, operation, assertion);
        } else if (dep.getDep() instanceof GeneralPredefinedDependencyImpl) {
            assertion = writePredefinedDependency((GeneralPredefinedDependency) dep.getDep(), operation, assertion);
        }
        return assertion;
    }

    private String writeParamName(String paramName, CodegenOperation operation){
        return getParameter(paramName, operation).paramName;
    }

    private CodegenParameter getParameter(String paramName, CodegenOperation operation){
        return operation.queryParams.stream().filter(p->p.baseName.equals(paramName)).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("IDL parameter \"" + paramName + "\" not found in query params for operation \"" + operation.operationId +"\""));
    }

    private boolean isParamValueRelation(Param param){
        return param.getStringValues().size() != 0 || param.getPatternString() != null || param.getBooleanValue() != null || param.getDoubleValue() != null;
    }

    private String writeClause(GeneralClause clause, CodegenOperation operation, String assertOperation){
        if (clause.getPredicate() != null){
            if (clause.getNot() != null)
                assertOperation += "!";
            assertOperation += "(";
            assertOperation = writePredicate(clause.getPredicate(), operation, assertOperation);
            assertOperation += ")";
        }

        if (clause.getFirstElement() != null){
            if (clause.getFirstElement() instanceof RelationalDependencyImpl) {
                assertOperation = writeRelationalDependency((RelationalDependency) clause.getFirstElement(), false, operation, assertOperation);
            } else if (clause.getFirstElement() instanceof GeneralTermImpl){
                GeneralTerm term = (GeneralTerm) clause.getFirstElement();
                Param param = (Param) term.getParam();
                CodegenParameter parameter = getParameter(param.getName(), operation);

                if(term.getNot() != null)
                    assertOperation += "!";
                assertOperation += "(";

                assertOperation += parameter.paramName + " != null";
                if (parameter.isArray)
                    assertOperation += " && !" + parameter.paramName + ".isEmpty()";

                if(isParamValueRelation(param)){
                    assertOperation += " && ";
                    if (param.getBooleanValue() != null){
                        if (param.getBooleanValue().equals("false"))
                            assertOperation += "!";
                        assertOperation += parameter.paramName;
                    } else if (param.getDoubleValue() != null){
                        assertOperation += parameter.paramName + param.getRelationalOp() + Double.parseDouble(param.getDoubleValue());
                    } else if (param.getStringValues().size() != 0){
                        assertOperation += "(";
                        if (parameter.isArray){
                            for(String s: param.getStringValues()) {
                                assertOperation += parameter.paramName + ".contains(" + "\"" + s + "\") || ";
                            }
                        } else {
                            for(String s: param.getStringValues()) {
                                assertOperation += parameter.paramName + ".equals(" + "\"" + s + "\") || ";
                            }
                        }
                        assertOperation = assertOperation.substring(0,assertOperation.length()-4);
                        assertOperation += ")";
                    }else if (param.getPatternString() != null){
                        //TODO assert pattern
                    }
                }
                assertOperation += ")";

            } else if (clause.getFirstElement() instanceof ArithmeticDependencyImpl){
                assertOperation = writeArithmeticDependency((ArithmeticDependency) clause.getFirstElement(), false, operation, assertOperation);
            } else if (clause.getFirstElement() instanceof GeneralPredefinedDependencyImpl){
                assertOperation = writePredefinedDependency((GeneralPredefinedDependency) clause.getFirstElement(), operation, assertOperation);
            }

        }
        return assertOperation;
    }

    private String writePredicate(GeneralPredicate predicate, CodegenOperation operation, String assertOperation){
        assertOperation = writeClause(predicate.getFirstClause(), operation, assertOperation);

        if (predicate.getClauseContinuation() != null) {
            if (predicate.getClauseContinuation().getLogicalOp().equals("AND")){
                assertOperation += " && ";
            }else if (predicate.getClauseContinuation().getLogicalOp().equals("OR")){
                assertOperation += " || ";
            }
            assertOperation = writePredicate(predicate.getClauseContinuation().getAdditionalElements(), operation, assertOperation);
        }

        return assertOperation;
    }

    private String writeConditionalDependency(ConditionalDependency dep, CodegenOperation operation, String assertOperation) {
        assertOperation += "(!";
        assertOperation = writePredicate(dep.getCondition(), operation, assertOperation);
        assertOperation += " || ";
        assertOperation = writePredicate(dep.getConsequence(), operation, assertOperation);
        assertOperation += ")";

        return assertOperation;
    }

    private String writeRelationalDependency(RelationalDependency dep, boolean alone, CodegenOperation operation, String assertOperation){
        if(alone)
            assertOperation += "(!(" + writeParamName(dep.getParam1().getName(),operation) + " != null && " + writeParamName(dep.getParam2().getName(),operation) + " != null) || (" +
                    writeParamName(dep.getParam1().getName(),operation) + dep.getRelationalOp() + writeParamName(dep.getParam2().getName(),operation) + "))";
        else assertOperation += "(" + writeParamName(dep.getParam1().getName(),operation) + " != null && " + writeParamName(dep.getParam2().getName(),operation) + " != null && " +
                writeParamName(dep.getParam1().getName(),operation) + dep.getRelationalOp() + writeParamName(dep.getParam2().getName(),operation) + ")";
        return assertOperation;
    }

    private String writeArithmeticDependency(ArithmeticDependency dep, boolean alone, CodegenOperation operation, String assertOperation){
        assertOperation += "(";
        if (alone)
            assertOperation += "!(";
        Iterator params = IteratorExtensions.toIterable(Iterators.filter(dep.eAllContents(), Param.class)).iterator();

        while(params.hasNext()){
            Param param = (Param) params.next();
            assertOperation += writeParamName(param.getName(),operation) + " != null && ";
        }

        if (alone) {
            assertOperation = assertOperation.substring(0,assertOperation.length()-4);
            assertOperation += ") || (";
        }

        assertOperation = writeOperation(dep.getOperation(), operation, assertOperation);
        assertOperation += dep.getRelationalOp();
        assertOperation += Double.parseDouble(dep.getResult());
        assertOperation += ")";
        if (alone)
            assertOperation += ")";

        return assertOperation;
    }

    private String writeOperation(es.us.isa.idl.idl.Operation operation, CodegenOperation op, String assertOperation){
        if(operation.getOpeningParenthesis() == null){
            assertOperation += writeParamName(operation.getFirstParam().getName(),op);
            assertOperation = writeOperationContinuation(operation.getOperationContinuation(), op, assertOperation);
        } else {
            assertOperation += "(";
            assertOperation = writeOperation(operation.getOperation(), op, assertOperation);
            assertOperation += ")";
            if (operation.getOperationContinuation() != null)
                assertOperation = writeOperationContinuation(operation.getOperationContinuation(), op, assertOperation);
        }

        return assertOperation;
    }

    private String writeOperationContinuation(OperationContinuation opCont, CodegenOperation operation, String assertOperation){
        assertOperation += opCont.getArithOp();
        if(opCont.getAdditionalParams() instanceof ParamImpl){
            Param param = (Param) opCont.getAdditionalParams();
            assertOperation += writeParamName(param.getName(), operation);
        } else {
            assertOperation = writeOperation((es.us.isa.idl.idl.Operation) opCont.getAdditionalParams(), operation, assertOperation);
        }

        return assertOperation;
    }

    private String writePredefinedDependency(GeneralPredefinedDependency dep, CodegenOperation operation, String assertOperation){
        if (dep.getNot() != null)
            assertOperation += "(!";
        assertOperation += "(";

        for(GeneralPredicate depElement:dep.getPredefDepElements()){
            assertOperation += "(";
            switch (dep.getPredefDepType()){
                case "Or":
                    assertOperation = writePredicate(depElement, operation, assertOperation);
                    assertOperation += ") || ";
                    break;
                case "OnlyOne":
                case "ZeroOrOne":
                    assertOperation = writeZeroOrOneOnlyOneElement(depElement, dep.getPredefDepElements(), operation, assertOperation);
                    assertOperation += ") && ";
                    break;
                case "AllOrNone":
                    assertOperation = writeAllOrNoneElement(depElement, dep.getPredefDepElements(), operation, assertOperation);
                    assertOperation += ") && ";
                    break;
            }
        }

        if(dep.getPredefDepType().equals("OnlyOne")){
            assertOperation += "(";
            for (GeneralPredicate depElement:dep.getPredefDepElements()){
                assertOperation += "(";
                assertOperation = writePredicate(depElement, operation, assertOperation);
                assertOperation += ") || ";
            }
            assertOperation += ")";
        }
        assertOperation = assertOperation.substring(0,assertOperation.length()-4);
        assertOperation += ")";
        if (dep.getNot() != null)
            assertOperation += ")";

        return assertOperation;
    }

    private String writeZeroOrOneOnlyOneElement(GeneralPredicate element, EList<GeneralPredicate> allElements, CodegenOperation operation, String assertOperation) {
        return writeZeroOrOneAllOrNoneElement(element, allElements, false, true, operation, assertOperation);
    }

    private String writeAllOrNoneElement(GeneralPredicate element, EList<GeneralPredicate> allElements, CodegenOperation operation, String assertOperation) {
        assertOperation = writeZeroOrOneAllOrNoneElement(element, allElements, false, false, operation, assertOperation);
        assertOperation += ") && (";
        assertOperation = writeZeroOrOneAllOrNoneElement(element, allElements, true, true, operation, assertOperation);

        return assertOperation;
    }

    private String writeZeroOrOneAllOrNoneElement(GeneralPredicate element, EList<GeneralPredicate> allElements, boolean negateElement, boolean negateRemainingElements, CodegenOperation operation, String assertOperation) {
        if(negateElement){
            assertOperation += "(";
            assertOperation = writePredicate(element, operation, assertOperation);
            assertOperation += ") || (";
        } else {
            assertOperation += "(!(";
            assertOperation = writePredicate(element, operation, assertOperation);
            assertOperation += ")) || (";
        }

        for (GeneralPredicate remaining:allElements){
            if(!remaining.equals(element)){
                if (negateRemainingElements){
                    assertOperation += "(!(";
                    assertOperation = writePredicate(remaining, operation, assertOperation);
                    assertOperation += ")) && ";
                }else {
                    assertOperation += "(";
                    assertOperation = writePredicate(remaining, operation, assertOperation);
                    assertOperation += ") && ";
                }
            }
        }
        assertOperation = assertOperation.substring(0,assertOperation.length()-4);
        assertOperation += ")";

        return assertOperation;
    }

}


