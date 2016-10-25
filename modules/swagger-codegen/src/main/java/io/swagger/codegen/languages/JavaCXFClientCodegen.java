
package io.swagger.codegen.languages;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.languages.features.CXFFeatures;
import io.swagger.codegen.languages.features.LoggingFeatures;
import io.swagger.models.Operation;

public class JavaCXFClientCodegen extends AbstractJavaCodegen implements CXFFeatures 
{   
    private static final Logger LOGGER = LoggerFactory.getLogger(JavaCXFClientCodegen.class);
    
    /**
     * Name of the sub-directory in "src/main/resource" where to find the
     * Mustache template for the JAX-RS Codegen.
     */
    protected static final String JAXRS_TEMPLATE_DIRECTORY_NAME = "JavaJaxRS";
    
    protected boolean useBeanValidation = false;
    
    protected boolean useGzipFeature = false;
    
    protected boolean useLoggingFeature = false;
    
    protected boolean useBeanValidationFeature = false;
    
    
    public JavaCXFClientCodegen()
    {
        super();

        supportsInheritance = true;
        
        sourceFolder = "src/gen/java";
        invokerPackage = "io.swagger.api";
        artifactId = "swagger-jaxrs-client";
        dateLibrary = "legacy"; //TODO: add joda support to all jax-rs

        apiPackage = "io.swagger.api";
        modelPackage = "io.swagger.model";
        
        outputFolder = "generated-code/JavaJaxRS-CXF";
        
        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");


        typeMapping.put("date", "LocalDate");
        typeMapping.put("DateTime", "javax.xml.datatype.XMLGregorianCalendar"); // Map DateTime fields to Java standart class 'XMLGregorianCalendar'

        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "cxf";

        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));
        
        cliOptions.add(CliOption.newBoolean(USE_GZIP_FEATURE, "Use Gzip Feature"));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION_FEATURE, "Use BeanValidation Feature"));
        cliOptions.add(CliOption.newBoolean(USE_LOGGING_FEATURE, "Use Logging Feature"));
        
        
    }


    @Override
    public void processOpts()
    {
        super.processOpts();
        
        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            boolean useBeanValidationProp = convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION);
            this.setUseBeanValidation(useBeanValidationProp);
        }
        
        this.setUseGzipFeature(convertPropertyToBooleanAndWriteBack(USE_GZIP_FEATURE));
        this.setUseLoggingFeature(convertPropertyToBooleanAndWriteBack(USE_LOGGING_FEATURE));
            
        boolean useBeanValidationFeature = convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION_FEATURE);
            this.setUseBeanValidationFeature(useBeanValidationFeature);
            if (useBeanValidationFeature) {
                LOGGER.info("make sure your client supports Bean Validation 1.1");
            }
       
        supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen
        
        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        
    }

    @Override
    public String getName()
    {
        return "jaxrs-cxf-client";
    }


    @Override
    public CodegenType getTag()
    {
        return CodegenType.CLIENT;
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
        return "Generates a Java JAXRS Client based on Apache CXF framework.";
    }
    
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }
    

    public void setUseGzipFeature(boolean useGzipFeature) {
        this.useGzipFeature = useGzipFeature;
    }


    public void setUseLoggingFeature(boolean useLoggingFeature) {
        this.useLoggingFeature = useLoggingFeature;
    }


    public void setUseBeanValidationFeature(boolean useBeanValidationFeature) {
        this.useBeanValidationFeature = useBeanValidationFeature;
    }
    
}
