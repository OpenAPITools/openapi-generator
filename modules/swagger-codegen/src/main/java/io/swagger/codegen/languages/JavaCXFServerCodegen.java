
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
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.languages.features.CXFServerFeatures;
import io.swagger.models.Operation;

public class JavaCXFServerCodegen extends AbstractJavaJAXRSServerCodegen implements CXFServerFeatures
{   
    private static final Logger LOGGER = LoggerFactory.getLogger(JavaCXFServerCodegen.class);
    
    protected boolean useBeanValidation = false;
    
    protected boolean generateSpringApplication = false;
    
    protected boolean useSwaggerFeature = false;
    
    protected boolean useWadlFeature = false;
    
    protected boolean useMultipartFeature = false;
    
    protected boolean useGzipFeature = false;
    
    protected boolean useLoggingFeature = false;
    
    protected boolean useBeanValidationFeature = false;
    
    protected boolean generateSpringBootApplication= false;
    
    public JavaCXFServerCodegen()
    {
        super();

        supportsInheritance = true;
        
        artifactId = "swagger-cxf-server";
        
        outputFolder = "generated-code/JavaJaxRS-CXF";
        
        apiTemplateFiles.put("apiServiceImpl.mustache", ".java");
        
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
        cliOptions.add(CliOption.newBoolean(GENERATE_SPRING_APPLICATION, "Generate Spring application"));
        
        cliOptions.add(CliOption.newBoolean(USE_SWAGGER_FEATURE, "Use Swagger Feature"));
        cliOptions.add(CliOption.newBoolean(USE_WADL_FEATURE, "Use WADL Feature"));
        cliOptions.add(CliOption.newBoolean(USE_MULTIPART_FEATURE, "Use Multipart Feature"));
        cliOptions.add(CliOption.newBoolean(USE_GZIP_FEATURE, "Use Gzip Feature"));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION_FEATURE, "Use BeanValidation Feature"));
        cliOptions.add(CliOption.newBoolean(USE_LOGGING_FEATURE, "Use Logging Feature"));
        
        cliOptions.add(CliOption.newBoolean(GENERATE_SPRING_BOOT_APPLICATION, "Generate Spring Boot application"));
        
        
    }


    @Override
    public void processOpts()
    {
        super.processOpts();
        
        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            boolean useBeanValidationProp = convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION);
            this.setUseBeanValidation(useBeanValidationProp);
        }
        
        if (additionalProperties.containsKey(GENERATE_SPRING_APPLICATION)) {
            this.setGenerateSpringApplication(convertPropertyToBooleanAndWriteBack(GENERATE_SPRING_APPLICATION));
            
            this.setUseSwaggerFeature(convertPropertyToBooleanAndWriteBack(USE_SWAGGER_FEATURE));
            this.setUseWadlFeature(convertPropertyToBooleanAndWriteBack(USE_WADL_FEATURE));
            this.setUseMultipartFeature(convertPropertyToBooleanAndWriteBack(USE_MULTIPART_FEATURE));
            this.setUseGzipFeature(convertPropertyToBooleanAndWriteBack(USE_GZIP_FEATURE));
            this.setUseLoggingFeature(convertPropertyToBooleanAndWriteBack(USE_LOGGING_FEATURE));
            
            boolean useBeanValidationFeature = convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION_FEATURE);
            this.setUseBeanValidationFeature(useBeanValidationFeature);
            if (useBeanValidationFeature) {
                LOGGER.info("make sure your target server supports Bean Validation 1.1");
            }
            
            this.setGenerateSpringBootApplication(convertPropertyToBooleanAndWriteBack(GENERATE_SPRING_BOOT_APPLICATION));
        }
        
       
        supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen
        
        writeOptional(outputFolder, new SupportingFile("server/pom.mustache", "", "pom.xml"));
        
        if (this.generateSpringApplication) {
            writeOptional(outputFolder, new SupportingFile("server/readme.md", "", "readme.md"));
            
            writeOptional(outputFolder, new SupportingFile("server/ApplicationContext.xml.mustache",
                    ("src/main/resources"), "ApplicationContext.xml"));
            writeOptional(outputFolder, new SupportingFile("server/web.mustache",
                    ("src/main/webapp/WEB-INF"), "web.xml"));
            writeOptional(outputFolder, new SupportingFile("server/context.xml.mustache",
                    ("src/main/webapp/WEB-INF"), "context.xml"));
            
            // Jboss
            writeOptional(outputFolder, new SupportingFile("server/jboss-web.xml.mustache",
                    ("src/main/webapp/WEB-INF"), "jboss-web.xml"));
            
            // Spring Boot
            if (this.generateSpringBootApplication) {
                writeOptional(outputFolder, new SupportingFile("server/SpringBootApplication.mustache",
                        (testFolder + '/' + apiPackage).replace(".", "/"), "SpringBootApplication.java"));
                    
            }
            
        }
        
        
    }
    
    @Override
    public String getName()
    {
        return "jaxrs-cxf";
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
        return "Generates a Java JAXRS Server application based on Apache CXF framework.";
    }
    
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }
    
    public void setGenerateSpringApplication(boolean generateSpringApplication) {
        this.generateSpringApplication = generateSpringApplication;
    }


    public void setUseSwaggerFeature(boolean useSwaggerFeature) {
        this.useSwaggerFeature = useSwaggerFeature;
    }


    public void setUseWadlFeature(boolean useWadlFeature) {
        this.useWadlFeature = useWadlFeature;
    }


    public void setUseMultipartFeature(boolean useMultipartFeature) {
        this.useMultipartFeature = useMultipartFeature;
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
    
     public void setGenerateSpringBootApplication(boolean generateSpringBootApplication) {
        this.generateSpringBootApplication = generateSpringBootApplication;
    }
}
