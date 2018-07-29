package com.prokarma.pkmst.config.swagger;

import java.util.Date;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StopWatch;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
//import static springfox.documentation.builders.PathSelectors.regex;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;
/**
 * Enable swagger ui for application
 * @author pkmst
 *
 */
@EnableSwagger2
@Configuration
public class SwaggerDocumentationConfig {

    public static final String DEFAULT_INCLUDE_PATTERN = "/pkmst/.*";
	 @Bean
	    public Docket swaggerSpringfoxDocket(PkmstProperties pkmstProperties) {
	        StopWatch watch = new StopWatch();
	        watch.start();
	        Contact contact = new Contact(
	        		pkmstProperties.getSwagger().getContactName(),
	        		pkmstProperties.getSwagger().getContactUrl(),
	        		pkmstProperties.getSwagger().getContactEmail());

	        ApiInfo apiInfo = new ApiInfo(
	        		pkmstProperties.getSwagger().getTitle(),
	        		pkmstProperties.getSwagger().getDescription(),
	        		pkmstProperties.getSwagger().getVersion(),
	        		pkmstProperties.getSwagger().getTermsOfServiceUrl(),
	            contact,
	            pkmstProperties.getSwagger().getLicense(),
	            pkmstProperties.getSwagger().getLicenseUrl());

	        Docket docket = new Docket(DocumentationType.SWAGGER_2)
	            .apiInfo(apiInfo)
	            .forCodeGeneration(true)
	            .genericModelSubstitutes(ResponseEntity.class)
	            .ignoredParameterTypes(java.sql.Date.class)
	            .directModelSubstitute(java.time.LocalDate.class, java.sql.Date.class)
	            .directModelSubstitute(java.time.ZonedDateTime.class, Date.class)
	            .directModelSubstitute(java.time.LocalDateTime.class, Date.class)
	            .select()
	            .apis(RequestHandlerSelectors.basePackage("com.prokarma.pkmst"))
	           // .paths(regex(DEFAULT_INCLUDE_PATTERN))
	            .paths(PathSelectors.any())
	            .build();
	        watch.stop();
	        return docket;
	    }

}