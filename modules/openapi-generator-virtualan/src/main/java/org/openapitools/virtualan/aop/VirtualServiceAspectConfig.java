package org.openapitools.virtualan.aop;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
@Configuration
@EnableAspectJAutoProxy
@ComponentScan(basePackages = { "org.openapitools.virtualan.aop"})
public class VirtualServiceAspectConfig {

	  @Bean
	  public ApiVirtualAspect myApiVirtualAspect() {
	    return new ApiVirtualAspect();
	  }
	
}  