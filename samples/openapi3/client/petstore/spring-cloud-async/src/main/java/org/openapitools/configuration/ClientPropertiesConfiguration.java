package org.openapitools.configuration;

import org.springframework.context.annotation.PropertySource;
import org.springframework.context.annotation.Configuration;

@PropertySource( "classpath:/oauth2-client.properties" )
@Configuration
public class ClientPropertiesConfiguration {

}
