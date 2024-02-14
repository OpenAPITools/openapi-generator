package org.openapitools.configuration;

import java.util.Properties;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.PropertiesPropertySource;

@Configuration(value = "org.openapitools.configuration.clientPropertiesConfiguration")
public class ClientPropertiesConfiguration {

    public ClientPropertiesConfiguration( final ConfigurableEnvironment configurableEnvironment ) {
        final Properties properties = new Properties();
        properties.put("spring.security.oauth2.client.registration.petstoreAuthImplicit.client-id", "set-petstoreAuthImplicit-client-id" );
        properties.put("spring.security.oauth2.client.registration.petstoreAuthImplicit.scope", "write:pets,read:pets" );
        properties.put("spring.security.oauth2.client.registration.petstoreAuthImplicit.authorization-grant-type", "implicit" );
        properties.put("spring.security.oauth2.client.provider.petstoreAuthImplicit.authorization-uri", "http://petstore.swagger.io/api/oauth/dialog" );
        final PropertiesPropertySource propertiesPropertySource = new PropertiesPropertySource("petstoreAuthImplicit", properties );
        configurableEnvironment.getPropertySources().addLast( propertiesPropertySource );
    }

}
