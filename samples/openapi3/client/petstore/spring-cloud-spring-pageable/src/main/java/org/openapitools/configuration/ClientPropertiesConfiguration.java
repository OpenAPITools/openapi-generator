package org.openapitools.configuration;


import java.util.Properties;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.PropertiesPropertySource;

@Configuration(value = "org.openapitools.configuration.clientPropertiesConfiguration")
public class ClientPropertiesConfiguration {

    public ClientPropertiesConfiguration( final ConfigurableEnvironment configurableEnvironment ) {
        final Properties petstoreAuthImplicit = new Properties();
        petstoreAuthImplicit.put("spring.security.oauth2.client.registration.petstoreAuthImplicit.client-id", "set-petstoreAuthImplicit-client-id" );
        petstoreAuthImplicit.put("spring.security.oauth2.client.registration.petstoreAuthImplicit.scope", "write:pets,read:pets" );
        petstoreAuthImplicit.put("spring.security.oauth2.client.registration.petstoreAuthImplicit.authorization-grant-type", "implicit" );
        petstoreAuthImplicit.put("spring.security.oauth2.client.provider.petstoreAuthImplicit.authorization-uri", "http://petstore.swagger.io/api/oauth/dialog" );
        configurableEnvironment.getPropertySources().addLast( new PropertiesPropertySource("petstoreAuthImplicit", petstoreAuthImplicit ) );
    }

}
