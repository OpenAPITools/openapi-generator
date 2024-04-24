package org.openapitools.configuration;


import java.util.Properties;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.PropertiesPropertySource;

@Configuration(value = "org.openapitools.configuration.clientPropertiesConfiguration")
public class ClientPropertiesConfiguration {

    public ClientPropertiesConfiguration( final ConfigurableEnvironment configurableEnvironment ) {
        final Properties oAuth2Application = new Properties();
        oAuth2Application.put("spring.security.oauth2.client.registration.oAuth2Application.client-id", "set-oAuth2Application-client-id" );
        oAuth2Application.put("spring.security.oauth2.client.registration.oAuth2Application.authorization-grant-type", "client_credentials" );
        oAuth2Application.put("spring.security.oauth2.client.provider.oAuth2Application.token-uri", "/openid-connect/token" );
        configurableEnvironment.getPropertySources().addLast( new PropertiesPropertySource("oAuth2Application", oAuth2Application ) );
        final Properties oAuth2AccessCode = new Properties();
        oAuth2AccessCode.put("spring.security.oauth2.client.registration.oAuth2AccessCode.client-id", "set-oAuth2AccessCode-client-id" );
        oAuth2AccessCode.put("spring.security.oauth2.client.registration.oAuth2AccessCode.scope", "openid,profile,aud" );
        oAuth2AccessCode.put("spring.security.oauth2.client.registration.oAuth2AccessCode.authorization-grant-type", "authorization_code" );
        oAuth2AccessCode.put("spring.security.oauth2.client.registration.oAuth2AccessCode.redirect-uri", "set-oAuth2AccessCode-redirect-uri" );
        oAuth2AccessCode.put("spring.security.oauth2.client.provider.oAuth2AccessCode.token-uri", "${tokenUrl}" );
        oAuth2AccessCode.put("spring.security.oauth2.client.provider.oAuth2AccessCode.authorization-uri", "${authorizationUrl}" );
        configurableEnvironment.getPropertySources().addLast( new PropertiesPropertySource("oAuth2AccessCode", oAuth2AccessCode ) );
    }

}
