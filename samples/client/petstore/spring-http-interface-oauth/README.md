# Spring HTTP Interface with OAuth2 (@ClientRegistrationId)

This sample demonstrates the use of the `@ClientRegistrationId` annotation with Spring HTTP Interface clients.

## Feature

When generating Spring HTTP Interface clients, you can now specify a `clientRegistrationId` parameter to automatically add the `@ClientRegistrationId` annotation to all generated interface methods.

## Configuration

Add the `clientRegistrationId` property to your generator configuration:

```yaml
generatorName: spring
library: spring-http-interface
additionalProperties:
  clientRegistrationId: "petstore-oauth"
```

Or via command line:

```bash
openapi-generator-cli generate \
  -g spring \
  --library spring-http-interface \
  --additional-properties clientRegistrationId=petstore-oauth \
  -i petstore.yaml \
  -o ./output
```

## Generated Code

The generated interface methods will include the `@ClientRegistrationId` annotation:

```java
@ClientRegistrationId("petstore-oauth")
@HttpExchange(
    method = "GET",
    value = "/pet/{petId}",
    accept = { "application/json" }
)
ResponseEntity<PetDto> getPetById(@PathVariable("petId") Long petId);
```

## Spring Security Integration

This annotation is part of Spring Security's OAuth2 integration for HTTP Service Clients. It automatically associates OAuth2 tokens with HTTP requests.

### Requirements

- Spring Security 7.0+
- Spring Boot 3.x

### Configuration

Configure your Spring application with the OAuth2 client registration:

```yaml
spring:
  security:
    oauth2:
      client:
        registration:
          petstore-oauth:
            client-id: your-client-id
            client-secret: your-client-secret
            authorization-grant-type: client_credentials
            scope: read,write
        provider:
          petstore-oauth:
            token-uri: https://auth.example.com/oauth/token
```

### Bean Configuration

Use `OAuth2RestClientHttpServiceGroupConfigurer` to configure the HTTP Service Proxy Factory:

```java
@Configuration
public class HttpInterfaceConfig {

    @Bean
    public PetApi petApi(OAuth2RestClientHttpServiceGroupConfigurer configurer) {
        RestClient.Builder builder = RestClient.builder()
            .baseUrl("https://petstore.example.com/v2");

        configurer.configure(builder);

        RestClient restClient = builder.build();
        RestClientAdapter adapter = RestClientAdapter.create(restClient);
        HttpServiceProxyFactory factory = HttpServiceProxyFactory
            .builderFor(adapter)
            .build();

        return factory.createClient(PetApi.class);
    }
}
```

## References

- [Spring Security HTTP Service Client Integration](https://docs.spring.io/spring-security/reference/features/integrations/rest/http-service-client.html)
- [Spring Security ClientRegistrationId API](https://docs.spring.io/spring-security/site/docs/current/api/org/springframework/security/oauth2/client/annotation/ClientRegistrationId.html)
