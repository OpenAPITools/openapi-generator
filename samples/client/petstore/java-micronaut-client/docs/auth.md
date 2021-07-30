# Authorization methods
<a name="name"></a>
# api_key
Authorization method is of **api key type**. The parameter `api_key` is located in `header`.

The configuration for it can be found in [application.yml](src/main/resources/application.yml) at `security.api-key-auth.api_key`. The `api-key` parameter needs to be filled in order for it to work correctly.
<a name="name"></a>
# api_key_query
Authorization method is of **api key type**. The parameter `api_key_query` is located in `query`.

The configuration for it can be found in [application.yml](src/main/resources/application.yml) at `security.api-key-auth.api_key_query`. The `api-key` parameter needs to be filled in order for it to work correctly.
<a name="name"></a>
# http_basic_test
Authorization method is of **basic auth**.

The configuration for it can be found in [application.yml](src/main/resources/application.yml) at `security.basic-auth.http_basic_test`. `username` and `password` need to be filled for it to work correctly.
<a name="name"></a>
# petstore_auth
Authorization method is **OAuth2** with `implicit` flow.
The scopes are: 
    * `write:pets`
    * `read:pets`

The configuration for it can be found in [application.yml](src/main/resources/application.yml) at `micronaut.security.oauth2.clients.petstore_auth`. Some parameters may require to be filled in for it to work correctly.

More information on configuring can be found in [Micronaut Security Guide](https://micronaut-projects.github.io/micronaut-security/latest/guide/#oauth).
