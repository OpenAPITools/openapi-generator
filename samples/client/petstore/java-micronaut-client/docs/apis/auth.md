# Authorization methods
<a name="name"></a>
# api_key
Authorization method is of **api key type**. The parameter `api_key` is located in `header`.

It can be configured in the [application.yml](src/main/resources/application.yml) using Micronaut security features.
<a name="name"></a>
# api_key_query
Authorization method is of **api key type**. The parameter `api_key_query` is located in `query`.

It can be configured in the [application.yml](src/main/resources/application.yml) using Micronaut security features.
<a name="name"></a>
# http_basic_test
Authorization method is of **basic auth**.

It can be configured in the [application.yml](src/main/resources/application.yml) using Micronaut security features.
<a name="name"></a>
# petstore_auth
Authorization method is **OAuth2** with `implicit` flow.
The scopes are: 
    * `write:pets`
    * `read:pets`

It can be configured in the [application.yml](src/main/resources/application.yml) using Micronaut security features.

More information on configuring can be found in [Micronaut Security Guide](https://micronaut-projects.github.io/micronaut-security/latest/guide/#oauth).
