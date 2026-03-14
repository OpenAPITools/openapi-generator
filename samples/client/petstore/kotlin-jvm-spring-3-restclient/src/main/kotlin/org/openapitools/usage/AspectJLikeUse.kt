package org.openapitools.usage

import org.openapitools.client.apis.PetApi
import org.springframework.web.client.RestClient

// 'regression test' that API classes are open https://github.com/OpenAPITools/openapi-generator/issues/22271
class AspectJLikeUse(client: RestClient) : PetApi(client)
