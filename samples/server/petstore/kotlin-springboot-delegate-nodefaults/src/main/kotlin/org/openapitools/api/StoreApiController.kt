package org.openapitools.api

import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestMapping
import java.util.Optional

@jakarta.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.14.0-SNAPSHOT")
@Controller
class StoreApiController(
        private val delegate: StoreApiDelegate
) : StoreApi {

    override fun getDelegate(): StoreApiDelegate = delegate
}
