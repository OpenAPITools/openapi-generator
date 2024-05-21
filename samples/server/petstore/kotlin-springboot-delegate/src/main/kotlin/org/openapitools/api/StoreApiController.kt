package org.openapitools.api

import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestMapping
import java.util.Optional

@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.7.0-SNAPSHOT")
@Controller
@RequestMapping("\${openapi.openAPIPetstore.base-path:/v2}")
class StoreApiController(
        @org.springframework.beans.factory.annotation.Autowired(required = false) delegate: StoreApiDelegate?
) : StoreApi {
    private lateinit var delegate: StoreApiDelegate

    init {
        this.delegate = Optional.ofNullable(delegate).orElse(object : StoreApiDelegate {})
    }

    override fun getDelegate(): StoreApiDelegate = delegate
}
