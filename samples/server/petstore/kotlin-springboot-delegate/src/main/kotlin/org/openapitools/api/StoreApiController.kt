package org.openapitools.api

import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestMapping
import java.util.Optional

@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.14.0-SNAPSHOT")
@Controller
class StoreApiController(
        delegate: StoreApiDelegate?
) : StoreApi {
    private lateinit var delegate: StoreApiDelegate

    init {
        this.delegate = Optional.ofNullable(delegate).orElse(object : StoreApiDelegate {})
    }

    override fun getDelegate(): StoreApiDelegate = delegate
}
