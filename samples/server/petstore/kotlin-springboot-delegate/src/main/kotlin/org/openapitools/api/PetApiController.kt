package org.openapitools.api

import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestMapping
import java.util.Optional

@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.17.0-SNAPSHOT")
@Controller
class PetApiController(
        delegate: PetApiDelegate?
) : PetApi {
    private lateinit var delegate: PetApiDelegate

    init {
        this.delegate = Optional.ofNullable(delegate).orElse(object : PetApiDelegate {})
    }

    override fun getDelegate(): PetApiDelegate = delegate
}
