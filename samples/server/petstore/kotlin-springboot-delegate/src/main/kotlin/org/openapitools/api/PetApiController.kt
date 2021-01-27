package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import java.util.Optional;
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"])

@Controller
@RequestMapping("\${openapi.openAPIPetstore.base-path:/v2}")
class PetApiController(
        @org.springframework.beans.factory.annotation.Autowired(required = false) delegate: PetApiDelegate?
) : PetApi {
    private val delegate: PetApiDelegate

    init {
        this.delegate = Optional.ofNullable(delegate).orElse(object : PetApiDelegate {})
    }

    override fun getDelegate(): PetApiDelegate = delegate
}
