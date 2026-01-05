package org.openapitools.api

import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestMapping
import java.util.Optional

@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.19.0-SNAPSHOT")
@Controller
@RequestMapping("\${api.base-path:/v2}")
class UserApiController(
        delegate: UserApiDelegate?
) : UserApi {
    private lateinit var delegate: UserApiDelegate

    init {
        this.delegate = Optional.ofNullable(delegate).orElse(object : UserApiDelegate {})
    }

    override fun getDelegate(): UserApiDelegate = delegate

    companion object {
    //for your own safety never directly reuse these path definitions in tests
        const val BASE_PATH: String = "/v2"
    }

}
