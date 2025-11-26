package org.openapitools.api

import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestMapping
import java.util.Optional
import org.openapitools.api.UserApiController.Companion.BASE_PATH

@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.18.0-SNAPSHOT")
@Controller
@RequestMapping("\${openapi.openAPIPetstore.base-path:\${api.base-path:$BASE_PATH}}")
class UserApiController(
        private val delegate: UserApiDelegate
) : UserApi {

    override fun getDelegate(): UserApiDelegate = delegate

    companion object {
    //for your own safety never directly reuse these path definitions in tests
        const val BASE_PATH: String = "/v2"
    }

}
