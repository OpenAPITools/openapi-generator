package org.openapitools.api

import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestMapping
import java.util.Optional

@jakarta.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.17.0-SNAPSHOT")
@Controller
class UserApiController(
        private val delegate: UserApiDelegate
) : UserApi {

    override fun getDelegate(): UserApiDelegate = delegate
}
