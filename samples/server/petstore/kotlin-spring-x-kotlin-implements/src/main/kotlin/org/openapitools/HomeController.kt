package org.openapitools

import org.springframework.context.annotation.Bean
import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.ResponseBody
import org.springframework.web.bind.annotation.GetMapping

/**
 * Home redirection to OpenAPI api documentation
 */
@Controller
class HomeController {

    @RequestMapping("/")
    fun index(): String = "redirect:swagger-ui.html"
}
