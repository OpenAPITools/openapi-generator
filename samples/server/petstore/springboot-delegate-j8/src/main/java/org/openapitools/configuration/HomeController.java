package org.openapitools.configuration;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;


/**
 * Home redirection to OpenAPI api documentation
 */
@Controller
public class HomeController {

    @RequestMapping("/")
    public String index() {
        return "redirect:swagger-ui.html";
    }


}
