package org.openapitools.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.GetMapping;

/**
 * Home redirection to OpenAPI api documentation
 */
@Controller
public class HomeController {

    @GetMapping(value = "/swagger-config.yaml", produces = "text/plain")
    @ResponseBody
    public String swaggerConfig() {
        return "url: /v2/api-docs\n";
    }
    @RequestMapping("/")
    public String index() {
        return "redirect:swagger-ui.html";
    }

}
