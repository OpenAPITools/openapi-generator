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

    static final String API_DOCS_PATH = "/v2/api-docs";

    @GetMapping(value = "/swagger-config.yaml", produces = "text/plain")
    @ResponseBody
    public String swaggerConfig() {
        return "url: " + API_DOCS_PATH + "\n";
    }

    @RequestMapping("/")
    public String index() {
        return "redirect:swagger-ui.html";
    }

}