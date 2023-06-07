package org.openapitools

import org.springframework.context.annotation.Bean
import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.RequestMapping
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper
import org.springframework.beans.factory.annotation.Value
import org.springframework.core.io.Resource
import org.springframework.util.StreamUtils
import org.springframework.web.bind.annotation.ResponseBody
import org.springframework.web.bind.annotation.GetMapping
import java.nio.charset.Charset

/**
 * Home redirection to OpenAPI api documentation
 */
@Controller
class HomeController {
    private val apiDocsPath = "/openapi.json"
    private val yamlMapper = YAMLMapper()

    @Value("classpath:/openapi.yaml")
    private lateinit var openapi: Resource

    @Bean
    fun openapiContent(): String {
        return openapi.inputStream.use {
            StreamUtils.copyToString(it, Charset.defaultCharset())
        }
    }

    @GetMapping(value = ["/openapi.yaml"], produces = ["application/vnd.oai.openapi"])
    @ResponseBody
    fun openapiYaml(): String = openapiContent()

    @GetMapping(value = ["/openapi.json"], produces = ["application/json"])
    @ResponseBody
    fun openapiJson(): Any = yamlMapper.readValue(openapiContent(), Any::class.java)

    @GetMapping(value = ["/swagger-config.yaml"], produces = ["text/plain"])
    @ResponseBody
    fun swaggerConfig(): String = "url: $apiDocsPath\n"

    @RequestMapping("/")
    fun index(): String = "redirect:swagger-ui.html"
}
