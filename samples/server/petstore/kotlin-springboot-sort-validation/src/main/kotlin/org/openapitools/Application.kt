package org.openapitools

import com.fasterxml.jackson.databind.Module
import org.openapitools.jackson.nullable.JsonNullableModule
import org.springframework.context.annotation.Bean
import org.springframework.boot.runApplication
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.context.annotation.ComponentScan

@SpringBootApplication
@ComponentScan(basePackages = ["org.openapitools", "org.openapitools.api", "org.openapitools.model"])
class Application {
    @Bean(name = ["org.openapitools.Application.jsonNullableModule"])
    fun jsonNullableModule(): Module = JsonNullableModule()
}

fun main(args: Array<String>) {
    runApplication<Application>(*args)
}
