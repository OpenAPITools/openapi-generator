package org.openapitools

import org.springframework.boot.runApplication
import org.springframework.context.annotation.ComponentScan
import org.springframework.boot.autoconfigure.SpringBootApplication


@SpringBootApplication
@ComponentScan(basePackages = ["org.openapitools", "org.openapitools.api", "org.openapitools.model"])
class Application

fun main(args: Array<String>) {
    runApplication<Application>(*args)
}
