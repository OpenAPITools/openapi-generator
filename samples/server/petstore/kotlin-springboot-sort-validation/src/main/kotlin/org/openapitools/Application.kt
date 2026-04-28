package org.openapitools

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.ComponentScan

@SpringBootApplication
@ComponentScan("org.openapitools")
class Application

fun main(args: Array<String>) {
    runApplication<Application>(*args)
}
