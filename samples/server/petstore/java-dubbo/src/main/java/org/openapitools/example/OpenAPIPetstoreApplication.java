package org.openapitools.example;

import org.apache.dubbo.config.spring.context.annotation.EnableDubbo;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
@EnableDubbo(scanBasePackages = "org.openapitools.example")
public class OpenAPIPetstoreApplication {

    public static void main(String[] args) {
        SpringApplication.run(OpenAPIPetstoreApplication.class, args);
    }

}
