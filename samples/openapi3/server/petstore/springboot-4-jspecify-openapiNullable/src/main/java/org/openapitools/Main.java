package org.openapitools;

import org.openapitools.api.RequiredAndNullableApi;
import org.openapitools.model.RequiredAndNullable;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
public class Main {
    public static void main(String[] args) {
        new RequiredAndNullable()._list(null).addListItem("test");
        //SpringApplication.run(Main.class, args);
    }



    @RestController
    public static class MyRestController implements RequiredAndNullableApi {
        @Override
        public ResponseEntity<RequiredAndNullable> requiredAndNullablePost(RequiredAndNullable requiredAndNullable) {
            return RequiredAndNullableApi.super.requiredAndNullablePost(requiredAndNullable);
        }
    }
}
