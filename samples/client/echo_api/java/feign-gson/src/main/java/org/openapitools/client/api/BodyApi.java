package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import org.openapitools.client.model.Pet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public interface BodyApi extends ApiClient.Api {


  /**
   * Test body parameter(s)
   * Test body parameter(s)
   * @param pet Pet object that needs to be added to the store (optional)
   * @return Pet
   */
  @RequestLine("POST /echo/body/Pet")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Pet testEchoBodyPet(Pet pet);

  /**
   * Test body parameter(s)
   * Similar to <code>testEchoBodyPet</code> but it also returns the http response headers .
   * Test body parameter(s)
   * @param pet Pet object that needs to be added to the store (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /echo/body/Pet")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Pet> testEchoBodyPetWithHttpInfo(Pet pet);


}
