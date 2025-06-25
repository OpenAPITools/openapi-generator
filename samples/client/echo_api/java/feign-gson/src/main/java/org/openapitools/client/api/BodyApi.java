package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import java.io.File;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.StringEnumRef;
import org.openapitools.client.model.Tag;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public interface BodyApi extends ApiClient.Api {


  /**
   * Test binary (gif) response body
   * Test binary (gif) response body
   * @return File
   */
  @RequestLine("POST /binary/gif")
  @Headers({
    "Accept: image/gif",
  })
  File testBinaryGif();

  /**
   * Test binary (gif) response body
   * Similar to <code>testBinaryGif</code> but it also returns the http response headers .
   * Test binary (gif) response body
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /binary/gif")
  @Headers({
    "Accept: image/gif",
  })
  ApiResponse<File> testBinaryGifWithHttpInfo();



  /**
   * Test body parameter(s)
   * Test body parameter(s)
   * @param body  (optional)
   * @return String
   */
  @RequestLine("POST /body/application/octetstream/binary")
  @Headers({
    "Content-Type: application/octet-stream",
    "Accept: text/plain",
  })
  String testBodyApplicationOctetstreamBinary(@javax.annotation.Nullable File body);

  /**
   * Test body parameter(s)
   * Similar to <code>testBodyApplicationOctetstreamBinary</code> but it also returns the http response headers .
   * Test body parameter(s)
   * @param body  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /body/application/octetstream/binary")
  @Headers({
    "Content-Type: application/octet-stream",
    "Accept: text/plain",
  })
  ApiResponse<String> testBodyApplicationOctetstreamBinaryWithHttpInfo(@javax.annotation.Nullable File body);



  /**
   * Test array of binary in multipart mime
   * Test array of binary in multipart mime
   * @param files  (required)
   * @return String
   */
  @RequestLine("POST /body/application/octetstream/array_of_binary")
  @Headers({
    "Content-Type: multipart/form-data",
    "Accept: text/plain",
  })
  String testBodyMultipartFormdataArrayOfBinary(@Param("files") @javax.annotation.Nonnull List<File> files);

  /**
   * Test array of binary in multipart mime
   * Similar to <code>testBodyMultipartFormdataArrayOfBinary</code> but it also returns the http response headers .
   * Test array of binary in multipart mime
   * @param files  (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /body/application/octetstream/array_of_binary")
  @Headers({
    "Content-Type: multipart/form-data",
    "Accept: text/plain",
  })
  ApiResponse<String> testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(@Param("files") @javax.annotation.Nonnull List<File> files);



  /**
   * Test single binary in multipart mime
   * Test single binary in multipart mime
   * @param myFile  (optional)
   * @return String
   */
  @RequestLine("POST /body/application/octetstream/single_binary")
  @Headers({
    "Content-Type: multipart/form-data",
    "Accept: text/plain",
  })
  String testBodyMultipartFormdataSingleBinary(@Param("my-file") @javax.annotation.Nullable File myFile);

  /**
   * Test single binary in multipart mime
   * Similar to <code>testBodyMultipartFormdataSingleBinary</code> but it also returns the http response headers .
   * Test single binary in multipart mime
   * @param myFile  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /body/application/octetstream/single_binary")
  @Headers({
    "Content-Type: multipart/form-data",
    "Accept: text/plain",
  })
  ApiResponse<String> testBodyMultipartFormdataSingleBinaryWithHttpInfo(@Param("my-file") @javax.annotation.Nullable File myFile);



  /**
   * Test body parameter(s)
   * Test body parameter(s)
   * @param pet Pet object that needs to be added to the store (optional)
   * @return Pet
   */
  @RequestLine("POST /echo/body/allOf/Pet")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Pet testEchoBodyAllOfPet(@javax.annotation.Nullable Pet pet);

  /**
   * Test body parameter(s)
   * Similar to <code>testEchoBodyAllOfPet</code> but it also returns the http response headers .
   * Test body parameter(s)
   * @param pet Pet object that needs to be added to the store (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /echo/body/allOf/Pet")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Pet> testEchoBodyAllOfPetWithHttpInfo(@javax.annotation.Nullable Pet pet);



  /**
   * Test free form object
   * Test free form object
   * @param body Free form object (optional)
   * @return String
   */
  @RequestLine("POST /echo/body/FreeFormObject/response_string")
  @Headers({
    "Content-Type: application/json",
    "Accept: text/plain",
  })
  String testEchoBodyFreeFormObjectResponseString(@javax.annotation.Nullable Object body);

  /**
   * Test free form object
   * Similar to <code>testEchoBodyFreeFormObjectResponseString</code> but it also returns the http response headers .
   * Test free form object
   * @param body Free form object (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /echo/body/FreeFormObject/response_string")
  @Headers({
    "Content-Type: application/json",
    "Accept: text/plain",
  })
  ApiResponse<String> testEchoBodyFreeFormObjectResponseStringWithHttpInfo(@javax.annotation.Nullable Object body);



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
  Pet testEchoBodyPet(@javax.annotation.Nullable Pet pet);

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
  ApiResponse<Pet> testEchoBodyPetWithHttpInfo(@javax.annotation.Nullable Pet pet);



  /**
   * Test empty response body
   * Test empty response body
   * @param pet Pet object that needs to be added to the store (optional)
   * @return String
   */
  @RequestLine("POST /echo/body/Pet/response_string")
  @Headers({
    "Content-Type: application/json",
    "Accept: text/plain",
  })
  String testEchoBodyPetResponseString(@javax.annotation.Nullable Pet pet);

  /**
   * Test empty response body
   * Similar to <code>testEchoBodyPetResponseString</code> but it also returns the http response headers .
   * Test empty response body
   * @param pet Pet object that needs to be added to the store (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /echo/body/Pet/response_string")
  @Headers({
    "Content-Type: application/json",
    "Accept: text/plain",
  })
  ApiResponse<String> testEchoBodyPetResponseStringWithHttpInfo(@javax.annotation.Nullable Pet pet);



  /**
   * Test string enum response body
   * Test string enum response body
   * @param body String enum (optional)
   * @return StringEnumRef
   */
  @RequestLine("POST /echo/body/string_enum")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  StringEnumRef testEchoBodyStringEnum(@javax.annotation.Nullable String body);

  /**
   * Test string enum response body
   * Similar to <code>testEchoBodyStringEnum</code> but it also returns the http response headers .
   * Test string enum response body
   * @param body String enum (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /echo/body/string_enum")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<StringEnumRef> testEchoBodyStringEnumWithHttpInfo(@javax.annotation.Nullable String body);



  /**
   * Test empty json (request body)
   * Test empty json (request body)
   * @param tag Tag object (optional)
   * @return String
   */
  @RequestLine("POST /echo/body/Tag/response_string")
  @Headers({
    "Content-Type: application/json",
    "Accept: text/plain",
  })
  String testEchoBodyTagResponseString(@javax.annotation.Nullable Tag tag);

  /**
   * Test empty json (request body)
   * Similar to <code>testEchoBodyTagResponseString</code> but it also returns the http response headers .
   * Test empty json (request body)
   * @param tag Tag object (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /echo/body/Tag/response_string")
  @Headers({
    "Content-Type: application/json",
    "Accept: text/plain",
  })
  ApiResponse<String> testEchoBodyTagResponseStringWithHttpInfo(@javax.annotation.Nullable Tag tag);


}
