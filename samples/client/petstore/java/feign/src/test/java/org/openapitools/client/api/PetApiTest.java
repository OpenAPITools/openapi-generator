package org.openapitools.client.api;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.matching.MultipartValuePatternBuilder;
import com.google.common.collect.Sets;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Pet;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * API tests for PetAp
 */
class PetApiTest {

  private static PetApi api;

  private static WireMockServer wm = new WireMockServer(options().dynamicPort());
  private static String petJson;
  private static String petListJson;

  private ObjectMapper objectMapper = new ObjectMapper();

  @BeforeAll
  static void setup() throws IOException {
    wm.start();

    ApiClient apiClient = new ApiClient();
    apiClient.setBasePath(wm.baseUrl());
    api = apiClient.buildClient(PetApi.class);

    petJson = IOUtils.toString(PetApiTest.class.getResourceAsStream("/pet.json"), "UTF-8");
    petListJson = IOUtils.toString(PetApiTest.class.getResourceAsStream("/pet_list.json"), "UTF-8");
  }

  @AfterAll
  static void shutdown() {
    wm.shutdown();
  }

  @Test
  void addPet() throws JsonProcessingException {
    wm.stubFor(post(urlEqualTo("/pet"))
            .willReturn(aResponse().withBody(petJson)));

    Pet pet = objectMapper.readValue(petJson, Pet.class);

    api.addPet(pet);

    wm.verify(postRequestedFor(urlEqualTo("/pet"))
            .withHeader("Content-Type", equalTo("application/json"))
            .withHeader("Accept", equalTo("application/json"))
            .withRequestBody(equalToJson(petJson)));
  }

  @Test
  void deletedPet() {
    wm.stubFor(delete(urlEqualTo("/pet/85"))
            .willReturn(aResponse()));

    api.deletePet(85L, "API_KEY");

    wm.verify(deleteRequestedFor(urlEqualTo("/pet/85"))
            .withHeader("api_key", equalTo("API_KEY"))
            .withHeader("Accept", equalTo("application/json")));
  }

  @Test
  void findPetsByStatus() {
    wm.stubFor(get(urlEqualTo("/pet/findByStatus?status=available&status=sold"))
            .willReturn(aResponse()
                    .withHeader("Content-Type", "application/json")
                    .withBody(petListJson)));

    List<Pet> petList = api.findPetsByStatus(Arrays.asList("available", "sold"));
    assertThat(petList.size(), is(2));

    validatePet1(petList.get(0));
    validatePet2(petList.get(1));
  }

  @Test
  void findPetsByStatusQueryMap() {
    wm.stubFor(get(urlEqualTo("/pet/findByStatus?status=available,sold"))
            .willReturn(aResponse()
                    .withHeader("Content-Type", "application/json")
                    .withBody(petListJson)));

    PetApi.FindPetsByStatusQueryParams findPetsByStatusQueryParams = new PetApi.FindPetsByStatusQueryParams();
    findPetsByStatusQueryParams.status(Arrays.asList("available", "sold"));

    List<Pet> petList = api.findPetsByStatus(findPetsByStatusQueryParams);
    assertThat(petList.size(), is(2));

    validatePet1(petList.get(0));
    validatePet2(petList.get(1));
  }

  @Test
  void findPetsByTags() {
    wm.stubFor(get(urlEqualTo("/pet/findByTags?tags=tag1&tags=tag2"))
            .willReturn(aResponse()
                    .withHeader("Content-Type", "application/json")
                    .withBody(petListJson)));

    Set<Pet> petList = api.findPetsByTags(Sets.newHashSet("tag1", "tag2"));
    assertThat(petList.size(), is(2));
  }

  @Test
  void getPetById() {
    wm.stubFor(get(urlEqualTo("/pet/85"))
            .willReturn(aResponse()
                    .withHeader("Content-Type", "application/json")
                    .withBody(petJson)));

    Pet pet = api.getPetById(85L);

    validatePet1(pet);
  }

  @Test
  void updatePet() throws JsonProcessingException {
    wm.stubFor(put(urlEqualTo("/pet"))
            .willReturn(aResponse()
                    .withHeader("Content-Type", "application/json")
                    .withBody(petJson)));

    Pet pet = objectMapper.readValue(petJson, Pet.class);
    api.updatePet(pet);

    wm.verify(putRequestedFor(urlEqualTo("/pet"))
            .withHeader("Accept", equalTo("application/json"))
            .withHeader("Content-Type", equalTo("application/json"))
            .withRequestBody(equalToJson(petJson)));
  }

  @Test
  void updatePetWithForm() {
    wm.stubFor(post(anyUrl()).willReturn(aResponse()));

    api.updatePetWithForm(85L, "Rex", "sold");

    wm.verify(postRequestedFor(urlEqualTo("/pet/85"))
            .withHeader("Accept", equalTo("application/json"))
            .withHeader("Content-Type", containing("application/x-www-form-urlencoded"))
            .withRequestBody(containing("name=Rex"))
            .withRequestBody(containing("status=sold")));
  }

  @Test
  void uploadFile() throws IOException {
    wm.stubFor(post("/pet/85/uploadImage").willReturn(aResponse()));
    File file = File.createTempFile("test", ".tmp");
    IOUtils.write("ABCD".getBytes(), new FileOutputStream(file));

    api.uploadFile(85L, "metadata", file);

    wm.verify(postRequestedFor(urlEqualTo("/pet/85/uploadImage"))
            .withHeader("Content-Type", containing("multipart/form-data"))
            .withHeader("Accept", containing("application/json"))
            .withRequestBodyPart(new MultipartValuePatternBuilder()
                    .withName("additionalMetadata").build())
            .withRequestBodyPart(new MultipartValuePatternBuilder()
                    .withName("file").withBody(binaryEqualTo("ABCD".getBytes())).build())
    );
  }

  private void validatePet1(Pet pet) {
    assertThat(pet.getId(), is(85L));
    assertThat(pet.getCategory().getName(), is("Dogs"));
    assertThat(pet.getCategory().getId(), is(1L));
    assertThat(pet.getName(), is("LvRcat"));
    assertThat(pet.getPhotoUrls().size(), is(1));
    assertThat(pet.getPhotoUrls().stream().findAny().get(), is("string"));
    assertThat(pet.getTags().size(), is(1));
    assertThat(pet.getTags().get(0).getId(), is(10L));
    assertThat(pet.getTags().get(0).getName(), is("tag"));
    assertThat(pet.getStatus(), is(Pet.StatusEnum.AVAILABLE));
  }

  private void validatePet2(Pet pet) {
    assertThat(pet.getId(), is(42L));
    assertThat(pet.getCategory().getName(), is("Dogs"));
    assertThat(pet.getCategory().getId(), is(1L));
    assertThat(pet.getName(), is("Louise"));
    assertThat(pet.getPhotoUrls().size(), is(1));
    assertThat(pet.getPhotoUrls().stream().findAny().get(), is("photo"));
    assertThat(pet.getTags().size(), is(1));
    assertThat(pet.getTags().get(0).getId(), is(0L));
    assertThat(pet.getTags().get(0).getName(), is("obedient"));
    assertThat(pet.getStatus(), is(Pet.StatusEnum.SOLD));
  }
}
