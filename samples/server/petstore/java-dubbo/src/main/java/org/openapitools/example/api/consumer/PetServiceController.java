package org.openapitools.example.api.consumer;

import org.openapitools.example.model.ModelApiResponse;
import org.openapitools.example.model.Pet;
import org.openapitools.example.model.*;
import org.openapitools.example.api.interfaces.PetService;
import java.util.List;
import java.util.Map;
import java.time.OffsetDateTime;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.apache.dubbo.config.annotation.DubboReference;
import org.springframework.web.bind.annotation.*;
import javax.annotation.Generated;


@Generated(value = "org.openapitools.codegen.languages.JavaDubboServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")

@RestController
@RequestMapping("/pet")
public class PetServiceController {

    @DubboReference
    private PetService petService;

    @RequestMapping(method = RequestMethod.POST, value = "/")
    public Pet addPet(
        @RequestParam(name = "pet") Pet pet
    ) {
        return petService.addPet(pet);
    }

    @RequestMapping(method = RequestMethod.DELETE, value = "/{petId}")
    public void deletePet(
        @RequestParam(name = "petId") Long petId,
        @RequestParam(name = "apiKey") String apiKey
    ) {
        petService.deletePet(petId, apiKey);
    }

    @RequestMapping(method = RequestMethod.GET, value = "/findByStatus")
    public List<Pet> findPetsByStatus(
        @RequestParam(name = "status") List<String> status
    ) {
        return petService.findPetsByStatus(status);
    }

    @RequestMapping(method = RequestMethod.GET, value = "/findByTags")
    public List<Pet> findPetsByTags(
        @RequestParam(name = "tags") List<String> tags
    ) {
        return petService.findPetsByTags(tags);
    }

    @RequestMapping(method = RequestMethod.GET, value = "/{petId}")
    public Pet getPetById(
        @RequestParam(name = "petId") Long petId
    ) {
        return petService.getPetById(petId);
    }

    @RequestMapping(method = RequestMethod.PUT, value = "/")
    public Pet updatePet(
        @RequestParam(name = "pet") Pet pet
    ) {
        return petService.updatePet(pet);
    }

    @RequestMapping(method = RequestMethod.POST, value = "/{petId}")
    public void updatePetWithForm(
        @RequestParam(name = "petId") Long petId,
        @RequestParam(name = "name") String name,
        @RequestParam(name = "status") String status
    ) {
        petService.updatePetWithForm(petId, name, status);
    }

    @RequestMapping(method = RequestMethod.POST, value = "/{petId}/uploadImage")
    public ModelApiResponse uploadFile(
        @RequestParam(name = "petId") Long petId,
        @RequestParam(name = "additionalMetadata") String additionalMetadata,
        @RequestParam(name = "_file") org.springframework.web.multipart.MultipartFile _file
    ) {
        return petService.uploadFile(petId, additionalMetadata, _file);
    }
}
