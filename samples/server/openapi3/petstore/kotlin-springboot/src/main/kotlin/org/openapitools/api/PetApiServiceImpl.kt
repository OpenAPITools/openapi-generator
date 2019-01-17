package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.stereotype.Service

@Service
class PetApiServiceImpl : PetApiService {

    override fun addPet(pet: Pet): Unit {
        TODO("Implement me")
    }

    override fun deletePet(petId: Long,apiKey: String): Unit {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: List<String>): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: List<String>): List<Pet> {
        TODO("Implement me")
    }

    override fun getPetById(petId: Long): Pet {
        TODO("Implement me")
    }

    override fun updatePet(pet: Pet): Unit {
        TODO("Implement me")
    }

    override fun updatePetWithForm(petId: Long,name: String,status: String): Unit {
        TODO("Implement me")
    }

    override fun uploadFile(petId: Long,additionalMetadata: String,file: org.springframework.web.multipart.MultipartFile): ModelApiResponse {
        TODO("Implement me")
    }
}
