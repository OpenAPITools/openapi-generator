package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.stereotype.Service
@Service
class PetApiServiceImpl : PetApiService {

    override fun addPet(body: Pet): Unit {
        TODO("Implement me")
    }

    override fun deletePet(petId: kotlin.Long, apiKey: kotlin.String?): Unit {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): List<Pet> {
        TODO("Implement me")
    }

    override fun getImage(petId: kotlin.Long): org.springframework.core.io.Resource {
        TODO("Implement me")
    }

    override fun getPetById(petId: kotlin.Long): Pet {
        TODO("Implement me")
    }

    override fun updatePet(body: Pet): Unit {
        TODO("Implement me")
    }

    override fun updatePetWithForm(petId: kotlin.Long, name: kotlin.String?, status: kotlin.String?): Unit {
        TODO("Implement me")
    }

    override fun uploadFile(petId: kotlin.Long, image: org.springframework.web.multipart.MultipartFile, additionalMetadata: kotlin.String?): ModelApiResponse {
        TODO("Implement me")
    }

    override fun uploadMultipleFile(petId: kotlin.Long, images: Array<org.springframework.web.multipart.MultipartFile>, additionalMetadata: kotlin.String?): ModelApiResponse {
        TODO("Implement me")
    }
}
