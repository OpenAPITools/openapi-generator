package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Service


@Service
class PetApiServiceImpl : PetApiService {

    override fun addPet(body: Pet): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun deletePet(petId: kotlin.Long, apiKey: kotlin.String?): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): ResponseEntity<List<Pet>> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): ResponseEntity<List<Pet>> {
        TODO("Implement me")
    }

    override fun getPetById(petId: kotlin.Long): ResponseEntity<Pet> {
        TODO("Implement me")
    }

    override fun updatePet(body: Pet): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun updatePetWithForm(petId: kotlin.Long, name: kotlin.String?, status: kotlin.String?): ResponseEntity<Unit> {
        TODO("Implement me")
    }

    override fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: org.springframework.core.io.Resource?): ResponseEntity<ModelApiResponse> {
        TODO("Implement me")
    }
}
