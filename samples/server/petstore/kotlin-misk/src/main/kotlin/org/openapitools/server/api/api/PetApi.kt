package org.openapitools.server.api.api

import org.openapitools.server.api.model.ModelApiResponse
import org.openapitools.server.api.model.Pet
import okhttp3.Headers

interface PetApi {

    fun addPet(pet: Pet) : Pet

    fun deletePet(petId: kotlin.Long, headers: Headers) 

    fun findPetsByStatus(status: kotlin.Array<kotlin.String>) : kotlin.Array<Pet>

    fun findPetsByTags(tags: kotlin.Array<kotlin.String>) : kotlin.Array<Pet>

    fun getPetById(petId: kotlin.Long) : Pet

    fun updatePet(pet: Pet) : Pet

    fun updatePetWithForm(petId: kotlin.Long,  name: kotlin.String?,  status: kotlin.String?) 

    fun uploadFile(petId: kotlin.Long,  additionalMetadata: kotlin.String?, ) : ModelApiResponse
}
