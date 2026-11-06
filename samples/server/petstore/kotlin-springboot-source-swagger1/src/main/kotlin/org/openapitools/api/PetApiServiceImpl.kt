package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import javax.validation.Valid
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import org.springframework.stereotype.Service
@Service
class PetApiServiceImpl : PetApiService {

    override fun addPet(body: Pet): Unit {
        TODO("Implement me")
    }

    override fun deletePet(petId: kotlin.Long): Unit {
        TODO("Implement me")
    }

    override fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>): List<@Valid Pet> {
        TODO("Implement me")
    }

    override fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>): List<@Valid Pet> {
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

    override fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: org.springframework.web.multipart.MultipartFile): ModelApiResponse {
        TODO("Implement me")
    }
}
