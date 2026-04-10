package org.openapitools.api

import org.springframework.data.domain.Pageable
import org.openapitools.model.Pet
import org.openapitools.model.PetSort
import org.openapitools.configuration.ValidSort
import org.springframework.stereotype.Service
@Service
class PetApiServiceImpl : PetApiService {

    override fun findPetsAutoDetectedWithSort(status: kotlin.String?, page: kotlin.Int, size: kotlin.Int, sort: kotlin.String?): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsNonPaginatedWithSortEnum(sort: kotlin.String?): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithRefSort(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithSortEnum(status: kotlin.String?): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithoutSortEnum(): List<Pet> {
        TODO("Implement me")
    }
}
