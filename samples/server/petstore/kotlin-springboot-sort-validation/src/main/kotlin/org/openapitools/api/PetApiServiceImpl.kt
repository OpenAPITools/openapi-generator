package org.openapitools.api

import org.springframework.data.domain.Pageable
import org.springframework.data.web.PageableDefault
import org.openapitools.model.Pet
import org.openapitools.model.PetSort
import org.openapitools.model.PetSortEnum
import org.springframework.data.domain.Sort
import org.springframework.data.web.SortDefault
import org.openapitools.configuration.ValidPageable
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

    override fun findPetsWithAllDefaults(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithArraySortEnum(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithArraySortRefEnum(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithExternalParamRefArraySort(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithMixedSortDefaults(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithNonExplodedExternalParamRefArraySort(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithPageAndSizeConstraint(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithPageSizeDefaultsOnly(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithRefSort(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithSizeConstraint(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithSortDefaultAsc(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithSortDefaultOnly(): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithSortEnum(status: kotlin.String?): List<Pet> {
        TODO("Implement me")
    }

    override fun findPetsWithoutSortEnum(): List<Pet> {
        TODO("Implement me")
    }
}
