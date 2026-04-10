package org.openapitools.api

import org.springframework.data.domain.Pageable
import org.openapitools.model.Pet
import org.openapitools.model.PetSort
import org.openapitools.configuration.ValidSort

interface PetApiService {

    /**
     * GET /pet/findAutoDetectedWithSort : Find pets with auto-detected pagination and sort enum
     *
     * @param status Status filter (optional)
     * @param page  (optional, default to 0)
     * @param size  (optional, default to 20)
     * @param sort Sort order (optional)
     * @return successful operation (status code 200)
     * @see PetApi#findPetsAutoDetectedWithSort
     */
    fun findPetsAutoDetectedWithSort(status: kotlin.String?, page: kotlin.Int, size: kotlin.Int, sort: kotlin.String?): List<Pet>

    /**
     * GET /pet/findNonPaginatedWithSortEnum : Find pets without pagination but sort param has enum — no sort validation expected
     *
     * @param sort Sort order with enum but no pagination (optional)
     * @return successful operation (status code 200)
     * @see PetApi#findPetsNonPaginatedWithSortEnum
     */
    fun findPetsNonPaginatedWithSortEnum(sort: kotlin.String?): List<Pet>

    /**
     * GET /pet/findWithRefSort : Find pets with x-spring-paginated and $ref sort enum
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithRefSort
     */
    fun findPetsWithRefSort(): List<Pet>

    /**
     * GET /pet/findByStatusWithSort : Find pets with explicit x-spring-paginated and inline sort enum
     *
     * @param status Status filter (optional)
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithSortEnum
     */
    fun findPetsWithSortEnum(status: kotlin.String?): List<Pet>

    /**
     * GET /pet/findWithoutSortEnum : Find pets with pagination but sort has no enum constraint
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithoutSortEnum
     */
    fun findPetsWithoutSortEnum(): List<Pet>
}
