package org.openapitools.api

import org.springframework.data.domain.Pageable
import org.openapitools.model.Pet
import org.openapitools.model.PetSort
import org.openapitools.configuration.ValidSort
import org.junit.jupiter.api.Test
import org.springframework.http.ResponseEntity

class PetApiTest {

    private val service: PetApiService = PetApiServiceImpl()
    private val api: PetApiController = PetApiController(service)

    /**
     * To test PetApiController.findPetsAutoDetectedWithSort
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun findPetsAutoDetectedWithSortTest() {
        val status: kotlin.String? = TODO()
        val page: kotlin.Int = TODO()
        val size: kotlin.Int = TODO()
        val sort: kotlin.String? = TODO()
        
        
        val response: ResponseEntity<List<Pet>> = api.findPetsAutoDetectedWithSort(status, page, size, sort)

        // TODO: test validations
    }

    /**
     * To test PetApiController.findPetsNonPaginatedWithSortEnum
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun findPetsNonPaginatedWithSortEnumTest() {
        val sort: kotlin.String? = TODO()
        
        
        val response: ResponseEntity<List<Pet>> = api.findPetsNonPaginatedWithSortEnum(sort)

        // TODO: test validations
    }

    /**
     * To test PetApiController.findPetsWithRefSort
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun findPetsWithRefSortTest() {
        
        val pageable: Pageable = TODO()
        val response: ResponseEntity<List<Pet>> = api.findPetsWithRefSort(pageable)

        // TODO: test validations
    }

    /**
     * To test PetApiController.findPetsWithSortEnum
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun findPetsWithSortEnumTest() {
        val status: kotlin.String? = TODO()
        
        val pageable: Pageable = TODO()
        val response: ResponseEntity<List<Pet>> = api.findPetsWithSortEnum(status, pageable)

        // TODO: test validations
    }

    /**
     * To test PetApiController.findPetsWithoutSortEnum
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun findPetsWithoutSortEnumTest() {
        
        val pageable: Pageable = TODO()
        val response: ResponseEntity<List<Pet>> = api.findPetsWithoutSortEnum(pageable)

        // TODO: test validations
    }
}
