package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call
import okhttp3.RequestBody
import com.fasterxml.jackson.annotation.JsonProperty

import org.openapitools.client.models.Animal
import org.openapitools.client.models.Beverage
import org.openapitools.client.models.Fruit

interface DefaultApi {
    /**
     * POST animals
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @param animal 
     * @return [Call]<[Animal]>
     */
    @POST("animals")
    fun createAnimal(@Body animal: Animal): Call<Animal>

    /**
     * POST beverages
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @param beverage 
     * @return [Call]<[Beverage]>
     */
    @POST("beverages")
    fun createBeverage(@Body beverage: Beverage): Call<Beverage>

    /**
     * POST fruits
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @param fruit 
     * @return [Call]<[Fruit]>
     */
    @POST("fruits")
    fun createFruit(@Body fruit: Fruit): Call<Fruit>

}
