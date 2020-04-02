package org.openapitools.client.infrastructure

import okhttp3.OkHttpClient
import retrofit2.Retrofit
import retrofit2.converter.scalars.ScalarsConverterFactory
import retrofit2.adapter.rxjava.RxJavaCallAdapterFactory
import retrofit2.converter.gson.GsonConverterFactory

class ApiClient(
    private var baseUrl: String = defaultBasePath,
    private var okHttpClient: OkHttpClient
) {
    companion object {
        @JvmStatic
        val defaultBasePath: String by lazy {
            System.getProperties().getProperty("org.openapitools.client.baseUrl", "http://petstore.swagger.io:80/v2")
        }
    }

    init {
        normalizeBaseUrl()
    }

    val retrofitBuilder: Retrofit.Builder by lazy {

        Retrofit.Builder()
        .baseUrl(baseUrl)
        .addConverterFactory(ScalarsConverterFactory.create())
        .addConverterFactory(GsonConverterFactory.create(Serializer.gson))
        .addCallAdapterFactory(RxJavaCallAdapterFactory.create())
    }

    fun <S> createService(serviceClass: Class<S>): S {
        return retrofitBuilder.client(okHttpClient).build().create(serviceClass)
    }

    private fun normalizeBaseUrl() {
        if (!baseUrl.endsWith("/")) {
            baseUrl += "/"
        }
    }
}