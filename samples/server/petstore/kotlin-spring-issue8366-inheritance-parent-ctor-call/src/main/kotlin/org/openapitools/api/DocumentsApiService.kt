package org.openapitools.api

import org.openapitools.model.ParentSchema
import kotlinx.coroutines.flow.Flow

interface DocumentsApiService {

    /**
     * GET /documents/v1
     *
     * @return lorem (status code 200)
     * @see DocumentsApi#documentsV1Get
     */
    fun documentsV1Get(): Flow<ParentSchema>
}
