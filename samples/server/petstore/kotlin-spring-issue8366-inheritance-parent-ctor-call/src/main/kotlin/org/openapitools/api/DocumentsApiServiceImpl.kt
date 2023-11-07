package org.openapitools.api

import org.openapitools.model.ParentSchema
import kotlinx.coroutines.flow.Flow
import org.springframework.stereotype.Service
@Service
class DocumentsApiServiceImpl : DocumentsApiService {

    override fun documentsV1Get(): Flow<ParentSchema> {
        TODO("Implement me")
    }
}
