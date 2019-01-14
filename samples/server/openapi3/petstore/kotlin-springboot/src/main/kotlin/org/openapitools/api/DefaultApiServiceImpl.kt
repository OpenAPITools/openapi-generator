package org.openapitools.api

import org.openapitools.model.SysMailFormData
import org.springframework.stereotype.Service

@Service
class DefaultApiServiceImpl : DefaultApiService {

    override fun updateSysMailAtKey(key: String,sysMailFormData: SysMailFormData): Unit {
        TODO("Implement me")
    }
}
