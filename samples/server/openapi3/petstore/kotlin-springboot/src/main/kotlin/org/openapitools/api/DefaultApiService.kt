package org.openapitools.api

import org.openapitools.model.SysMailFormData

interface DefaultApiService {

    fun updateSysMailAtKey(key: String,sysMailFormData: SysMailFormData): Unit
}
