package org.openapitools.lib.datatypes

import okhttp3.Response

class ReturnObject<out T>(
        response: Response,
        val data: T
): AbstractReturnObject(response)
