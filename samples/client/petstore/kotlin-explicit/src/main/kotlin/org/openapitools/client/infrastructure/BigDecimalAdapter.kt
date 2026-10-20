package org.openapitools.client.infrastructure

import com.squareup.moshi.FromJson
import com.squareup.moshi.ToJson
import java.math.BigDecimal

public class BigDecimalAdapter {
    @ToJson
    public fun toJson(value: BigDecimal): String {
        return value.toPlainString()
    }

    @FromJson
    public fun fromJson(value: String): BigDecimal {
        return BigDecimal(value)
    }
}
