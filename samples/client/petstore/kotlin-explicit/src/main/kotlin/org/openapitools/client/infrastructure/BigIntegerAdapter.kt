package org.openapitools.client.infrastructure

import com.squareup.moshi.FromJson
import com.squareup.moshi.ToJson
import java.math.BigInteger

public class BigIntegerAdapter {
    @ToJson
    public fun toJson(value: BigInteger): String {
        return value.toString()
    }

    @FromJson
    public fun fromJson(value: String): BigInteger {
        return BigInteger(value)
    }
}
