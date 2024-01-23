package org.openapitools.client.infrastructure

import com.squareup.moshi.Moshi
import com.squareup.moshi.adapters.EnumJsonAdapter

object SerializerHelper {
    fun addEnumUnknownDefaultCase(moshiBuilder: Moshi.Builder): Moshi.Builder {
        return moshiBuilder
            .add(org.openapitools.client.models.ModelWithEnumPropertyHavingDefault.PropertyName::class.java, EnumJsonAdapter.create(org.openapitools.client.models.ModelWithEnumPropertyHavingDefault.PropertyName::class.java)
                .withUnknownFallback(org.openapitools.client.models.ModelWithEnumPropertyHavingDefault.PropertyName.unknownDefaultOpenApi))
            .add(org.openapitools.client.models.PropertyOfDay.DaysOfWeek::class.java, EnumJsonAdapter.create(org.openapitools.client.models.PropertyOfDay.DaysOfWeek::class.java)
                .withUnknownFallback(org.openapitools.client.models.PropertyOfDay.DaysOfWeek.unknownDefaultOpenApi))
            .add(org.openapitools.client.models.PropertyOfDay.MonthOfYear::class.java, EnumJsonAdapter.create(org.openapitools.client.models.PropertyOfDay.MonthOfYear::class.java)
                .withUnknownFallback(org.openapitools.client.models.PropertyOfDay.MonthOfYear.unknownDefaultOpenApi))
            .add(org.openapitools.client.models.PropertyOfDay.HolidayTypes::class.java, EnumJsonAdapter.create(org.openapitools.client.models.PropertyOfDay.HolidayTypes::class.java)
                .withUnknownFallback(org.openapitools.client.models.PropertyOfDay.HolidayTypes.unknownDefaultOpenApi))
    }
}
