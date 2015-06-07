package com.wordnik.client.model


case class Pet(
                id: Long,
                category: Category,
                name: String,
                photoUrls: List[String],
                tags: List[Tag],
                status: String
                )
