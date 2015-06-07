package io.swagger.client.model


case class Related(
                    label1: Option[String],
                    relationshipType: Option[String],
                    label2: Option[String],
                    label3: Option[String],
                    words: Option[Seq[String]],
                    gram: Option[String],
                    label4: Option[String])
  extends ApiModel


