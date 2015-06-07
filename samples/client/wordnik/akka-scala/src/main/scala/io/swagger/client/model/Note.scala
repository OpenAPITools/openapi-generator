package io.swagger.client.model


case class Note(
                 noteType: Option[String],
                 appliesTo: Option[Seq[String]],
                 value: Option[String],
                 pos: Option[Int])
  extends ApiModel


