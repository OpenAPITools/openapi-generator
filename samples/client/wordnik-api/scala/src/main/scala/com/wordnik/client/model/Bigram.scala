package com.wordnik.client.model

case class Bigram (
  count: Long,
  gram2: String,
  gram1: String,
  wlmi: Double,
  mi: Double)

