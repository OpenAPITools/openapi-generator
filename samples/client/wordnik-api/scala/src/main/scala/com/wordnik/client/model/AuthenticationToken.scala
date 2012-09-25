package com.wordnik.client.model

case class AuthenticationToken (
  token: String,
  userId: Long,
  userSignature: String)

