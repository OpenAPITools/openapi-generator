package com.wordnik.client.model

case class ApiTokenStatus (
  valid: Boolean,
  token: String,
  resetsInMillis: Long,
  remainingCalls: Long,
  expiresInMillis: Long,
  totalRequests: Long)

