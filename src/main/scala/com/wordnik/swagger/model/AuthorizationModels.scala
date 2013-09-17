/**
 *  Copyright 2013 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.model

trait AuthorizationType {
  def `type`: String
}
case class OAuth(
  scopes: List[String], 
  grantTypes: List[GrantType]) extends AuthorizationType {
  override def `type` = "oauth2"
}
case class ApiKey(keyname: String, passAs: String = "header") extends AuthorizationType {
  override def `type` = "apiKey"
}

trait GrantType {
  def `type`: String
}
case class ImplicitGrant(
  loginEndpoint: LoginEndpoint, 
  tokenName: String) extends GrantType {
  def `type` = "implicit"
}
case class AuthorizationCodeGrant(
  tokenRequestEndpoint: TokenRequestEndpoint,
  tokenEndpoint: TokenEndpoint) extends GrantType {
  def `type` = "authorization_code"
}

trait AuthorizationValue
case class ApiKeyValue(keyName: String, passAs: String, value: String) extends AuthorizationValue
