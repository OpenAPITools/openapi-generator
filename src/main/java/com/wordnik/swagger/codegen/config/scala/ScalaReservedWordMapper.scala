/**
 *  Copyright 2011 Wordnik, Inc.
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

package com.wordnik.swagger.codegen.config.scala

import com.wordnik.swagger.codegen.config.ReservedWordMapper

object ScalaKeywordMapper {
  val reservedWords = Array("type", "case", "object","match")
}

class ScalaReservedWordMapper extends ReservedWordMapper {
  override def translate(input: String): String = {
    ScalaKeywordMapper.reservedWords.contains(input) match {
      case true => "`" + input + "`"
      case false => input
    }
  }

  override def retranslate(input: String): String = {
    (input.startsWith("`") && input.endsWith("`")) match {
      case true =>  input.substring(1, input.length()-1)
      case false => input
    }
  }

}
