/**
 *  Copyright 2012 Wordnik, Inc.
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

import com.wordnik.swagger.codegen.BasicRubyGenerator

object RubyPetstoreCodegen extends BasicRubyGenerator {
  def main(args: Array[String]) = generateClient(args)
  
  // to avoid recompiling ...
  override def templateDir = "src/main/resources/ruby"

  // where to write generated code
  override def destinationDir = "samples/client/petstore/ruby"
}