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

package com.wordnik.swagger.codegen

trait PathUtil {
  def getResourcePath(host: String) = {
    System.getProperty("fileMap") match {
      case s: String => s
      case _ => host
    }
  }

  def getBasePath(host: String, basePath: String) = {
    System.getProperty("fileMap") match {
      case s: String => {
        // return the parent folder
        val f = new java.io.File(s)
        f.getParent
      }
      case _ => {
        if(basePath != "") basePath
        else host
      }
    }
  }

  def toModelName(name: String) = {
    if(name.length > 0)
      name(0).toUpper + name.substring(1)
    else "MISSING MODEL NAME"
  }

  def toApiName(name: String) = {
    name.replaceAll("\\{","").replaceAll("\\}", "") match {
      case s: String if(s.length > 0) => s(0).toUpper + s.substring(1) + "Api"
      case _ => "Api"
    }
  }

  def nameFromPath(apiPath: String) = {
    apiPath.split("/")(1).split("\\.")(0).replaceAll("/", "")
  }

  def apiNameFromPath(apiPath: String) = toApiName(nameFromPath(apiPath))

  def resourceNameFromFullPath(apiPath: String) = {
    apiPath.split("/")(1).split("\\.")(0).replaceAll("/", "")
  }
}
