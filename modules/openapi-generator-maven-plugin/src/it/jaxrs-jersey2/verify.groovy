/*
 * Copyright 2022 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

File readme = new File(basedir, "out/README.md")

assert readme.isFile()
assert readme.text.contains("# TEST TEST TEST")
assert readme.text.contains("# openapi-jaxrs-server")
assert readme.text.contains("OpenAPI Petstore")

File mavenPomXml = new File(basedir, "out/pom.xml")
assert mavenPomXml.isFile()

File petApi = new File(basedir, "out/src/gen/java/org/openapitools/api/PetApi.java")
assert petApi.isFile()
assert petApi.text.contains("import org.openapitools.api.PetApiService;")

File petApiService = new File(basedir, "out/src/gen/java/org/openapitools/api/PetApiService.java")
assert petApiService.isFile()
assert petApiService.text.contains("import javax.ws.rs.core.Response;")

File petModel = new File(basedir, "out/src/gen/java/org/openapitools/model/Pet.java")
assert petModel.isFile()
assert petModel.text.contains("public class Pet")
