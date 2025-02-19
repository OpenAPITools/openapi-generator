/*
 * Copyright 2020 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
assert readme.text.contains("# kotlin-client")
assert readme.text.contains("OpenAPI Petstore")

File gradle = new File(basedir, "out/build.gradle")
assert gradle.isFile()

File api = new File(basedir, "out/src/main/kotlin/org/openapitools/client/apis/PetApi.kt")
assert api.isFile()

File model = new File(basedir, "out/src/main/kotlin/org/openapitools/client/models/Pet.kt")
assert model.isFile()

// note that in Java 11+, this anything matching this condition could fail due to
// Illegal reflective access by org.codehaus.groovy.reflection.CachedClass
// and cause tests to fail. This is more to document for engineers.
if (GroovySystem.version.tokenize('.')[0].toInteger() < 3) {
    throw new IllegalStateException("Found:" + GroovySystem.version + ", need Groovy 3.x or higher for Java 11+, so we require it for all versions")
}
