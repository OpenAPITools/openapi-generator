/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

public enum GeneratorLanguage {
    /*
    Not defined because they use the default Java language:
    Android

    Note: all documentation generators have generatorLanguage set to null
     */
    JAVA("Java"), ADA("Ada"), APEX("Apex"), BASH("Bash"), C("C"),
    CLOJURE("Clojure"), C_PLUS_PLUS("C++"), CRYSTAL("Crystal"), C_SHARP("C#"),
    DART("Dart"), EIFFEL("Eiffel"), ELIXIR("Elixir"), ELM("Elm"),
    ERLANG("Erlang"), FLASH("Flash"), F_SHARP("F#"), GO("Go"),
    JAVASCRIPT("Javascript"), GRAPH_QL("GraphQL"), GROOVY("Groovy"),
    HASKELL("Haskell"), TYPESCRIPT("Typescript"), K_SIX("k6"), KOTLIN("Kotlin"),
    KTORM("Ktorm"), LUA("Lua"), MYSQL("Mysql"), NIM("Nim"),
    OBJECTIVE_C("Objective-C"), OCAML("OCaml"), PERL("Perl"), PHP("PHP"),
    POWERSHELL("PowerShell"), PROTOBUF("Protocol Buffers (Protobuf)"), PYTHON("Python"),
    R("R"), RUBY("Ruby"), RUST("Rust"), SCALA("Scala"), SWIFT("Swift"),
    WSDL("Web Services Description Language (WSDL)");

    private final String label;

    private GeneratorLanguage(String label) {
        this.label = label;
    }

    @Override
    public String toString() {
        return this.label;
    }
}
