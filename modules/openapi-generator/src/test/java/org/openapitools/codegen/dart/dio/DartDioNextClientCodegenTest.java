/*
 * Copyright 2021 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.dart.dio;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.DartDioClientCodegen;
import org.openapitools.codegen.languages.DartDioNextClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class DartDioNextClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final DartDioNextClientCodegen codegen = new DartDioNextClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final DartDioClientCodegen codegen = new DartDioClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final DartDioClientCodegen codegen = new DartDioClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testKeywords() throws Exception {
        final DartDioClientCodegen codegen = new DartDioClientCodegen();

        List<String> reservedWordsList = new ArrayList<String>();
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("src/main/resources/dart/dart-keywords.txt"), StandardCharsets.UTF_8));
            while(reader.ready()) { reservedWordsList.add(reader.readLine()); }
            reader.close();
        } catch (Exception e) {
            String errorString = String.format(Locale.ROOT, "Error reading dart keywords: %s", e);
            Assert.fail(errorString, e);
        }

        Assert.assertTrue(reservedWordsList.size() > 20);
        Assert.assertEquals(codegen.reservedWords().size(), reservedWordsList.size());
        for(String keyword : reservedWordsList) {
            // reserved words are stored in lowercase
            Assert.assertTrue(codegen.reservedWords().contains(keyword.toLowerCase(Locale.ROOT)), String.format(Locale.ROOT, "%s, part of %s, was not found in %s", keyword, reservedWordsList, codegen.reservedWords().toString()));
        }
    }


    @Test
    public void testInitialDioLibraryValues() throws Exception {
        final DartDioNextClientCodegen codegen = new DartDioNextClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(DartDioNextClientCodegen.DIO_LIBRARY), DartDioNextClientCodegen.DIO_LIBRARY_DEFAULT);
        Assert.assertEquals(codegen.getDioLibrary(), DartDioNextClientCodegen.DIO_LIBRARY_DEFAULT);
    }

    @Test
    public void testAdditionalPropertiesPutForDioLibraryValues() throws Exception {
        final DartDioNextClientCodegen codegen = new DartDioNextClientCodegen();
        codegen.additionalProperties().put(DartDioNextClientCodegen.DIO_LIBRARY, DartDioNextClientCodegen.DIO_HTTP);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(DartDioNextClientCodegen.DIO_LIBRARY), DartDioNextClientCodegen.DIO_HTTP);
        Assert.assertEquals(codegen.getDioLibrary(), DartDioNextClientCodegen.DIO_HTTP);
    }

}
