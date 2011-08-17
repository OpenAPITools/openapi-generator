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

package com.wordnik.swagger.codegen.config;

import com.wordnik.swagger.codegen.exception.CodeGenerationException;

/**
 * User: deepakmichael
 * Date: 23/07/11
 * Time: 8:01 AM
 */
public class LanguageConfiguration {

    private String classFileExtension;

    private String templateLocation;

    private String structureLocation;

    private String libraryHome;

    private String modelClassLocation;

    private String resourceClassLocation;

    private String exceptionPackageName;

    private String annotationPackageName;

    private boolean generateHelperEnums = true;

    private boolean generateOutputWrappers = false;

    public String getClassFileExtension() {
        return classFileExtension;
    }

    public void setClassFileExtension(String classFileExtension) {
        this.classFileExtension = classFileExtension;
    }

    public String getTemplateLocation() {
        return templateLocation;
    }

    public void setTemplateLocation(String templateLocation) {
        this.templateLocation = templateLocation;
    }

    public void setOutputDirectory(String outputDirectory) {

        if(outputDirectory == null || outputDirectory.length() == 0){
            throw new CodeGenerationException("Error creating output path : Output path was null ");
        }
        outputDirectory = outputDirectory.endsWith("/") ? outputDirectory.substring(0, outputDirectory.lastIndexOf("/")) : outputDirectory;


        this.modelClassLocation = outputDirectory + "/model/";
        this.resourceClassLocation = outputDirectory + "/api/";
    }

    public String getModelClassLocation() {
        return modelClassLocation;
    }

    public String getResourceClassLocation() {
        return resourceClassLocation;
    }

    public String getExceptionPackageName() {
        return exceptionPackageName;
    }

    public void setExceptionPackageName(String exceptionPackageName) {
        this.exceptionPackageName = exceptionPackageName;
    }

    public String getAnnotationPackageName() {
        return annotationPackageName;
    }

    public void setAnnotationPackageName(String annotationPackageName) {
        this.annotationPackageName = annotationPackageName;
    }


    public String getStructureLocation() {
        return structureLocation;
    }

    public void setStructureLocation(String structureLocation) {
        this.structureLocation = structureLocation;
    }

    public String getLibraryHome() {
        return libraryHome;
    }

    public void setLibraryHome(String libraryHome) {
        this.libraryHome = libraryHome;
    }

    public void setGenerateHelperEnums(boolean generateHelperEnums) {
        this.generateHelperEnums = generateHelperEnums;
    }

    public boolean isGenerateHelperEnums() {
        return generateHelperEnums;
    }

    public void setGenerateOutputWrappers(boolean generateOutputWrappers) {
        this.generateOutputWrappers = generateOutputWrappers;
    }

    public boolean isGenerateOutputWrappers() {
        return generateOutputWrappers;
    }
}
