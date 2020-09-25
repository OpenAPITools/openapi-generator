@file:JvmName("MyclientcodegenGenerator")
package com.my.company.codegen

import org.openapitools.codegen.*

import java.util.*
import java.io.File

open class MyclientcodegenGenerator() : DefaultCodegen(), CodegenConfig {

    // source folder where files are written
    private var sourceFolder = "src"
    private var apiVersion = "1.0.0"

    /**
     * Configures the type of generator.
     *
     * @return  the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    override fun getTag(): CodegenType {
        return CodegenType.DOCUMENTATION
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    override fun getName(): String {
        return "myClientCodegen"
    }

    /**
     * Provides an opportunity to inspect and modify operation data before the code is generated.
     */
    @Suppress("UNCHECKED_CAST")
    override fun postProcessOperationsWithModels(objs: Map<String, Any>, allModels: List<Any>?): Map<String, Any> {
        val results = super.postProcessOperationsWithModels(objs, allModels)

        val ops = results["operations"] as Map<String, Any>
        val opList = ops["operation"] as ArrayList<CodegenOperation>

        // iterate over the operation and perhaps modify something
        for (co: CodegenOperation in opList) {
            // example:
            // co.httpMethod = co.httpMethod.toLowerCase();
        }

        return results
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    override fun getHelp(): String {
        return "Generates a myClientCodegen client library."
    }

    init {
        // set the output folder here
        outputFolder = "generated-code/myClientCodegen"

        /**
         * Models.  You can write model files using the modelTemplateFiles map.
         * if you want to create one template for file, you can do so here.
         * for multiple files for model, just put another entry in the `modelTemplateFiles` with
         * a different extension
         */
        modelTemplateFiles["model.mustache"] = ".sample"       // the extension for each file to write

        /**
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        apiTemplateFiles["api.mustache"] = ".sample"       // the extension for each file to write

        /**
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        templateDir = "myClientCodegen"

        /**
         * Api Package.  Optional, if needed, this can be used in templates
         */
        apiPackage = "org.openapitools.api"

        /**
         * Model Package.  Optional, if needed, this can be used in templates
         */
        modelPackage = "org.openapitools.model"

        /**
         * Reserved words.  Override this with reserved words specific to your language
         */
        reservedWords = HashSet(
            listOf("sample1", "sample2")
        )

        /**
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties["apiVersion"] = apiVersion

        /**
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(
            SupportingFile(
                "myFile.mustache", // the input template or file
                "", // the destination folder, relative `outputFolder`
                "myFile.sample"
            )                                          // the output file
        )

        /**
         * Language Specific Primitives.  These types will not trigger imports by
         * the client generator
         */
        languageSpecificPrimitives = HashSet(
            listOf("Type1", "Type2")
        )
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reserved words
     *
     * @return the escaped term
     */
    override fun escapeReservedWord(name: String?): String {
        // add an underscore to the name if it exists.
        return if (name == null) "" else "_$name"
    }

    /**
     * Location to write model files.  You can use the modelPackage() as defined when the class is
     * instantiated
     */
    override fun modelFileFolder(): String {
        return """$outputFolder/$sourceFolder/${modelPackage().replace('.', File.separatorChar)}"""
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    override fun apiFileFolder(): String {
        return """$outputFolder/$sourceFolder/${apiPackage().replace('.', File.separatorChar)}"""
    }

    /**
     * override with any special text escaping logic to handle unsafe
     * characters so as to avoid code injection
     *
     * @param input String to be cleaned up
     * @return string with unsafe characters removed or escaped
     */
    override fun escapeUnsafeCharacters(input: String): String {
        //TODO: check that this logic is safe to escape unsafe characters to avoid code injection
        return input
    }

    /**
     * Escape single and/or double quote to avoid code injection
     *
     * @param input String to be cleaned up
     * @return string with quotation mark removed or escaped
     */
    override fun escapeQuotationMark(input: String): String {
        //TODO: check that this logic is safe to escape quotation mark to avoid code injection
        return with(input) { replace("\"", "\\\"") }
    }
}