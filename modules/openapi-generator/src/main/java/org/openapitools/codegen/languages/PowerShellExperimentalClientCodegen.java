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

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static java.util.UUID.randomUUID;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class PowerShellExperimentalClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(PowerShellExperimentalClientCodegen.class);

    private String packageGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";

    protected String sourceFolder = "src";
    protected String packageName = "PSOpenAPITools";
    protected String packageVersion = "0.1.2";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String apiTestPath = "tests/Api";
    protected String modelTestPath = "tests/Model";
    protected HashSet nullablePrimitives;
    protected String powershellGalleryUrl;
    protected HashSet powershellVerbs;
    protected Map<String, String> commonVerbs; // verbs not in the official ps verb list but can be mapped to one of the verbs
    protected HashSet methodNames; // store a list of method names to detect duplicates

    /**
     * Constructs an instance of `PowerShellExperimentalClientCodegen`.
     */
    public PowerShellExperimentalClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "powershell-expiermental";
        modelTemplateFiles.put("model.mustache", ".ps1");
        apiTemplateFiles.put("api.mustache", ".ps1");
        modelTestTemplateFiles.put("model_test.mustache", ".ps1");
        apiTestTemplateFiles.put("api_test.mustache", ".ps1");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "powershell-experimental";
        apiPackage = packageName + File.separator + "Api";
        modelPackage = packageName + File.separator + "Model";

        // https://blogs.msdn.microsoft.com/powershell/2010/01/07/how-objects-are-sent-to-and-from-remote-sessions/
        languageSpecificPrimitives = new HashSet<String>(Arrays.asList(
                "Byte",
                "SByte",
                "Byte[]",
                "Int16",
                "Int32",
                "Int64",
                "UInt16",
                "UInt32",
                "UInt64",
                "Decimal",
                "Single",
                "Double",
                "TimeSpan",
                "System.DateTime",
                "ProgressRecord",
                "Char",
                "String",
                "XmlDocument",
                "SecureString",
                "Boolean",
                "Guid",
                "Uri",
                "System.IO.FileInfo",
                "Version"
        ));

        commonVerbs = new HashMap<String, String>();

        Map<String, List<String>> verbMappings = new HashMap<String, List<String>>();

        // common
        verbMappings.put("Add", Arrays.asList("Append", "Attach", "Concatenate", "Insert"));
        verbMappings.put("Clear", Arrays.asList("Flush", "Erase", "Release", "Unmark", "Unset", "Nullify"));
        verbMappings.put("Close", Arrays.asList());
        verbMappings.put("Copy", Arrays.asList("Duplicate", "Clone", "Replicate", "Sync"));
        verbMappings.put("Enter", Arrays.asList("PushInto"));
        verbMappings.put("Exit", Arrays.asList("PopOut"));
        verbMappings.put("Find", Arrays.asList());
        verbMappings.put("Format", Arrays.asList());
        verbMappings.put("Get", Arrays.asList("Read", "Open", "Cat", "Type", "Dir", "Obtain", "Dump", "Acquire", "Examine", "Find", "Search"));
        verbMappings.put("Hide", Arrays.asList("Block"));
        verbMappings.put("Join", Arrays.asList("Combine", "Unite", "Connect", "Associate"));
        verbMappings.put("Lock", Arrays.asList("RestrictSecure"));
        verbMappings.put("Move", Arrays.asList("Transfer", "Name", "Migrate"));
        verbMappings.put("New", Arrays.asList("Create", "Generate", "Build", "Make", "Allocate"));
        verbMappings.put("Open", Arrays.asList());
        verbMappings.put("Optimize", Arrays.asList());
        verbMappings.put("Pop", Arrays.asList());
        verbMappings.put("Push", Arrays.asList());
        verbMappings.put("Redo", Arrays.asList());
        verbMappings.put("Remove", Arrays.asList("Clear", "Cut", "Dispose", "Discard", "Erase"));
        verbMappings.put("Rename", Arrays.asList("Change"));
        verbMappings.put("Reset", Arrays.asList());
        verbMappings.put("Search", Arrays.asList("FindLocate"));
        verbMappings.put("Select", Arrays.asList("FindLocate"));
        verbMappings.put("Set", Arrays.asList("Write", "Reset", "Assign", "Configure"));
        verbMappings.put("Show", Arrays.asList("DisplayProduce"));
        verbMappings.put("Skip", Arrays.asList("BypassJump"));
        verbMappings.put("Split", Arrays.asList("parate"));
        verbMappings.put("Step", Arrays.asList());
        verbMappings.put("Switch", Arrays.asList());
        verbMappings.put("Undo", Arrays.asList());
        verbMappings.put("Unlock", Arrays.asList("Release", "Unrestrict", "Unsecure"));
        verbMappings.put("Watch", Arrays.asList());

        // communication
        verbMappings.put("Connect", Arrays.asList("JoinTelnet"));
        verbMappings.put("Disconnect", Arrays.asList("BreakLogoff"));
        verbMappings.put("Read", Arrays.asList("Acquire", "Prompt", "Get"));
        verbMappings.put("Receive", Arrays.asList("Read", "Accept", "Peek"));
        verbMappings.put("Send", Arrays.asList("Put", "Broadcast", "Mail", "Fax"));
        verbMappings.put("Write", Arrays.asList("PutPrint"));

        // data
        verbMappings.put("Backup", Arrays.asList(" Save", " Burn", " Replicate", "Sync"));
        verbMappings.put("Checkpoint", Arrays.asList("  Diff"));
        verbMappings.put("Compare", Arrays.asList("  Diff"));
        verbMappings.put("Compress", Arrays.asList("  Compact"));
        verbMappings.put("Convert", Arrays.asList(" Change", " Resize", "Resample"));
        verbMappings.put("ConvertFrom", Arrays.asList(" Export", " Output", "Out"));
        verbMappings.put("ConvertTo", Arrays.asList(" Import", " Input", "In"));
        verbMappings.put("Dismount", Arrays.asList(" UnmountUnlink"));
        verbMappings.put("Edit", Arrays.asList(" Change", " Update", "Modify"));
        verbMappings.put("Expand", Arrays.asList(" ExplodeUncompress"));
        verbMappings.put("Export", Arrays.asList(" ExtractBackup"));
        verbMappings.put("Group", Arrays.asList(" Aggregate", " Arrange", " Associate", "Correlate"));
        verbMappings.put("Import", Arrays.asList(" BulkLoadLoad"));
        verbMappings.put("Initialize", Arrays.asList(" Erase", " Init", " Renew", " Rebuild", " Reinitialize", "Setup"));
        verbMappings.put("Limit", Arrays.asList("  Quota"));
        verbMappings.put("Merge", Arrays.asList(" CombineJoin"));
        verbMappings.put("Mount", Arrays.asList(" Connect"));
        verbMappings.put("Out", Arrays.asList());
        verbMappings.put("Publish", Arrays.asList(" Deploy", " Release", "Install"));
        verbMappings.put("Restore", Arrays.asList(" Repair", " Return", " Undo", "Fix"));
        verbMappings.put("Save", Arrays.asList());
        verbMappings.put("Sync", Arrays.asList(" Replicate", " Coerce", "Match"));
        verbMappings.put("Unpublish", Arrays.asList(" Uninstall", " Revert", "Hide"));
        verbMappings.put("Update", Arrays.asList(" Refresh", " Renew", " Recalculate", "Re-index"));

        // diagnostic
        verbMappings.put("Debug", Arrays.asList("Diagnose"));
        verbMappings.put("Measure", Arrays.asList("Calculate", "Determine", "Analyze"));
        verbMappings.put("Ping", Arrays.asList());
        verbMappings.put("Repair", Arrays.asList("FixRestore"));
        verbMappings.put("Resolve", Arrays.asList("ExpandDetermine"));
        verbMappings.put("Test", Arrays.asList("Diagnose", "Analyze", "Salvage", "Verify"));
        verbMappings.put("Trace", Arrays.asList("Track", "Follow", "Inspect", "Dig"));

        // lifecycle
        verbMappings.put("Approve", Arrays.asList());
        verbMappings.put("Assert", Arrays.asList("Certify"));
        verbMappings.put("Build", Arrays.asList());
        verbMappings.put("Complete", Arrays.asList());
        verbMappings.put("Confirm", Arrays.asList("Acknowledge", "Agree", "Certify", "Validate", "Verify"));
        verbMappings.put("Deny", Arrays.asList("Block", "Object", "Refuse", "Reject"));
        verbMappings.put("Deploy", Arrays.asList());
        verbMappings.put("Disable", Arrays.asList("HaltHide"));
        verbMappings.put("Enable", Arrays.asList("StartBegin"));
        verbMappings.put("Install", Arrays.asList("Setup"));
        verbMappings.put("Invoke", Arrays.asList("RunStart"));
        verbMappings.put("Register", Arrays.asList());
        verbMappings.put("Request", Arrays.asList());
        verbMappings.put("Restart", Arrays.asList("Recycle"));
        verbMappings.put("Resume", Arrays.asList());
        verbMappings.put("Start", Arrays.asList("Launch", "Initiate", "Boot"));
        verbMappings.put("Stop", Arrays.asList("End", "Kill", "Terminate", "Cancel"));
        verbMappings.put("Submit", Arrays.asList("Post"));
        verbMappings.put("Suspend", Arrays.asList("Pause"));
        verbMappings.put("Uninstall", Arrays.asList());
        verbMappings.put("Unregister", Arrays.asList("Remove"));
        verbMappings.put("Wait", Arrays.asList("SleepPause"));

        // security
        verbMappings.put("Block", Arrays.asList("Prevent", "Limit", "Deny"));
        verbMappings.put("Grant", Arrays.asList("AllowEnable"));
        verbMappings.put("Protect", Arrays.asList("Encrypt", "Safeguard", "Seal"));
        verbMappings.put("Revoke", Arrays.asList("RemoveDisable"));
        verbMappings.put("Unblock", Arrays.asList("ClearAllow"));
        verbMappings.put("Unprotect", Arrays.asList("DecryptUnseal"));

        // other
        verbMappings.put("Use", Arrays.asList());

        for (Map.Entry<String, List<String>> entry : verbMappings.entrySet()) {
            // loop through each verb in the list
            for (String verb : entry.getValue()) {
                if (verbMappings.containsKey(verb)) {
                    // the verb to be mapped is also a common verb, do nothing
                    LOGGER.debug("verbmapping: skipped {}", verb);
                } else {
                    commonVerbs.put(verb, entry.getKey());
                    LOGGER.debug("verbmapping: adding {} => {}", verb, entry.getKey());
                }
            }
        }

        powershellVerbs = new HashSet<String>(Arrays.asList(
                "Add",
                "Clear",
                "Close",
                "Copy",
                "Enter",
                "Exit",
                "Find",
                "Format",
                "Get",
                "Hide",
                "Join",
                "Lock",
                "Move",
                "New",
                "Open",
                "Optimize",
                "Pop",
                "Push",
                "Redo",
                "Remove",
                "Rename",
                "Reset",
                "Search",
                "Select",
                "Set",
                "Show",
                "Skip",
                "Split",
                "Step",
                "Switch",
                "Undo",
                "Unlock",
                "Watch",
                "Connect",
                "Disconnect",
                "Read",
                "Receive",
                "Send",
                "Write",
                "Backup",
                "Checkpoint",
                "Compare",
                "Compress",
                "Convert",
                "ConvertFrom",
                "ConvertTo",
                "Dismount",
                "Edit",
                "Expand",
                "Export",
                "Group",
                "Import",
                "Initialize",
                "Limit",
                "Merge",
                "Mount",
                "Out",
                "Publish",
                "Restore",
                "Save",
                "Sync",
                "Unpublish",
                "Update",
                "Debug",
                "Measure",
                "Ping",
                "Repair",
                "Resolve",
                "Test",
                "Trace",
                "Approve",
                "Assert",
                "Build",
                "Complete",
                "Confirm",
                "Deny",
                "Deploy",
                "Disable",
                "Enable",
                "Install",
                "Invoke",
                "Register",
                "Request",
                "Restart",
                "Resume",
                "Start",
                "Stop",
                "Submit",
                "Suspend",
                "Uninstall",
                "Unregister",
                "Wait",
                "Block",
                "Grant",
                "Protect",
                "Revoke",
                "Unblock",
                "Unprotect",
                "Use"
        ));

        methodNames = new HashSet<String>();

        nullablePrimitives = new HashSet<String>(Arrays.asList(
                "System.Nullable[Byte]",
                "System.Nullable[SByte]",
                "System.Nullable[Int16]",
                "System.Nullable[Int32]",
                "System.Nullable[Int64]",
                "System.Nullable[UInt16]",
                "System.Nullable[UInt32]",
                "System.Nullable[UInt64]",
                "System.Nullable[Decimal]",
                "System.Nullable[Single]",
                "System.Nullable[Double]",
                "System.Nullable[Boolean]"
        ));

        // list of reserved words - must be in lower case
        reservedWords = new HashSet<String>(Arrays.asList(
                // https://richardspowershellblog.wordpress.com/2009/05/02/powershell-reserved-words/
                "begin",
                "break",
                "catch",
                "continue",
                "data",
                "do",
                "dynamicparam",
                "else",
                "elseif",
                "end",
                "exit",
                "filter",
                "finally",
                "for",
                "foreach",
                "from",
                "function",
                "if",
                "in",
                "param",
                "process",
                "return",
                "switch",
                "throw",
                "trap",
                "try",
                "until",
                "while",
                "local",
                "private",
                "where",
                // special variables 
                "args",
                "consolefilename",
                "error",
                "event",
                "eventargs",
                "eventsubscriber",
                "executioncontext",
                "false",
                "foreach",
                "home",
                "host",
                "input",
                "lastexitcode",
                "matches",
                "myinvocation",
                "nestedpromptlevel",
                "null",
                "pid",
                "profile",
                "pscmdlet",
                "pscommandpath",
                "psculture",
                "psdebugcontext",
                "pshome",
                "psitem",
                "psscriptroot",
                "pssenderinfo",
                "psuiculture",
                "psversiontable",
                "sender",
                "shellid",
                "stacktrace",
                "this",
                "true"
        ));

        defaultIncludes = new HashSet<String>(Arrays.asList(
                "Byte",
                "SByte",
                "Byte[]",
                "Int16",
                "Int32",
                "Int64",
                "UInt16",
                "UInt32",
                "UInt64",
                "Decimal",
                "Single",
                "Double",
                "TimeSpan",
                "System.DateTime",
                "ProgressRecord",
                "Char",
                "String",
                "XmlDocument",
                "SecureString",
                "Boolean",
                "Guid",
                "Uri",
                "System.IO.FileInfo",
                "Version"
        ));

        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "String");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("integer", "Int32");
        typeMapping.put("float", "Double");
        typeMapping.put("long", "Int64");
        typeMapping.put("double", "Double");
        typeMapping.put("number", "Decimal");
        typeMapping.put("object", "System.Collections.Hashtable");
        typeMapping.put("file", "System.IO.FileInfo");
        typeMapping.put("ByteArray", "System.Byte[]");
        typeMapping.put("binary", "System.IO.FileInfo");
        typeMapping.put("date", "System.DateTime");
        typeMapping.put("date-time", "System.DateTime");
        typeMapping.put("Date", "System.DateTime");
        typeMapping.put("DateTime", "System.DateTime");
        typeMapping.put("UUID", "String");
        typeMapping.put("URI", "String");

        cliOptions.clear();
        cliOptions.add(new CliOption("powershellGalleryUrl", "URL to the module in PowerShell Gallery (e.g. https://www.powershellgallery.com/packages/PSTwitter/)."));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Client package name (e.g. PSTwitter).").defaultValue(this.packageName));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Package version (e.g. 0.1.2).").defaultValue(this.packageVersion));
        cliOptions.add(new CliOption(CodegenConstants.OPTIONAL_PROJECT_GUID, "GUID for PowerShell module (e.g. a27b908d-2a20-467f-bc32-af6f3a654ac5). A random GUID will be generated by default."));
        cliOptions.add(new CliOption(CodegenConstants.API_NAME_PREFIX, "Prefix that will be appended to all PS objects. Default: empty string. e.g. Pet => PSPet."));
        cliOptions.add(new CliOption("commonVerbs", "PS common verb mappings. e.g. Delete=Remove:Patch=Update to map Delete with Remove and Patch with Update accordingly."));

    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "powershell-experimental";
    }

    public String getHelp() {
        return "Generates a PowerShell API client (beta)";
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
        this.apiPackage = packageName + File.separator + "Api";
        this.modelPackage = packageName + File.separator + "Model";
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    public void setPowershellGalleryUrl(String powershellGalleryUrl) {
        this.powershellGalleryUrl = powershellGalleryUrl;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("POWERSHELL_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable POWERSHELL_POST_PROCESS_FILE not defined so the PowerShell code may not be properly formatted. To define it, try 'export POWERSHELL_POST_PROCESS_FILE=\"Edit-DTWBeautifyScript\"'");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey("powershellGalleryUrl")) {
            setPowershellGalleryUrl((String) additionalProperties.get("powershellGalleryUrl"));
        } else {
            additionalProperties.put("powershellGalleryUrl", powershellGalleryUrl);
        }

        if (StringUtils.isNotBlank(powershellGalleryUrl)) {
            // get the last segment of the URL
            // e.g. https://www.powershellgallery.com/packages/PSTwitter => PSTwitter
            additionalProperties.put("powershellGalleryId", powershellGalleryUrl.replaceFirst(".*/([^/?]+).*", "$1"));
        }

        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_GUID)) {
            setPackageGuid((String) additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_GUID));
        } else {
            additionalProperties.put("packageGuid", packageGuid);
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            this.setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            this.setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        }

        if (additionalProperties.containsKey("commonVerbs")) {
            String[] entries = ((String)additionalProperties.get("commonVerbs")).split(":");
            for (String entry : entries) {
                String[] pair = entry.split("=");
                if (pair.length == 2) {
                    commonVerbs.put(pair[0], pair[1]);
                    LOGGER.debug("Add commonVerbs: {} => {}", pair[0], pair[1]);
                } else {
                    LOGGER.error("Failed to parse commonVerbs: {}", entry);
                }
            }
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            LOGGER.warn(CodegenConstants.MODEL_PACKAGE + " with " + this.getName() + " generator is ignored. Setting this value independently of " + CodegenConstants.PACKAGE_NAME + " is not currently supported.");
        }

        if (additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            LOGGER.warn(CodegenConstants.API_PACKAGE + " with " + this.getName() + " generator is ignored. Setting this value independently of " + CodegenConstants.PACKAGE_NAME + " is not currently supported.");
        }

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Build.ps1.mustache", "", "Build.ps1"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator);

        supportingFiles.add(new SupportingFile("Org.OpenAPITools.psm1.mustache", infrastructureFolder, packageName + ".psm1"));

        // client/configuration
        supportingFiles.add(new SupportingFile("configuration.mustache", infrastructureFolder + "Client", apiNamePrefix + "Configuration.ps1"));

        // private
        supportingFiles.add(new SupportingFile("api_client.mustache", infrastructureFolder + "Private", apiNamePrefix + "ApiClient.ps1"));
        supportingFiles.add(new SupportingFile("Get-CommonParameters.mustache", infrastructureFolder + File.separator + "Private" + File.separator, "Get-CommonParameters.ps1"));
        supportingFiles.add(new SupportingFile("Out-DebugParameter.mustache", infrastructureFolder + File.separator + "Private" + File.separator, "Out-DebugParameter.ps1"));
        supportingFiles.add(new SupportingFile("http_signature_auth.mustache", infrastructureFolder + "Private", apiNamePrefix + "HttpSignatureAuth.ps1"));
        supportingFiles.add(new SupportingFile("rsa_provider.mustache", infrastructureFolder + "Private", apiNamePrefix + "RSAEncryptionProvider.cs"));
        

        // en-US
        supportingFiles.add(new SupportingFile("about_Org.OpenAPITools.help.txt.mustache", infrastructureFolder + File.separator + "en-US" + File.separator + "about_" + packageName + ".help.txt"));

        // appveyor
        supportingFiles.add(new SupportingFile("appveyor.mustache", "", "appveyor.yml"));
    }

    @SuppressWarnings("static-method")
    @Override
    public String escapeText(String input) {
        
        if (input == null) {
            return input;
        }

        // remove \t, \n, \r
        // replace \ with \\
        // replace " with \"
        // outter unescape to retain the original multi-byte characters
        // finally escalate characters avoiding code injection
        return escapeUnsafeCharacters(
                StringEscapeUtils.unescapeJava(
                        StringEscapeUtils.escapeJava(input)
                                .replace("\\/", "/"))
                        .replaceAll("[\\t\\n\\r]", " ")
                        .replace("\\", "\\\\")
                        .replace("\"", "\"\""));
        
    }
    
    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("#>", "#_>").replace("<#", "<_#");
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiFilename(name) + ".Tests";
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelFilename(name) + ".Tests";
    }

    @Override
    public String apiTestFileFolder() {
        return (outputFolder + "/" + apiTestPath).replace('/', File.separatorChar);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage();
    }

    @Override
    public String modelTestFileFolder() {
        return (outputFolder + "/" + modelTestPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }


    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage();
    }

    @Override
    public String escapeReservedWord(String name) {
        return "Var" + name;
    }

    /**
     * Output the proper model name (capitalized).
     * In case the name belongs to the TypeSystem it won't be renamed.
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    @Override
    public String toModelName(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        name = camelize(sanitizeName(name));

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word or special variable name) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = camelize("model_" + name); // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = camelize("model_" + name); // e.g. 200Response => Model200Response (after camelize)
        }

        return name;
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    /**
     * returns the OpenAPI type for the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the type
     **/
    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;

        // This maps, for example, long -> Long based on hashes in this type's constructor
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }

        // model/object
        return toModelName(type);
    }

    /**
     * Output the type declaration of the property
     *
     * @param p OpenAPI Schema object
     * @return a string presentation of the property type
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getTypeDeclaration(inner) + "[]";
        } else if (ModelUtils.isMapSchema(p)) {
            return "System.Collections.Hashtable";
        } else if (!languageSpecificPrimitives.contains(getSchemaType(p))) {
            return super.getTypeDeclaration(p);
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty (should not occur as an auto-generated method name will be used)
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        return sanitizeName(operationId);
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        HashMap<String, CodegenModel> modelMaps = new HashMap<String, CodegenModel>();
        HashMap<String, Integer> processedModelMaps = new HashMap<String, Integer>();

        for (Object o : allModels) {
            HashMap<String, Object> h = (HashMap<String, Object>) o;
            CodegenModel m = (CodegenModel) h.get("model");
            modelMaps.put(m.classname, m);
        }

        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            int index = 0;
            for (CodegenParameter p : op.allParams) {
                p.vendorExtensions.put("x-powershell-data-type", getPSDataType(p));
                p.vendorExtensions.put("x-powershell-example", constructExampleCode(p, modelMaps, processedModelMaps));
                p.vendorExtensions.put("x-index", index);
                index++;
            }

            if (!op.vendorExtensions.containsKey("x-powershell-method-name")) { // x-powershell-method-name not set
                String methodName = toMethodName(op.operationId);
                op.vendorExtensions.put("x-powershell-method-name", methodName);
                op.vendorExtensions.put("x-powershell-method-name-lowercase", methodName);
            } else {
                op.vendorExtensions.put("x-powershell-method-name-lowercase", ((String) op.vendorExtensions.get("x-powershell-method-name")).toLowerCase(Locale.ROOT));
            }

            // detect duplicated method name
            if (methodNames.contains(op.vendorExtensions.get("x-powershell-method-name"))) {
                LOGGER.error("Duplicated method name found: {}", op.vendorExtensions.get("x-powershell-method-name"));
            } else {
                methodNames.add(op.vendorExtensions.get("x-powershell-method-name"));
            }

            if (op.produces != null && op.produces.size() > 1) {
                op.vendorExtensions.put("x-powershell-select-accept", true);
            }
        }

        processedModelMaps.clear();
        for (CodegenOperation operation : operationList) {
            for (CodegenParameter cp : operation.allParams) {
                cp.vendorExtensions.put("x-powershell-example", constructExampleCode(cp, modelMaps, processedModelMaps));
            }
        }

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        // add x-index to properties
        ProcessUtils.addIndexToProperties(models);

        // add x-data-type to store powershell type
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            for (CodegenProperty cp : cm.allVars) {
                cp.vendorExtensions.put("x-powershell-data-type", getPSDataType(cp));
            }
        }

        return objs;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // camelize the variable name
        // pet_id => PetId
        name = camelize(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            LOGGER.warn(name + " (reserved word or special variable name) cannot be used in naming. Renamed to " + escapeReservedWord(name));
            name = escapeReservedWord(name);
        }

        return name;
    }

    private String constructExampleCode(CodegenParameter codegenParameter, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        if (codegenParameter.isListContainer) { // array
            return "@(" + constructExampleCode(codegenParameter.items, modelMaps, processedModelMap) + ")";
        } else if (codegenParameter.isMapContainer) { // TODO: map, file type
            return "@{ \"Key\" = \"Value\" }";
        } else if (languageSpecificPrimitives.contains(codegenParameter.dataType) ||
                nullablePrimitives.contains(codegenParameter.dataType)) { // primitive type
            if ("String".equals(codegenParameter.dataType) || "Character".equals(codegenParameter.dataType)) {
                if (StringUtils.isEmpty(codegenParameter.example)) {
                    return "\"" + codegenParameter.example + "\"";
                } else {
                    return "\"" + codegenParameter.paramName + "_example\"";
                }
            } else if ("Boolean".equals(codegenParameter.dataType) ||
                    "System.Nullable[Boolean]".equals(codegenParameter.dataType)) { // boolean
                if (Boolean.parseBoolean(codegenParameter.example)) {
                    return "true";
                } else {
                    return "false";
                }
            } else if ("URL".equals(codegenParameter.dataType)) { // URL
                return "URL(string: \"https://example.com\")!";
            } else if ("System.DateTime".equals(codegenParameter.dataType)) { // datetime or date
                return "Get-Date";
            } else { // numeric
                if (StringUtils.isEmpty(codegenParameter.example)) {
                    return codegenParameter.example;
                } else {
                    return "987";
                }
            }
        } else { // model
            // look up the model
            if (modelMaps.containsKey(codegenParameter.dataType)) {
                return constructExampleCode(modelMaps.get(codegenParameter.dataType), modelMaps, processedModelMap);
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenParameter.dataType);
                return "TODO";
            }
        }
    }

    private String constructExampleCode(CodegenProperty codegenProperty, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        if (codegenProperty.isListContainer) { // array
            return "@(" + constructExampleCode(codegenProperty.items, modelMaps, processedModelMap) + ")";
        } else if (codegenProperty.isMapContainer) { // map
            return "\"TODO\"";
        } else if (languageSpecificPrimitives.contains(codegenProperty.dataType) || // primitive type
                nullablePrimitives.contains(codegenProperty.dataType)) { // nullable primitive type
            if ("String".equals(codegenProperty.dataType)) {
                if (StringUtils.isEmpty(codegenProperty.example)) {
                    return "\"" + codegenProperty.example + "\"";
                } else {
                    return "\"" + codegenProperty.name + "_example\"";
                }
            } else if ("Boolean".equals(codegenProperty.dataType) ||
                    "System.Nullable[Boolean]".equals(codegenProperty.dataType)) { // boolean
                if (Boolean.parseBoolean(codegenProperty.example)) {
                    return "$true";
                } else {
                    return "$false";
                }
            } else if ("URL".equals(codegenProperty.dataType)) { // URL
                return "URL(string: \"https://example.com\")!";
            } else if ("System.DateTime".equals(codegenProperty.dataType)) { // datetime or date
                return "Get-Date";
            } else { // numeric
                if (StringUtils.isEmpty(codegenProperty.example)) {
                    return codegenProperty.example;
                } else {
                    return "123";
                }
            }
        } else {
            // look up the model
            if (modelMaps.containsKey(codegenProperty.dataType)) {
                return constructExampleCode(modelMaps.get(codegenProperty.dataType), modelMaps, processedModelMap);
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenProperty.dataType);
                return "\"TODO\"";
            }
        }
    }

    private String constructExampleCode(CodegenModel codegenModel, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        String example;

        // break infinite recursion. Return, in case a model is already processed in the current context.
        String model = codegenModel.name;
        if (processedModelMap.containsKey(model)) {
            int count = processedModelMap.get(model);
            if (count == 1) {
                processedModelMap.put(model, 2);
            } else if (count == 2) {
                return "";
            } else {
                throw new RuntimeException("Invalid count when constructing example: " + count);
            }
        } else {
            processedModelMap.put(model, 1);
        }

        example = "(Initialize-" + codegenModel.name;
        List<String> propertyExamples = new ArrayList<>();
        for (CodegenProperty codegenProperty : codegenModel.allVars) {
            propertyExamples.add("-" + codegenProperty.name + " " + constructExampleCode(codegenProperty, modelMaps, processedModelMap));
        }
        example += StringUtils.join(propertyExamples, " ");
        example += ")";
        return example;
    }

    private String getPSDataType(CodegenProperty cp) {
        String dataType;
        if (cp.isPrimitiveType) {
            dataType = cp.dataType;
            if (!(cp.isString || cp.isFile || cp.isContainer)
                    && (cp.isNullable || !cp.required)) {
                dataType = "System.Nullable[" + dataType + "]";
            }
            return dataType;
        } else if (cp.isListContainer) { // array
            return getPSDataType(cp.items) + "[]";
        } else if (cp.isMapContainer) { // map
            return "System.Collections.Hashtable";
        } else { // model
            return "PSCustomObject";
        }
    }

    private String getPSDataType(CodegenParameter cp) {
        String dataType;
        if (cp.isPrimitiveType) {
            dataType = cp.dataType;
            if (!(cp.isString || cp.isFile || cp.isContainer)
                    && (cp.isNullable || !cp.required)) {
                dataType = "System.Nullable[" + dataType + "]";
            }
            return dataType;
        } else if (cp.isListContainer) { // array
            return getPSDataType(cp.items) + "[]";
        } else if (cp.isMapContainer) { // map
            return "System.Collections.Hashtable";
        } else { // model
            return "PSCustomObject";
        }
    }

    private String toMethodName(String operationId) {
        String methodName = camelize(operationId);

        // check if method name starts with powershell verbs
        for (String verb : (HashSet<String>) powershellVerbs) {
            if (methodName.startsWith(verb)) {
                methodName = verb + "-" + apiNamePrefix + methodName.substring(verb.length());
                LOGGER.info("Naming the method using the PowerShell verb: {} => {}", operationId, methodName);
                return methodName;
            }
        }

        for (Map.Entry<String, String> entry : commonVerbs.entrySet()) {
            if (methodName.startsWith(entry.getKey())) {
                methodName = entry.getValue() + "-" + apiNamePrefix + methodName.substring(entry.getKey().length());
                LOGGER.info("Naming the method by mapping the common verbs (e.g. Create, Change) to PS verbs: {} => {}", operationId, methodName);
                return methodName;
            }
        }

        // not using powershell verb
        return "Invoke-" + apiNamePrefix + methodName;
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }
        String powershellPostProcessFile = System.getenv("POWERSHELL_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(powershellPostProcessFile)) {
            return; // skip if POWERSHELL_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with ps extension
        if ("ps".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = powershellPostProcessFile + " " + file.toString();
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: " + command);
                }
            } catch (Exception e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
            }
        }

    }

    @Override
    public String toRegularExpression(String pattern) {
        return escapeText(pattern);
    }

}
