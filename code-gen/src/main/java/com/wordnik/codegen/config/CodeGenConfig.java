package com.wordnik.codegen.config;

/**
 * Sets the configurations required for the code generation
 *
 * User: ramesh
 * Date: 5/25/11
 * Time: 8:39 AM
 */
public abstract class CodeGenConfig {

    private RulesProvider codeGenRulesProvider;

    private DataTypeMappingProvider dataTypeMapper;

    private NamingPolicyProvider nameGenerator;


}
