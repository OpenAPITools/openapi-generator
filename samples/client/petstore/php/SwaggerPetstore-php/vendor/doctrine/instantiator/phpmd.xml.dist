<?xml version="1.0" encoding="UTF-8" ?>
<ruleset
    name="Instantiator rules"
    xmlns="http://pmd.sf.net/ruleset/1.0.0"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://pmd.sf.net/ruleset/1.0.0 http://pmd.sf.net/ruleset_xml_schema.xsd"
    xsi:noNamespaceSchemaLocation="http://pmd.sf.net/ruleset_xml_schema.xsd"
>
    <rule ref="rulesets/cleancode.xml">
        <!-- static access is used for caching purposes -->
        <exclude name="StaticAccess"/>
    </rule>
    <rule ref="rulesets/codesize.xml"/>
    <rule ref="rulesets/controversial.xml"/>
    <rule ref="rulesets/design.xml"/>
    <rule ref="rulesets/naming.xml"/>
    <rule ref="rulesets/unusedcode.xml"/>
    <rule
        name="NPathComplexity"
        message="The {0} {1}() has an NPath complexity of {2}. The configured NPath complexity threshold is {3}."
        class="PHP_PMD_Rule_Design_NpathComplexity"
    >
        <properties>
            <property name="minimum" description="The npath reporting threshold" value="10"/>
        </properties>
    </rule>
</ruleset>
