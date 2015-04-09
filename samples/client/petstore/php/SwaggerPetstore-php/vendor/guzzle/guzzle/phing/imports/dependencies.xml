<?xml version="1.0" encoding="UTF-8"?>
<project basedir="../../" default="install-dependencies">

    <property name="cmd.composer" value="" />
    <property name="cmd.git" value="" />
    <property name="cmd.testserver" value="" />

    <!--
    Our custom tasks
    -->
    <taskdef name="composerlint" classname="phing.tasks.ComposerLintTask" />
    <taskdef name="guzzlesubsplit" classname="phing.tasks.GuzzleSubSplitTask" />
    <taskdef name="guzzlepear" classname="phing.tasks.GuzzlePearPharPackageTask" />

    <target name="find-git">
        <if>
            <contains string="${cmd.git}" substring="git" />
            <then>
                <echo>using git at ${cmd.git}</echo>
            </then>
        <else>
            <exec command="which git" outputProperty="cmd.git" />
            <echo>found git at ${cmd.git}</echo>
        </else>
        </if>
    </target>

    <target name="clean-dependencies">
        <delete dir="${project.basedir}/vendor"/>
        <delete file="${project.basedir}/composer.lock" />
    </target>

</project>
