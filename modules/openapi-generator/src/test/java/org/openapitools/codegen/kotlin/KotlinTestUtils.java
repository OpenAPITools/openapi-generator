package org.openapitools.codegen.kotlin;

import kotlin.script.experimental.jvm.util.KotlinJars;
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys;
import org.jetbrains.kotlin.cli.common.config.ContentRootsKt;
import org.jetbrains.kotlin.cli.common.messages.MessageRenderer;
import org.jetbrains.kotlin.cli.common.messages.PrintingMessageCollector;
import org.jetbrains.kotlin.cli.jvm.compiler.EnvironmentConfigFiles;
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment;
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinToJVMBytecodeCompiler;
import org.jetbrains.kotlin.cli.jvm.config.JvmContentRootsKt;
import org.jetbrains.kotlin.codegen.state.GenerationState;
import org.jetbrains.kotlin.com.intellij.openapi.Disposable;
import org.jetbrains.kotlin.config.CommonConfigurationKeys;
import org.jetbrains.kotlin.config.CompilerConfiguration;
import org.jetbrains.kotlin.config.JVMConfigurationKeys;
import org.jetbrains.kotlin.config.JvmTarget;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.util.*;

import static kotlin.script.experimental.jvm.util.JvmClasspathUtilKt.classpathFromClassloader;

public class KotlinTestUtils {

    public static ClassLoader buildModule(List<String> sourcePath, ClassLoader classLoader) {

        String moduleName = UUID.randomUUID().toString().replaceAll("-", "");
        File saveClassesDir;
        try {
            saveClassesDir = Files.createTempDirectory("kotlin" + moduleName).toFile();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        compileModule(moduleName, sourcePath, saveClassesDir, classLoader, true);

        try {
            return new URLClassLoader(new URL[]{saveClassesDir.toURI().toURL()}, classLoader);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }

    }

    private static GenerationState compileModule(String moduleName, List<String> sourcePath, File saveClassesDir, ClassLoader classLoader, boolean forcedAddKotlinStd) {
        Disposable stubDisposable = new StubDisposable();
        CompilerConfiguration configuration = new CompilerConfiguration();
        configuration.put(CommonConfigurationKeys.MODULE_NAME, moduleName);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = null;
        try {
            ps = new PrintStream(baos, true, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
        configuration.put(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, new PrintingMessageCollector(ps, MessageRenderer.PLAIN_FULL_PATHS, true));
        configuration.put(JVMConfigurationKeys.OUTPUT_DIRECTORY, saveClassesDir);
//        configuration.put(JVMConfigurationKeys.RETAIN_OUTPUT_IN_MEMORY, true)
        configuration.put(JVMConfigurationKeys.JVM_TARGET, JvmTarget.JVM_1_8);
        Set<File> classPath = new HashSet<>();
        if (classLoader != null) {
            classPath.addAll(classpathFromClassloader(classLoader, false));
        }
        if (forcedAddKotlinStd) {
            classPath.add(KotlinJars.INSTANCE.getStdlib());
        }
        JvmContentRootsKt.addJvmClasspathRoots(configuration, new ArrayList<>(classPath));
        ContentRootsKt.addKotlinSourceRoots(configuration, sourcePath);

        KotlinCoreEnvironment env = KotlinCoreEnvironment.createForProduction(stubDisposable, configuration, EnvironmentConfigFiles.JVM_CONFIG_FILES);
        GenerationState result = KotlinToJVMBytecodeCompiler.INSTANCE.analyzeAndGenerate(env);
        ps.flush();
        if (result != null) {
            return result;
        } else {
            String s;
            try {
                s = baos.toString("UTF-8");
            } catch (UnsupportedEncodingException e) {
                throw new RuntimeException(e);
            }
            throw new IllegalStateException("Compilation error. Details:\n" + s);
        }
    }

    static class StubDisposable implements Disposable {

        volatile boolean isDisposed = false;

        @Override
        public void dispose() {
            isDisposed = true;
        }

        public boolean isDisposed() {
            return isDisposed;
        }
    }
}
