package org.openapitools.codegen.kotlin;

import kotlin.script.experimental.jvm.util.KotlinJars;
import lombok.Getter;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys;
import org.jetbrains.kotlin.cli.common.config.ContentRootsKt;
import org.jetbrains.kotlin.cli.common.messages.MessageRenderer;
import org.jetbrains.kotlin.cli.common.messages.PrintingMessageCollector;
import org.jetbrains.kotlin.cli.jvm.compiler.EnvironmentConfigFiles;
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment;
import org.jetbrains.kotlin.cli.jvm.config.JvmContentRootsKt;
import org.jetbrains.kotlin.cli.common.ExitCode;
import org.jetbrains.kotlin.cli.common.arguments.K2JVMCompilerArguments;
import org.jetbrains.kotlin.cli.common.messages.MessageCollector;
import org.jetbrains.kotlin.cli.jvm.K2JVMCompiler;
import org.jetbrains.kotlin.config.Services;
import org.jetbrains.kotlin.cli.common.messages.MessageRenderer;
import org.jetbrains.kotlin.cli.common.messages.PrintingMessageCollector;
import org.jetbrains.kotlin.com.intellij.openapi.Disposable;
import org.jetbrains.kotlin.config.CommonConfigurationKeys;
import org.jetbrains.kotlin.config.CompilerConfiguration;
import org.jetbrains.kotlin.config.JVMConfigurationKeys;
import org.jetbrains.kotlin.config.JvmTarget;
import org.openapitools.codegen.antlr4.KotlinParser;
import org.openapitools.codegen.antlr4.KotlinParserBaseListener;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.stream.Collectors;
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

    private static void compileModule(
        String moduleName,
        List<String> sourcePath,
        File saveClassesDir,
        ClassLoader classLoader,
        boolean forcedAddKotlinStd) {

        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        try (PrintStream ps = new PrintStream(baos, true, StandardCharsets.UTF_8)) {
            MessageCollector messageCollector =
                    new PrintingMessageCollector(
                            ps,
                            MessageRenderer.PLAIN_FULL_PATHS,
                            true);

            Set<File> classPath = new HashSet<>();
            if (classLoader != null) {
                classPath.addAll(classpathFromClassloader(classLoader, false));
            }
            if (forcedAddKotlinStd) {
                classPath.add(KotlinJars.INSTANCE.getStdlib());
            }

            K2JVMCompiler compiler = new K2JVMCompiler();
            K2JVMCompilerArguments arguments = compiler.createArguments();

            arguments.setModuleName(moduleName);
            arguments.setDestination(saveClassesDir.getAbsolutePath());
            arguments.setJvmTarget("1.8");
            arguments.setClasspath(
                    classPath.stream()
                            .map(File::getAbsolutePath)
                            .collect(Collectors.joining(File.pathSeparator)));
            arguments.setFreeArgs(sourcePath);

            ExitCode exitCode = compiler.exec(
                    messageCollector,
                    Services.EMPTY,
                    arguments);

            if (exitCode != ExitCode.OK) {
                throw new IllegalStateException(
                        "Compilation error. Details:\n" + baos);
            }
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

    public static class CustomKotlinParseListener extends KotlinParserBaseListener {
        @Getter
        private int stringReferenceCount = 0;

        @Override
        public void exitLineStringContent(KotlinParser.LineStringContentContext ctx) {
            if(ctx.LineStrRef() != null) {
                stringReferenceCount++;
            }
        }

        @Override
        public void exitMultiLineStringContent(KotlinParser.MultiLineStringContentContext ctx) {
            if(ctx.MultiLineStrRef() != null) {
                stringReferenceCount++;
            }
        }
    }

    public static class SyntaxErrorListener extends BaseErrorListener {

        @Getter
        private int syntaxErrorCount = 0;
        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
            syntaxErrorCount++;
        }
    }
}
