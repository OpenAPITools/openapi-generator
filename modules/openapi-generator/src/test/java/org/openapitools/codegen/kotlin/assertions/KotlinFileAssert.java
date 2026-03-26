package org.openapitools.codegen.kotlin.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.CanIgnoreReturnValue;
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys;
import org.jetbrains.kotlin.cli.common.messages.MessageCollector;
import org.jetbrains.kotlin.cli.jvm.compiler.EnvironmentConfigFiles;
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment;
import org.jetbrains.kotlin.com.intellij.openapi.util.Disposer;
import org.jetbrains.kotlin.com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.kotlin.com.intellij.openapi.vfs.local.CoreLocalFileSystem;
import org.jetbrains.kotlin.com.intellij.psi.PsiManager;
import org.jetbrains.kotlin.config.CompilerConfiguration;
import org.jetbrains.kotlin.psi.KtClass;
import org.jetbrains.kotlin.psi.KtFile;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@CanIgnoreReturnValue
public class KotlinFileAssert extends AbstractAssert<KotlinFileAssert, KtFile> {

    private KotlinFileAssert(final KtFile ktFile) {
        super(ktFile, KotlinFileAssert.class);
    }

    public static KotlinFileAssert assertThat(final File file) {
        final CompilerConfiguration config = new CompilerConfiguration();
        config.put(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, MessageCollector.Companion.getNONE());
        final KotlinCoreEnvironment env = KotlinCoreEnvironment.createForProduction(Disposer.newDisposable(), config, EnvironmentConfigFiles.JVM_CONFIG_FILES);
        final VirtualFile vFile = new CoreLocalFileSystem().findFileByIoFile(file);
        Assertions.assertThat(vFile)
                .withFailMessage("Expected file %s to exist but was not found", file.getAbsolutePath())
                .isNotNull();
        final KtFile ktFile = (KtFile) PsiManager.getInstance(env.getProject()).findFile(vFile);
        return new KotlinFileAssert(ktFile);
    }

    public ClassAssert assertClass(final String className) {
        final List<KtClass> ktClasses = Arrays.stream(actual.findChildrenByClass(KtClass.class))
                .filter(clazz -> Objects.equals(clazz.getName(), className)).collect(Collectors.toList());
        Assertions.assertThat(ktClasses)
                .withFailMessage("Expected file to have single class %s, but found %s", className, ktClasses.size())
                .hasSize(1);

        return new ClassAssert(this, ktClasses.get(0));
    }
}
