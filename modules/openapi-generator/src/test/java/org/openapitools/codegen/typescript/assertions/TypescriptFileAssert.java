package org.openapitools.codegen.typescript.assertions;

import lombok.Getter;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.openapitools.codegen.antlr4.TypeScriptLexer;
import org.openapitools.codegen.antlr4.TypeScriptParser;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class TypescriptFileAssert extends AbstractAssert<TypescriptFileAssert, TypeScriptParser.ProgramContext> {

    private CustomErrorListener customErrorListener;

//    public TypescriptFileAssert(TypeScriptParser.ProgramContext programContext) {
//        super(programContext, TypescriptFileAssert.class);
//    }

    private TypescriptFileAssert(TypeScriptParser.ProgramContext programContext, CustomErrorListener customErrorListener) {
        super(programContext, TypescriptFileAssert.class);
        this.customErrorListener = customErrorListener;
    }
    public static TypescriptFileAssert assertThat(String source) {
        CustomErrorListener customErrorListener = new CustomErrorListener();
        TypeScriptLexer typeScriptLexer = new TypeScriptLexer(CharStreams.fromString(source));
        typeScriptLexer.addErrorListener(customErrorListener);
        CommonTokenStream commonTokenStream = new CommonTokenStream(typeScriptLexer);
        TypeScriptParser typeScriptParser = new TypeScriptParser(commonTokenStream);
        typeScriptParser.addErrorListener(customErrorListener);
        TypeScriptParser.ProgramContext programContext = typeScriptParser.program();
        return new TypescriptFileAssert(programContext, customErrorListener);
    }

    public TypescriptFileAssert isValid() {
        int syntaxErrorCount = customErrorListener.syntaxErrorCount;
        Assertions.assertThat(syntaxErrorCount)
                .isEqualTo(0)
                .withFailMessage("%d syntax errors", syntaxErrorCount);
        return this;
    }

    public InterfaceAssert assertInterface(String interfaceName) {
        Optional<TypeScriptParser.InterfaceDeclarationContext> interfaceDeclarationContext = actual
                .sourceElements().sourceElement().stream().filter((se) -> {
                    return se.statement().interfaceDeclaration() != null &&
                            se.statement().interfaceDeclaration().identifier().getText().equals(interfaceName);
                }).map((se) -> se.statement().interfaceDeclaration()).findFirst();
        Assertions.assertThat(interfaceDeclarationContext)
                .withFailMessage("%s is not present or is not an interface", interfaceName)
                .isPresent();
        return new InterfaceAssert(this, interfaceDeclarationContext.get());
    }

    /**
     * Placeholder for more detailed import-related assertions
     */
    public TypescriptFileAssert importsNotContain(String text) {
        boolean containsText = false;
        List<TypeScriptParser.SourceElementContext> sourceElements = actual.sourceElements().sourceElement();
        for(TypeScriptParser.SourceElementContext se: sourceElements) {
            if(se.statement().importStatement() != null &&
                    se.statement().importStatement().getText().contains(text)) {
                containsText = true;
                break;
            }
        }
        Assertions.assertThat(containsText)
                .withFailMessage("%s is in imports", text)
                .isFalse();
        return this;
    }

    private static class CustomErrorListener extends BaseErrorListener {
            @Getter
            private int syntaxErrorCount = 0;
            @Override
            public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
                syntaxErrorCount++;
            }

    }
}
