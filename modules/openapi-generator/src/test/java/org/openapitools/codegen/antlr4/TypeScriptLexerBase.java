package org.openapitools.codegen.antlr4;

import org.antlr.v4.runtime.*;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * All lexer methods that used in grammar (IsStrictMode)
 * should start with Upper Case Char similar to Lexer rules.
 */
public abstract class TypeScriptLexerBase extends Lexer
{
    /**
     * Stores values of nested modes. By default mode is strict or
     * defined externally (useStrictDefault)
     */
    private final Deque<Boolean> scopeStrictModes = new ArrayDeque<>();

    private Token lastToken = null;
    /**
     * Default value of strict mode
     * Can be defined externally by setUseStrictDefault
     */
    private boolean useStrictDefault = false;
    /**
     * Current value of strict mode
     * Can be defined during parsing, see StringFunctions.js and StringGlobal.js samples
     */
    private boolean useStrictCurrent = false;
    /**
     * Keeps track of the current depth of nested template string backticks.
     * E.g. after the X in:
     *
     * `${a ? `${X
     *
     * templateDepth will be 2. This variable is needed to determine if a `}` is a
     * plain CloseBrace, or one that closes an expression inside a template string.
     */
    private int templateDepth = 0;

    /**
     * Keeps track of the depth of open- and close-braces. Used for expressions like:
     *
     * `${[1, 2, 3].map(x => { return x * 2;}).join("")}`
     *
     * where the '}' from `return x * 2;}` should not become a `TemplateCloseBrace`
     * token but rather a `CloseBrace` token.
     */
    private int bracesDepth = 0;

    public TypeScriptLexerBase(CharStream input) {
        super(input);
    }

    public boolean getStrictDefault() {
        return useStrictDefault;
    }

    public void setUseStrictDefault(boolean value) {
        useStrictDefault = value;
        useStrictCurrent = value;
    }

    public boolean IsStrictMode() {
        return useStrictCurrent;
    }

    public void StartTemplateString() {
        this.bracesDepth = 0;
    }

    public boolean IsInTemplateString() {
        return this.templateDepth > 0 && this.bracesDepth == 0;
    }

    /**
     * Return the next token from the character stream and records this last
     * token in case it resides on the default channel. This recorded token
     * is used to determine when the lexer could possibly match a regex
     * literal. Also changes scopeStrictModes stack if tokenize special
     * string 'use strict';
     *
     * @return the next token from the character stream.
     */
    @Override
    public Token nextToken() {
        Token next = super.nextToken();

        if (next.getChannel() == Token.DEFAULT_CHANNEL) {
            // Keep track of the last token on the default channel.
            this.lastToken = next;
        }

        return next;
    }

    protected void ProcessOpenBrace()
    {
        bracesDepth++;
        useStrictCurrent = scopeStrictModes.size() > 0 && scopeStrictModes.peek() ? true : useStrictDefault;
        scopeStrictModes.push(useStrictCurrent);
    }

    protected void ProcessCloseBrace()
    {
        bracesDepth--;
        useStrictCurrent = scopeStrictModes.size() > 0 ? scopeStrictModes.pop() : useStrictDefault;
    }

    protected void ProcessStringLiteral()
    {
        if (lastToken == null || lastToken.getType() == TypeScriptLexer.OpenBrace)
        {
            String text = getText();
            if (text.equals("\"use strict\"") || text.equals("'use strict'"))
            {
                if (scopeStrictModes.size() > 0)
                    scopeStrictModes.pop();
                useStrictCurrent = true;
                scopeStrictModes.push(useStrictCurrent);
            }
        }
    }

    protected void IncreaseTemplateDepth() {
        this.templateDepth++;
    }

    protected void DecreaseTemplateDepth() {
        this.templateDepth--;
    }

    /**
     * Returns {@code true} if the lexer can match a regex literal.
     */
    protected boolean IsRegexPossible() {
                                       
        if (this.lastToken == null) {
            // No token has been produced yet: at the start of the input,
            // no division is possible, so a regex literal _is_ possible.
            return true;
        }
        
        switch (this.lastToken.getType()) {
            case TypeScriptLexer.Identifier:
            case TypeScriptLexer.NullLiteral:
            case TypeScriptLexer.BooleanLiteral:
            case TypeScriptLexer.This:
            case TypeScriptLexer.CloseBracket:
            case TypeScriptLexer.CloseParen:
            case TypeScriptLexer.OctalIntegerLiteral:
            case TypeScriptLexer.DecimalLiteral:
            case TypeScriptLexer.HexIntegerLiteral:
            case TypeScriptLexer.StringLiteral:
            case TypeScriptLexer.PlusPlus:
            case TypeScriptLexer.MinusMinus:
                // After any of the tokens above, no regex literal can follow.
                return false;
            default:
                // In all other cases, a regex literal _is_ possible.
                return true;
        }
    }
}