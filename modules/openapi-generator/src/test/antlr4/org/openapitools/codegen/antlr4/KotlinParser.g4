/**
 * Kotlin Grammar for ANTLR v4
 *
 * Based on:
 * http://jetbrains.github.io/kotlin-spec/#_grammars_and_parsing
 * and
 * http://kotlinlang.org/docs/reference/grammar.html
 *
 * Tested on
 * https://github.com/JetBrains/kotlin/tree/master/compiler/testData/psi
 */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar KotlinParser;

options {
    tokenVocab = KotlinLexer;
}

kotlinFile
    : NL* preamble anysemi* (topLevelObject (anysemi+ topLevelObject?)*)? EOF
    ;

script
    : NL* preamble anysemi* (expression (anysemi+ expression?)*)? EOF
    ;

preamble
    : fileAnnotations? packageHeader importList
    ;

fileAnnotations
    : fileAnnotation+
    ;

fileAnnotation
    : (FILE COLON (LSQUARE unescapedAnnotation+ RSQUARE | unescapedAnnotation) semi?)+
    ;

packageHeader
    : (modifierList? PACKAGE identifier semi?)?
    ;

importList
    : importHeader*
    ;

importHeader
    : IMPORT identifier (DOT MULT | importAlias)? semi?
    ;

importAlias
    : AS simpleIdentifier
    ;

topLevelObject
    : classDeclaration
    | objectDeclaration
    | functionDeclaration
    | propertyDeclaration
    | typeAlias
    ;

classDeclaration
    : modifierList? (CLASS | INTERFACE) NL* simpleIdentifier (NL* typeParameters)? (
        NL* primaryConstructor
    )? (NL* COLON NL* delegationSpecifiers)? (NL* typeConstraints)? (
        NL* classBody
        | NL* enumClassBody
    )?
    ;

primaryConstructor
    : modifierList? (CONSTRUCTOR NL*)? classParameters
    ;

classParameters
    : LPAREN (classParameter (COMMA classParameter)* COMMA?)? RPAREN
    ;

classParameter
    : modifierList? (VAL | VAR)? simpleIdentifier COLON type (ASSIGNMENT expression)?
    ;

delegationSpecifiers
    : annotations* delegationSpecifier (NL* COMMA NL* annotations* delegationSpecifier)*
    ;

delegationSpecifier
    : constructorInvocation
    | userType
    | explicitDelegation
    ;

constructorInvocation
    : userType callSuffix
    ;

explicitDelegation
    : userType NL* BY NL* expression
    ;

classBody
    : LCURL NL* classMemberDeclaration* NL* RCURL
    ;

classMemberDeclaration
    : (
        classDeclaration
        | functionDeclaration
        | objectDeclaration
        | companionObject
        | propertyDeclaration
        | anonymousInitializer
        | secondaryConstructor
        | typeAlias
    ) anysemi+
    ;

anonymousInitializer
    : INIT NL* block
    ;

secondaryConstructor
    : modifierList? CONSTRUCTOR NL* functionValueParameters (
        NL* COLON NL* constructorDelegationCall
    )? NL* block?
    ;

constructorDelegationCall
    : THIS NL* valueArguments
    | SUPER NL* valueArguments
    ;

enumClassBody
    : LCURL NL* enumEntries? (NL* SEMICOLON NL* classMemberDeclaration*)? NL* RCURL
    ;

enumEntries
    : (enumEntry NL*)+ SEMICOLON?
    ;

enumEntry
    : annotations* simpleIdentifier (NL* valueArguments)? (NL* classBody)? (NL* COMMA)?
    ;

functionDeclaration
    : modifierList? FUN (NL* type NL* DOT)? (NL* typeParameters)? (NL* receiverType NL* DOT)? (
        NL* identifier
    )? NL* functionValueParameters (NL* COLON NL* type)? (NL* typeConstraints)? (NL* functionBody)?
    ;

functionValueParameters
    : LPAREN (functionValueParameter (COMMA functionValueParameter)* COMMA?)? RPAREN
    ;

functionValueParameter
    : modifierList? parameter (ASSIGNMENT expression)?
    ;

parameter
    : simpleIdentifier COLON type
    ;

receiverType
    : typeModifierList? (parenthesizedType | nullableType | typeReference)
    ;

functionBody
    : block
    | ASSIGNMENT NL* expression
    ;

objectDeclaration
    : modifierList? OBJECT NL* simpleIdentifier (NL* primaryConstructor)? (
        NL* COLON NL* delegationSpecifiers
    )? (NL* classBody)?
    ;

companionObject
    : modifierList? COMPANION NL* modifierList? OBJECT (NL* simpleIdentifier)? (
        NL* COLON NL* delegationSpecifiers
    )? (NL* classBody)?
    ;

propertyDeclaration
    : modifierList? (VAL | VAR) (NL* typeParameters)? (NL* type NL* DOT)? (
        NL* (multiVariableDeclaration | variableDeclaration)
    ) (NL* typeConstraints)? (NL* (BY | ASSIGNMENT) NL* expression)? (
        NL* getter (semi setter)?
        | NL* setter (semi getter)?
    )?
    ;

multiVariableDeclaration
    : LPAREN variableDeclaration (COMMA variableDeclaration)* RPAREN
    ;

variableDeclaration
    : simpleIdentifier (COLON type)?
    ;

getter
    : modifierList? GETTER
    | modifierList? GETTER NL* LPAREN RPAREN (NL* COLON NL* type)? NL* (
        block
        | ASSIGNMENT NL* expression
    )
    ;

setter
    : modifierList? SETTER
    | modifierList? SETTER NL* LPAREN (annotations | parameterModifier)* (
        simpleIdentifier
        | parameter
    ) RPAREN NL* functionBody
    ;

typeAlias
    : modifierList? TYPE_ALIAS NL* simpleIdentifier (NL* typeParameters)? NL* ASSIGNMENT NL* type
    ;

typeParameters
    : LANGLE NL* typeParameter (NL* COMMA NL* typeParameter)* (NL* COMMA)? NL* RANGLE
    ;

typeParameter
    : modifierList? NL* (simpleIdentifier | MULT) (NL* COLON NL* type)?
    ;

type
    : typeModifierList? (functionType | parenthesizedType | nullableType | typeReference)
    ;

typeModifierList
    : (annotations | SUSPEND NL*)+
    ;

parenthesizedType
    : LPAREN type RPAREN
    ;

nullableType
    : (typeReference | parenthesizedType) NL* QUEST+
    ;

typeReference
    : LPAREN typeReference RPAREN
    | userType
    | DYNAMIC
    ;

functionType
    : (functionTypeReceiver NL* DOT NL*)? functionTypeParameters NL* ARROW (NL* type)
    ;

functionTypeReceiver
    : parenthesizedType
    | nullableType
    | typeReference
    ;

userType
    : simpleUserType (NL* DOT NL* simpleUserType)*
    ;

simpleUserType
    : simpleIdentifier (NL* typeArguments)?
    ;

//parameters for functionType
functionTypeParameters
    : LPAREN NL* (parameter | type)? (NL* COMMA NL* (parameter | type))* (NL* COMMA)? NL* RPAREN
    ;

typeConstraints
    : WHERE NL* typeConstraint (NL* COMMA NL* typeConstraint)*
    ;

typeConstraint
    : annotations* simpleIdentifier NL* COLON NL* type
    ;

block
    : LCURL statements RCURL
    ;

statements
    : anysemi* (statement (anysemi+ statement?)*)?
    ;

statement
    : declaration
    | blockLevelExpression
    ;

blockLevelExpression
    : annotations* NL* expression
    ;

declaration
    : labelDefinition* (classDeclaration | functionDeclaration | propertyDeclaration | typeAlias)
    ;

expression
    : disjunction (assignmentOperator disjunction)*
    ;

disjunction
    : conjunction (NL* DISJ NL* conjunction)*
    ;

conjunction
    : equalityComparison (NL* CONJ NL* equalityComparison)*
    ;

equalityComparison
    : comparison (equalityOperation NL* comparison)*
    ;

comparison
    : namedInfix (comparisonOperator NL* namedInfix)?
    ;

namedInfix
    : elvisExpression ((inOperator NL* elvisExpression)+ | (isOperator NL* type))?
    ;

elvisExpression
    : infixFunctionCall (NL* ELVIS NL* infixFunctionCall)*
    ;

infixFunctionCall
    : rangeExpression (simpleIdentifier NL* rangeExpression)*
    ;

rangeExpression
    : additiveExpression (RANGE NL* additiveExpression)*
    ;

additiveExpression
    : multiplicativeExpression (additiveOperator NL* multiplicativeExpression)*
    ;

multiplicativeExpression
    : typeRHS (multiplicativeOperation NL* typeRHS)*
    ;

typeRHS
    : prefixUnaryExpression (NL* typeOperation prefixUnaryExpression)*
    ;

prefixUnaryExpression
    : prefixUnaryOperation* postfixUnaryExpression
    ;

postfixUnaryExpression
    : (atomicExpression | callableReference) postfixUnaryOperation*
    ;

atomicExpression
    : parenthesizedExpression
    | literalConstant
    | functionLiteral
    | thisExpression        // THIS labelReference?
    | superExpression       // SUPER (LANGLE type RANGLE)? labelReference?
    | conditionalExpression // ifExpression, whenExpression
    | tryExpression
    | objectLiteral
    | jumpExpression
    | loopExpression
    | collectionLiteral
    | simpleIdentifier
    | VAL identifier
    ;

parenthesizedExpression
    : LPAREN expression RPAREN
    ;

callSuffix
    : typeArguments valueArguments? annotatedLambda*
    | valueArguments annotatedLambda*
    | annotatedLambda+
    ;

annotatedLambda
    : unescapedAnnotation* LabelDefinition? NL* functionLiteral
    ;

arrayAccess
    : LSQUARE (expression (COMMA expression)*)? RSQUARE
    ;

valueArguments
    : LPAREN (valueArgument (COMMA valueArgument)* (NL* COMMA)?)? RPAREN
    ;

typeArguments
    : LANGLE NL* typeProjection (NL* COMMA typeProjection)* (NL* COMMA)? NL* RANGLE QUEST?
    ;

typeProjection
    : typeProjectionModifierList? type
    | MULT
    ;

typeProjectionModifierList
    : varianceAnnotation+
    ;

valueArgument
    : (simpleIdentifier NL* ASSIGNMENT NL*)? MULT? NL* expression
    ;

literalConstant
    : BooleanLiteral
    | IntegerLiteral
    | stringLiteral
    | HexLiteral
    | BinLiteral
    | CharacterLiteral
    | RealLiteral
    | NullLiteral
    | LongLiteral
    ;

stringLiteral
    : lineStringLiteral
    | multiLineStringLiteral
    ;

lineStringLiteral
    : QUOTE_OPEN (lineStringContent | lineStringExpression)* QUOTE_CLOSE
    ;

multiLineStringLiteral
    : TRIPLE_QUOTE_OPEN (
        multiLineStringContent
        | multiLineStringExpression
        | lineStringLiteral
        | MultiLineStringQuote
    )* TRIPLE_QUOTE_CLOSE
    ;

lineStringContent
    : LineStrText
    | LineStrEscapedChar
    | LineStrRef
    ;

lineStringExpression
    : LineStrExprStart expression RCURL
    ;

multiLineStringContent
    : MultiLineStrText
    | MultiLineStrEscapedChar
    | MultiLineStrRef
    ;

multiLineStringExpression
    : MultiLineStrExprStart expression RCURL
    ;

functionLiteral
    : annotations* (
        LCURL NL* statements NL* RCURL
        | LCURL NL* lambdaParameters NL* ARROW NL* statements NL* RCURL
    )
    ;

lambdaParameters
    : lambdaParameter? (NL* COMMA NL* lambdaParameter)*
    ;

lambdaParameter
    : variableDeclaration
    | multiVariableDeclaration (NL* COLON NL* type)?
    ;

// https://kotlinlang.org/docs/reference/grammar.html#objectLiteral
objectLiteral
    : OBJECT (NL* COLON NL* delegationSpecifiers)? NL* classBody?
    ;

collectionLiteral
    : LSQUARE expression? (COMMA expression)* RSQUARE
    ;

thisExpression
    : THIS LabelReference?
    ;

superExpression
    : SUPER (LANGLE NL* type NL* RANGLE)? LabelReference?
    ;

conditionalExpression
    : ifExpression
    | whenExpression
    ;

ifExpression
    : IF NL* LPAREN expression RPAREN NL* controlStructureBody? SEMICOLON? (
        NL* ELSE NL* controlStructureBody?
    )?
    ;

controlStructureBody
    : block
    | expression
    ;

whenExpression
    : WHEN NL* (LPAREN expression RPAREN)? NL* LCURL NL* (whenEntry NL*)* NL* RCURL
    ;

whenEntry
    : whenCondition (NL* COMMA NL* whenCondition)* NL* ARROW NL* controlStructureBody semi?
    | ELSE NL* ARROW NL* controlStructureBody
    ;

whenCondition
    : expression
    | rangeTest
    | typeTest
    ;

rangeTest
    : inOperator NL* expression
    ;

typeTest
    : isOperator NL* type
    ;

tryExpression
    : TRY NL* block (NL* catchBlock)* (NL* finallyBlock)?
    ;

catchBlock
    : CATCH NL* LPAREN annotations* simpleIdentifier COLON userType RPAREN NL* block
    ;

finallyBlock
    : FINALLY NL* block
    ;

loopExpression
    : forExpression
    | whileExpression
    | doWhileExpression
    ;

forExpression
    : FOR NL* LPAREN annotations* (variableDeclaration | multiVariableDeclaration) IN expression RPAREN NL* controlStructureBody?
    ;

whileExpression
    : WHILE NL* LPAREN expression RPAREN NL* controlStructureBody?
    ;

doWhileExpression
    : DO NL* controlStructureBody? NL* WHILE NL* LPAREN expression RPAREN
    ;

jumpExpression
    : THROW NL* expression
    | (RETURN | RETURN_AT) expression?
    | CONTINUE
    | CONTINUE_AT
    | BREAK
    | BREAK_AT
    ;

callableReference
    : (userType (QUEST NL*)*)? NL* (COLONCOLON | Q_COLONCOLON) NL* (identifier | CLASS)
    | THIS NL* COLONCOLON NL* CLASS
    ;

assignmentOperator
    : ASSIGNMENT
    | ADD_ASSIGNMENT
    | SUB_ASSIGNMENT
    | MULT_ASSIGNMENT
    | DIV_ASSIGNMENT
    | MOD_ASSIGNMENT
    ;

equalityOperation
    : EXCL_EQ
    | EXCL_EQEQ
    | EQEQ
    | EQEQEQ
    ;

comparisonOperator
    : LANGLE
    | RANGLE
    | LE
    | GE
    ;

inOperator
    : IN
    | NOT_IN
    ;

isOperator
    : IS
    | NOT_IS
    ;

additiveOperator
    : ADD
    | SUB
    ;

multiplicativeOperation
    : MULT
    | DIV
    | MOD
    ;

typeOperation
    : AS
    | AS_SAFE
    | COLON
    ;

prefixUnaryOperation
    : INCR
    | DECR
    | ADD
    | SUB
    | EXCL
    | annotations
    | labelDefinition
    ;

postfixUnaryOperation
    : INCR
    | DECR
    | EXCL EXCL
    | callSuffix
    | arrayAccess
    | NL* memberAccessOperator postfixUnaryExpression
    ;

memberAccessOperator
    : DOT
    | QUEST DOT
    ;

modifierList
    : (annotations | modifier)+
    ;

modifier
    : (
        classModifier
        | memberModifier
        | visibilityModifier
        | varianceAnnotation
        | functionModifier
        | propertyModifier
        | inheritanceModifier
        | parameterModifier
        | typeParameterModifier
    ) NL*
    ;

classModifier
    : ENUM
    | SEALED
    | ANNOTATION
    | DATA
    | INNER
    ;

memberModifier
    : OVERRIDE
    | LATEINIT
    ;

visibilityModifier
    : PUBLIC
    | PRIVATE
    | INTERNAL
    | PROTECTED
    ;

varianceAnnotation
    : IN
    | OUT
    ;

functionModifier
    : TAILREC
    | OPERATOR
    | INFIX
    | INLINE
    | EXTERNAL
    | SUSPEND
    ;

propertyModifier
    : CONST
    ;

inheritanceModifier
    : ABSTRACT
    | FINAL
    | OPEN
    ;

parameterModifier
    : VARARG
    | NOINLINE
    | CROSSINLINE
    ;

typeParameterModifier
    : REIFIED
    ;

labelDefinition
    : LabelDefinition NL*
    ;

annotations
    : (annotation | annotationList) NL*
    ;

annotation
    : annotationUseSiteTarget NL* COLON NL* unescapedAnnotation
    | LabelReference (NL* DOT NL* simpleIdentifier)* (NL* typeArguments)? (NL* valueArguments)?
    ;

annotationList
    : annotationUseSiteTarget COLON LSQUARE unescapedAnnotation+ RSQUARE
    | AT LSQUARE unescapedAnnotation+ RSQUARE
    ;

annotationUseSiteTarget
    : FIELD
    | FILE
    | PROPERTY
    | GET
    | SET
    | RECEIVER
    | PARAM
    | SETPARAM
    | DELEGATE
    ;

unescapedAnnotation
    : identifier typeArguments? valueArguments?
    ;

identifier
    : simpleIdentifier (NL* DOT simpleIdentifier)*
    ;

simpleIdentifier
    : Identifier
    //soft keywords:
    | ABSTRACT
    | ANNOTATION
    | BY
    | CATCH
    | COMPANION
    | CONSTRUCTOR
    | CROSSINLINE
    | DATA
    | DYNAMIC
    | ENUM
    | EXTERNAL
    | FINAL
    | FINALLY
    | GETTER
    | IMPORT
    | INFIX
    | INIT
    | INLINE
    | INNER
    | INTERNAL
    | LATEINIT
    | NOINLINE
    | OPEN
    | OPERATOR
    | OUT
    | OVERRIDE
    | PRIVATE
    | PROTECTED
    | PUBLIC
    | REIFIED
    | SEALED
    | TAILREC
    | SETTER
    | VARARG
    | WHERE
    //strong keywords
    | CONST
    | SUSPEND
    ;

semi
    : NL+
    | NL* SEMICOLON NL*
    ;

anysemi
    : NL
    | SEMICOLON
    ;