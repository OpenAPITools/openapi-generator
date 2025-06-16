/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 by Bart Kiers (original author) and Alexandre Vitorelli (contributor -> ported to CSharp)
 * Copyright (c) 2017 by Ivan Kochurkin (Positive Technologies):
    added ECMAScript 6 support, cleared and transformed to the universal grammar.
 * Copyright (c) 2018 by Juan Alvarez (contributor -> ported to Go)
 * Copyright (c) 2019 by Andrii Artiushok (contributor -> added TypeScript support)
 * Copyright (c) 2024 by Andrew Leppard (www.wegrok.review)
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar TypeScriptParser;

options {
    tokenVocab = TypeScriptLexer;
    superClass = TypeScriptParserBase;
}

// SupportSyntax

initializer
    : '=' singleExpression
    ;

bindingPattern
    : (arrayLiteral | objectLiteral)
    ;

// TypeScript SPart
// A.1 Types
typeParameters
    : '<' typeParameterList? '>'
    ;

typeParameterList
    : typeParameter (',' typeParameter)*
    ;

typeParameter
    : identifier constraint?
    | identifier '=' typeArgument
    | typeParameters
    ;

constraint
    : 'extends' type_
    ;

typeArguments
    : '<' typeArgumentList? '>'
    ;

typeArgumentList
    : typeArgument (',' typeArgument)*
    ;

typeArgument
    : type_
    ;

// Union and intersection types can have a leading '|' or '&'
// See https://github.com/microsoft/TypeScript/pull/12386
type_
    : ('|' | '&')? unionOrIntersectionOrPrimaryType
    | functionType
    | constructorType
    | typeGeneric
    ;

unionOrIntersectionOrPrimaryType
    : unionOrIntersectionOrPrimaryType '|' unionOrIntersectionOrPrimaryType # Union
    | unionOrIntersectionOrPrimaryType '&' unionOrIntersectionOrPrimaryType # Intersection
    | primaryType                                                           # Primary
    ;

primaryType
    : '(' type_ ')'                              # ParenthesizedPrimType
    | predefinedType                             # PredefinedPrimType
    | typeReference                              # ReferencePrimType
    | objectType                                 # ObjectPrimType
    | primaryType {this.notLineTerminator()}? '[' primaryType? ']' # ArrayPrimType
    | '[' tupleElementTypes ']'                  # TuplePrimType
    | typeQuery                                  # QueryPrimType
    | This                                       # ThisPrimType
    | typeReference Is primaryType               # RedefinitionOfType
    | KeyOf primaryType                          # KeyOfType
    ;

predefinedType
    : Any
    | NullLiteral
    | Number
    | DecimalLiteral
    | Boolean
    | BooleanLiteral
    | String
    | StringLiteral
    | Unique? Symbol
    | Never
    | Undefined
    | Object
    | Void
    ;

typeReference
    : typeName typeGeneric?
    ;

typeGeneric
    : '<' typeArgumentList typeGeneric?'>'
    ;

typeName
    : identifier
    | namespaceName
    ;

objectType
    : '{' typeBody? '}'
    ;

typeBody
    : typeMemberList (SemiColon | ',')?
    ;

typeMemberList
    : typeMember ((SemiColon | ',') typeMember)*
    ;

typeMember
    : propertySignatur
    | callSignature
    | constructSignature
    | indexSignature
    | methodSignature ('=>' type_)?
    ;

arrayType
    : primaryType {this.notLineTerminator()}? '[' ']'
    ;

tupleType
    : '[' tupleElementTypes ']'
    ;

// Tuples can have a trailing comma. See https://github.com/Microsoft/TypeScript/issues/28893
tupleElementTypes
    : type_ (',' type_)* ','?
    ;

functionType
    : typeParameters? '(' parameterList? ')' '=>' type_
    ;

constructorType
    : 'new' typeParameters? '(' parameterList? ')' '=>' type_
    ;

typeQuery
    : 'typeof' typeQueryExpression
    ;

typeQueryExpression
    : identifier
    | (identifierName '.')+ identifierName
    ;

propertySignatur
    : ReadOnly? propertyName '?'? typeAnnotation? ('=>' type_)?
    ;

typeAnnotation
    : ':' type_
    ;

callSignature
    : typeParameters? '(' parameterList? ')' typeAnnotation?
    ;

// Function parameter list can have a trailing comma.
// See https://github.com/Microsoft/TypeScript/issues/16152
parameterList
    : restParameter
    | parameter (',' parameter)* (',' restParameter)? ','?
    ;

requiredParameterList
    : requiredParameter (',' requiredParameter)*
    ;

parameter
    : requiredParameter
    | optionalParameter
    ;

optionalParameter
    : decoratorList? (
        accessibilityModifier? identifierOrPattern (
            '?' typeAnnotation?
            | typeAnnotation? initializer
        )
    )
    ;

restParameter
    : '...' singleExpression typeAnnotation?
    ;

requiredParameter
    : decoratorList? accessibilityModifier? identifierOrPattern typeAnnotation?
    ;

accessibilityModifier
    : Public
    | Private
    | Protected
    ;

identifierOrPattern
    : identifierName
    | bindingPattern
    ;

constructSignature
    : 'new' typeParameters? '(' parameterList? ')' typeAnnotation?
    ;

indexSignature
    : '[' identifier ':' (Number | String) ']' typeAnnotation
    ;

methodSignature
    : propertyName '?'? callSignature
    ;

typeAliasDeclaration
    : Export? 'type' identifier typeParameters? '=' type_ eos
    ;

constructorDeclaration
    : accessibilityModifier? Constructor '(' formalParameterList? ')' (
        ('{' functionBody '}')
        | SemiColon
    )?
    ;

// A.5 Interface

interfaceDeclaration
    : Export? Declare? Interface identifier typeParameters? interfaceExtendsClause? objectType SemiColon?
    ;

interfaceExtendsClause
    : Extends classOrInterfaceTypeList
    ;

classOrInterfaceTypeList
    : typeReference (',' typeReference)*
    ;

// A.7 Interface

enumDeclaration
    : Const? Enum identifier '{' enumBody? '}'
    ;

enumBody
    : enumMemberList ','?
    ;

enumMemberList
    : enumMember (',' enumMember)*
    ;

enumMember
    : propertyName ('=' singleExpression)?
    ;

// A.8 Namespaces

namespaceDeclaration
    : Declare? Namespace namespaceName '{' statementList? '}'
    ;

namespaceName
    : identifier ('.'+ identifier)*
    ;

importAliasDeclaration
    : identifier '=' namespaceName SemiColon
    ;

// Ext.2 Additions to 1.8: Decorators

decoratorList
    : decorator+
    ;

decorator
    : '@' (decoratorMemberExpression | decoratorCallExpression)
    ;

decoratorMemberExpression
    : identifier
    | decoratorMemberExpression '.' identifierName
    | '(' singleExpression ')'
    ;

decoratorCallExpression
    : decoratorMemberExpression arguments
    ;

// ECMAPart
program
    : sourceElements? EOF
    ;

sourceElement
    : Export? statement
    ;

statement
    : block
    | variableStatement
    | importStatement
    | exportStatement
    | emptyStatement_
    | abstractDeclaration //ADDED
    | classDeclaration
    | functionDeclaration
    | expressionStatement
    | interfaceDeclaration //ADDED
    | namespaceDeclaration //ADDED
    | ifStatement
    | iterationStatement
    | continueStatement
    | breakStatement
    | returnStatement
    | yieldStatement
    | withStatement
    | labelledStatement
    | switchStatement
    | throwStatement
    | tryStatement
    | debuggerStatement
    | arrowFunctionDeclaration
    | generatorFunctionDeclaration
    | typeAliasDeclaration //ADDED
    | enumDeclaration      //ADDED
    | Export statement
    ;

block
    : '{' statementList? '}'
    ;

statementList
    : statement+
    ;

abstractDeclaration
    : Abstract (identifier callSignature | variableStatement) eos
    ;

importStatement
    : Import importFromBlock
    ;

importFromBlock
    : importDefault? (importNamespace | importModuleItems) importFrom eos
    | StringLiteral eos
    ;

importModuleItems
    : '{' (importAliasName ',')* (importAliasName ','?)? '}'
    ;

importAliasName
    : moduleExportName (As importedBinding)?
    ;

moduleExportName
    : identifierName
    | StringLiteral
    ;

// yield and await are permitted as BindingIdentifier in the grammar
importedBinding
    : Identifier
    | Yield
    | Await
    ;

importDefault
    : aliasName ','
    ;

importNamespace
    : ('*' | identifierName) (As identifierName)?
    ;

importFrom
    : From StringLiteral
    ;

aliasName
    : identifierName (As identifierName)?
    ;

exportStatement
    : Export Default? (exportFromBlock | declaration) eos # ExportDeclaration
    | Export Default singleExpression eos                 # ExportDefaultDeclaration
    ;

exportFromBlock
    : importNamespace importFrom eos
    | exportModuleItems importFrom? eos
    ;

exportModuleItems
    : '{' (exportAliasName ',')* (exportAliasName ','?)? '}'
    ;

exportAliasName
    : moduleExportName (As moduleExportName)?
    ;

declaration
    : variableStatement
    | classDeclaration
    | functionDeclaration
    ;

variableStatement
    : bindingPattern typeAnnotation? initializer SemiColon?
    | accessibilityModifier? varModifier? ReadOnly? variableDeclarationList SemiColon?
    | Declare varModifier? variableDeclarationList SemiColon?
    ;

variableDeclarationList
    : variableDeclaration (',' variableDeclaration)*
    ;

variableDeclaration
    : (identifierOrKeyWord | arrayLiteral | objectLiteral) typeAnnotation? singleExpression? (
        '=' typeParameters? singleExpression
    )? // ECMAScript 6: Array & Object Matching
    ;

emptyStatement_
    : SemiColon
    ;

expressionStatement
    : {this.notOpenBraceAndNotFunctionAndNotInterface()}? expressionSequence SemiColon?
    ;

ifStatement
    : If '(' expressionSequence ')' statement (Else statement)?
    ;

iterationStatement
    : Do statement While '(' expressionSequence ')' eos                                                                     # DoStatement
    | While '(' expressionSequence ')' statement                                                                            # WhileStatement
    | For '(' expressionSequence? SemiColon expressionSequence? SemiColon expressionSequence? ')' statement                 # ForStatement
    | For '(' varModifier variableDeclarationList SemiColon expressionSequence? SemiColon expressionSequence? ')' statement # ForVarStatement
    | For '(' singleExpression In expressionSequence ')' statement                                                          # ForInStatement
    | For '(' varModifier variableDeclaration In expressionSequence ')' statement                                           # ForVarInStatement
    | For Await? '(' singleExpression identifier {this.p("of")}? expressionSequence (As type_)? ')' statement                            # ForOfStatement
    | For Await? '(' varModifier variableDeclaration identifier {this.p("of")}? expressionSequence (As type_)? ')' statement             # ForVarOfStatement
    ;

varModifier
    : Var
    | Let
    | Const
    ;

continueStatement
    : Continue ({this.notLineTerminator()}? identifier)? eos
    ;

breakStatement
    : Break ({this.notLineTerminator()}? identifier)? eos
    ;

returnStatement
    : Return ({this.notLineTerminator()}? expressionSequence)? eos
    ;

yieldStatement
    : (Yield | YieldStar) ({this.notLineTerminator()}? expressionSequence)? eos
    ;

withStatement
    : With '(' expressionSequence ')' statement
    ;

switchStatement
    : Switch '(' expressionSequence ')' caseBlock
    ;

caseBlock
    : '{' caseClauses? (defaultClause caseClauses?)? '}'
    ;

caseClauses
    : caseClause+
    ;

caseClause
    : Case expressionSequence ':' statementList?
    ;

defaultClause
    : Default ':' statementList?
    ;

labelledStatement
    : identifier ':' statement
    ;

throwStatement
    : Throw {this.notLineTerminator()}? expressionSequence eos
    ;

tryStatement
    : Try block (catchProduction finallyProduction? | finallyProduction)
    ;

catchProduction
    : Catch ('(' identifier typeAnnotation? ')')? block
    ;

finallyProduction
    : Finally block
    ;

debuggerStatement
    : Debugger eos
    ;

functionDeclaration
    : Async? Function_ '*'? identifier callSignature (('{' functionBody '}') | SemiColon)
    ;

//Ovveride ECMA
classDeclaration
    : decoratorList? (Export Default?)? Abstract? Class identifier typeParameters? classHeritage classTail
    ;

classHeritage
    : classExtendsClause? implementsClause?
    ;

classTail
    : '{' classElement* '}'
    ;

classExtendsClause
    : Extends typeReference
    ;

implementsClause
    : Implements classOrInterfaceTypeList
    ;

// Classes modified
classElement
    : constructorDeclaration
    | decoratorList? propertyMemberDeclaration
    | indexMemberDeclaration
    | statement
    ;

propertyMemberDeclaration
    : propertyMemberBase propertyName '?'? typeAnnotation? initializer? SemiColon        # PropertyDeclarationExpression
    | propertyMemberBase propertyName callSignature (('{' functionBody '}') | SemiColon) # MethodDeclarationExpression
    | propertyMemberBase (getAccessor | setAccessor)                                     # GetterSetterDeclarationExpression
    | abstractDeclaration                                                                # AbstractMemberDeclaration
    ;

propertyMemberBase
    : accessibilityModifier? Async? Static? ReadOnly?
    ;

indexMemberDeclaration
    : indexSignature SemiColon
    ;

generatorMethod
    : (Async {this.notLineTerminator()}?)? '*'? propertyName '(' formalParameterList? ')' '{' functionBody '}'
    ;

generatorFunctionDeclaration
    : Async? Function_ '*' identifier? '(' formalParameterList? ')' '{' functionBody '}'
    ;

generatorBlock
    : '{' generatorDefinition (',' generatorDefinition)* ','? '}'
    ;

generatorDefinition
    : '*' iteratorDefinition
    ;

iteratorBlock
    : '{' iteratorDefinition (',' iteratorDefinition)* ','? '}'
    ;

iteratorDefinition
    : '[' singleExpression ']' '(' formalParameterList? ')' '{' functionBody '}'
    ;

classElementName
    : propertyName
    | privateIdentifier
    ;

privateIdentifier
    : '#' identifierName
    ;

formalParameterList
    : formalParameterArg (',' formalParameterArg)* (',' lastFormalParameterArg)? ','?
    | lastFormalParameterArg
    | arrayLiteral                             // ECMAScript 6: Parameter Context Matching
    | objectLiteral (':' formalParameterList)? // ECMAScript 6: Parameter Context Matching
    ;

formalParameterArg
    : decorator? accessibilityModifier? assignable '?'? typeAnnotation? (
        '=' singleExpression
    )? // ECMAScript 6: Initialization
    ;

lastFormalParameterArg // ECMAScript 6: Rest Parameter
    : Ellipsis identifier typeAnnotation?
    ;

functionBody
    : sourceElements?
    ;

sourceElements
    : sourceElement+
    ;

arrayLiteral
    : ('[' elementList ']')
    ;

// JavaScript supports arrasys like [,,1,2,,].
elementList
    : ','* arrayElement? (','+ arrayElement) * ','* // Yes, everything is optional
    ;

arrayElement // ECMAScript 6: Spread Operator
    : Ellipsis? (singleExpression | identifier) ','?
    ;

objectLiteral
    : '{' (propertyAssignment (',' propertyAssignment)* ','?)? '}'
    ;

// MODIFIED
propertyAssignment
    : propertyName (':' | '=') singleExpression     # PropertyExpressionAssignment
    | '[' singleExpression ']' ':' singleExpression # ComputedPropertyExpressionAssignment
    | getAccessor                                   # PropertyGetter
    | setAccessor                                   # PropertySetter
    | generatorMethod                               # MethodProperty
    | identifierOrKeyWord                           # PropertyShorthand
    | Ellipsis? singleExpression                    # SpreadOperator
    | restParameter                                 # RestParameterInObject
    ;

getAccessor
    : getter '(' ')' typeAnnotation? '{' functionBody '}'
    ;

setAccessor
    : setter '(' formalParameterList? ')' '{' functionBody '}'
    ;

propertyName
    : identifierName
    | StringLiteral
    | numericLiteral
    | '[' singleExpression ']'
    ;

arguments
    : '(' (argumentList ','?)? ')'
    ;

argumentList
    : argument (',' argument)*
    ;

argument // ECMAScript 6: Spread Operator
    : Ellipsis? (singleExpression | identifier)
    ;

expressionSequence
    : singleExpression (',' singleExpression)*
    ;

singleExpression
    : anonymousFunction                                           # FunctionExpression
    | Class identifier? typeParameters? classHeritage classTail   # ClassExpression
    | singleExpression '?.'? '[' expressionSequence ']'           # MemberIndexExpression
    | singleExpression '?.' singleExpression                      # OptionalChainExpression
    | singleExpression '!'? '.' '#'? identifierName typeGeneric?  # MemberDotExpression
    | singleExpression '?'? '.' '#'? identifierName typeGeneric?  # MemberDotExpression
    // Split to try `new Date()` first, then `new Date`.
    | New singleExpression typeArguments? arguments                   # NewExpression
    | New singleExpression typeArguments?                             # NewExpression
    | singleExpression arguments                                      # ArgumentsExpression
    | singleExpression {this.notLineTerminator()}? '++'               # PostIncrementExpression
    | singleExpression {this.notLineTerminator()}? '--'               # PostDecreaseExpression
    | Delete singleExpression                                         # DeleteExpression
    | Void singleExpression                                           # VoidExpression
    | Typeof singleExpression                                         # TypeofExpression
    | '++' singleExpression                                           # PreIncrementExpression
    | '--' singleExpression                                           # PreDecreaseExpression
    | '+' singleExpression                                            # UnaryPlusExpression
    | '-' singleExpression                                            # UnaryMinusExpression
    | '~' singleExpression                                            # BitNotExpression
    | '!' singleExpression                                            # NotExpression
    | Await singleExpression                                          # AwaitExpression
    | <assoc = right> singleExpression '**' singleExpression          # PowerExpression
    | singleExpression ('*' | '/' | '%') singleExpression             # MultiplicativeExpression
    | singleExpression ('+' | '-') singleExpression                   # AdditiveExpression
    | singleExpression '??' singleExpression                          # CoalesceExpression
    | singleExpression ('<<' | '>' '>' | '>' '>' '>') singleExpression # BitShiftExpression
    | singleExpression ('<' | '>' | '<=' | '>=') singleExpression     # RelationalExpression
    | singleExpression Instanceof singleExpression                    # InstanceofExpression
    | singleExpression In singleExpression                            # InExpression
    | singleExpression ('==' | '!=' | '===' | '!==') singleExpression # EqualityExpression
    | singleExpression '&' singleExpression                           # BitAndExpression
    | singleExpression '^' singleExpression                           # BitXOrExpression
    | singleExpression '|' singleExpression                           # BitOrExpression
    | singleExpression '&&' singleExpression                          # LogicalAndExpression
    | singleExpression '||' singleExpression                          # LogicalOrExpression
    | singleExpression '?' singleExpression ':' singleExpression      # TernaryExpression
    | singleExpression '=' singleExpression                           # AssignmentExpression
    | singleExpression assignmentOperator singleExpression            # AssignmentOperatorExpression
    | singleExpression templateStringLiteral                          # TemplateStringExpression     // ECMAScript 6
    | iteratorBlock                                                   # IteratorsExpression          // ECMAScript 6
    | generatorBlock                                                  # GeneratorsExpression         // ECMAScript 6
    | generatorFunctionDeclaration                                    # GeneratorsFunctionExpression // ECMAScript 6
    | yieldStatement                                                  # YieldExpression              // ECMAScript 6
    | This                                                            # ThisExpression
    | identifierName singleExpression?                                # IdentifierExpression
    | Super                                                           # SuperExpression
    | literal                                                         # LiteralExpression
    | arrayLiteral                                                    # ArrayLiteralExpression
    | objectLiteral                                                   # ObjectLiteralExpression
    | '(' expressionSequence ')'                                      # ParenthesizedExpression
    | typeArguments expressionSequence?                               # GenericTypes
    | singleExpression As asExpression                                # CastAsExpression
// TypeScript v2.0
    | singleExpression '!'                                            # NonNullAssertionExpression
    ;

asExpression
    : predefinedType ('[' ']')?
    | singleExpression
    ;

assignable
    : identifier
    | keyword
    | arrayLiteral
    | objectLiteral
    ;

anonymousFunction
    : functionDeclaration
    | Async? Function_ '*'? '(' formalParameterList? ')' typeAnnotation? '{' functionBody '}'
    | arrowFunctionDeclaration
    ;

arrowFunctionDeclaration
    : Async? arrowFunctionParameters typeAnnotation? '=>' arrowFunctionBody
    ;

arrowFunctionParameters
    : propertyName
    | '(' formalParameterList? ')'
    ;

arrowFunctionBody
    : singleExpression
    | '{' functionBody '}'
    ;

assignmentOperator
    : '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '>>>='
    | '&='
    | '^='
    | '|='
    | '**='
    | '??='
    ;

literal
    : NullLiteral
    | BooleanLiteral
    | StringLiteral
    | templateStringLiteral
    | RegularExpressionLiteral
    | numericLiteral
    | bigintLiteral
    ;

templateStringLiteral
    : BackTick templateStringAtom* BackTick
    ;

templateStringAtom
    : TemplateStringAtom
    | TemplateStringStartExpression singleExpression TemplateCloseBrace
    | TemplateStringEscapeAtom
    ;

numericLiteral
    : DecimalLiteral
    | HexIntegerLiteral
    | OctalIntegerLiteral
    | OctalIntegerLiteral2
    | BinaryIntegerLiteral
    ;

bigintLiteral
    : BigDecimalIntegerLiteral
    | BigHexIntegerLiteral
    | BigOctalIntegerLiteral
    | BigBinaryIntegerLiteral
    ;

getter
    : {this.n("get")}? identifier classElementName
    ;

setter
    : {this.n("set")}? identifier classElementName
    ;

identifierName
    : identifier
    | reservedWord
    ;

identifier
    : Identifier
    | Async
    | As
    | From
    | Yield
    | Of
    | Any
    | Any
    | Number
    | Boolean
    | String
    | Unique
    | Symbol
    | Never
    | Undefined
    | Object
    | KeyOf
    | TypeAlias
    | Constructor
    | Namespace
    | Abstract
    ;

identifierOrKeyWord
    : identifier
    | TypeAlias
    | Require
    ;

reservedWord
    : keyword
    | NullLiteral
    | BooleanLiteral
    ;

keyword
    : Break
    | Do
    | Instanceof
    | Typeof
    | Case
    | Else
    | New
    | Var
    | Catch
    | Finally
    | Return
    | Void
    | Continue
    | For
    | Switch
    | While
    | Debugger
    | Function_
    | This
    | With
    | Default
    | If
    | Throw
    | Delete
    | In
    | Try
    | Class
    | Enum
    | Extends
    | Super
    | Const
    | Export
    | Import
    | Implements
    | Let
    | Private
    | Public
    | Interface
    | Package
    | Protected
    | Static
    | Yield
    | Async
    | Await
    | ReadOnly
    | From
    | As
    | Require
    | TypeAlias
    | String
    | Boolean
    | Number
    | Module
    ;

eos
    : SemiColon
    | EOF
    | {this.lineTerminatorAhead()}?
    | {this.closeBrace()}?
    ;