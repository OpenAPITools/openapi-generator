<?php
/**
 * The Tokens class contains weightings for tokens based on their
 * probability of occurrence in a file.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Marc McIntyre <mmcintyre@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

define('T_NONE', 0);
define('T_OPEN_CURLY_BRACKET', 1000);
define('T_CLOSE_CURLY_BRACKET', 1001);
define('T_OPEN_SQUARE_BRACKET', 1002);
define('T_CLOSE_SQUARE_BRACKET', 1003);
define('T_OPEN_PARENTHESIS', 1004);
define('T_CLOSE_PARENTHESIS', 1005);
define('T_COLON', 1006);
define('T_STRING_CONCAT', 1007);
define('T_INLINE_THEN', 1008);
define('T_INLINE_ELSE', 1009);
define('T_NULL', 1010);
define('T_FALSE', 1011);
define('T_TRUE', 1012);
define('T_SEMICOLON', 1013);
define('T_EQUAL', 1014);
define('T_MULTIPLY', 1015);
define('T_DIVIDE', 1016);
define('T_PLUS', 1017);
define('T_MINUS', 1018);
define('T_MODULUS', 1019);
define('T_POWER', 1020);
define('T_BITWISE_AND', 1021);
define('T_BITWISE_OR', 1022);
define('T_ARRAY_HINT', 1023);
define('T_GREATER_THAN', 1024);
define('T_LESS_THAN', 1025);
define('T_BOOLEAN_NOT', 1026);
define('T_SELF', 1027);
define('T_PARENT', 1028);
define('T_DOUBLE_QUOTED_STRING', 1029);
define('T_COMMA', 1030);
define('T_HEREDOC', 1031);
define('T_PROTOTYPE', 1032);
define('T_THIS', 1033);
define('T_REGULAR_EXPRESSION', 1034);
define('T_PROPERTY', 1035);
define('T_LABEL', 1036);
define('T_OBJECT', 1037);
define('T_CLOSE_OBJECT', 1038);
define('T_COLOUR', 1039);
define('T_HASH', 1040);
define('T_URL', 1041);
define('T_STYLE', 1042);
define('T_ASPERAND', 1043);
define('T_DOLLAR', 1044);
define('T_TYPEOF', 1045);
define('T_CLOSURE', 1046);
define('T_BACKTICK', 1047);
define('T_START_NOWDOC', 1048);
define('T_NOWDOC', 1049);
define('T_END_NOWDOC', 1050);
define('T_OPEN_SHORT_ARRAY', 1051);
define('T_CLOSE_SHORT_ARRAY', 1052);
define('T_GOTO_LABEL', 1053);
define('T_BINARY_CAST', 1054);
define('T_EMBEDDED_PHP', 1055);

// Some PHP 5.3 tokens, replicated for lower versions.
if (defined('T_NAMESPACE') === false) {
    define('T_NAMESPACE', 1100);
}

if (defined('T_NS_C') === false) {
    define('T_NS_C', 1101);
}

if (defined('T_NS_SEPARATOR') === false) {
    define('T_NS_SEPARATOR', 1102);
}

if (defined('T_GOTO') === false) {
    define('T_GOTO', 1103);
}

if (defined('T_DIR') === false) {
    define('T_DIR', 1104);
}

// Some PHP 5.4 tokens, replicated for lower versions.
if (defined('T_TRAIT') === false) {
    define('T_TRAIT', 1105);
}

if (defined('T_TRAIT_C') === false) {
    define('T_TRAIT_C', 1106);
}

if (defined('T_INSTEADOF') === false) {
    define('T_INSTEADOF', 1107);
}

if (defined('T_CALLABLE') === false) {
    define('T_CALLABLE', 1108);
}

// Some PHP 5.5 tokens, replicated for lower versions.
if (defined('T_FINALLY') === false) {
    define('T_FINALLY', 1109);
}

if (defined('T_YIELD') === false) {
    define('T_YIELD', 1110);
}

// Some PHP 5.6 tokens, replicated for lower versions.
if (defined('T_ELLIPSIS') === false) {
    define('T_ELLIPSIS', 1111);
}

// Tokens used for parsing doc blocks.
define('T_DOC_COMMENT_STAR', 1200);
define('T_DOC_COMMENT_WHITESPACE', 1201);
define('T_DOC_COMMENT_TAG', 1202);
define('T_DOC_COMMENT_OPEN_TAG', 1203);
define('T_DOC_COMMENT_CLOSE_TAG', 1204);
define('T_DOC_COMMENT_STRING', 1205);

/**
 * The Tokens class contains weightings for tokens based on their
 * probability of occurrence in a file.
 *
 * The less the chance of a high occurrence of an arbitrary token, the higher
 * the weighting.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Marc McIntyre <mmcintyre@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
final class PHP_CodeSniffer_Tokens
{

    /**
     * The token weightings.
     *
     * @var array(int => int)
     */
    public static $weightings = array(
                                 T_CLASS               => 1000,
                                 T_INTERFACE           => 1000,
                                 T_TRAIT               => 1000,
                                 T_NAMESPACE           => 1000,
                                 T_FUNCTION            => 100,
                                 T_CLOSURE             => 100,

                                 /*
                                     Conditions.
                                 */

                                 T_WHILE               => 50,
                                 T_FOR                 => 50,
                                 T_FOREACH             => 50,
                                 T_IF                  => 50,
                                 T_ELSE                => 50,
                                 T_ELSEIF              => 50,
                                 T_DO                  => 50,
                                 T_TRY                 => 50,
                                 T_CATCH               => 50,
                                 T_SWITCH              => 50,

                                 T_SELF                => 25,
                                 T_PARENT              => 25,

                                 /*
                                     Operators and arithmetic.
                                 */

                                 T_BITWISE_AND         => 8,
                                 T_BITWISE_OR          => 8,

                                 T_MULTIPLY            => 5,
                                 T_DIVIDE              => 5,
                                 T_PLUS                => 5,
                                 T_MINUS               => 5,
                                 T_MODULUS             => 5,
                                 T_POWER               => 5,

                                 T_SL                  => 5,
                                 T_SR                  => 5,
                                 T_SL_EQUAL            => 5,
                                 T_SR_EQUAL            => 5,

                                 T_EQUAL               => 5,
                                 T_AND_EQUAL           => 5,
                                 T_CONCAT_EQUAL        => 5,
                                 T_DIV_EQUAL           => 5,
                                 T_MINUS_EQUAL         => 5,
                                 T_MOD_EQUAL           => 5,
                                 T_MUL_EQUAL           => 5,
                                 T_OR_EQUAL            => 5,
                                 T_PLUS_EQUAL          => 5,
                                 T_XOR_EQUAL           => 5,

                                 T_BOOLEAN_AND         => 5,
                                 T_BOOLEAN_OR          => 5,

                                 /*
                                     Equality.
                                 */

                                 T_IS_EQUAL            => 5,
                                 T_IS_NOT_EQUAL        => 5,
                                 T_IS_IDENTICAL        => 5,
                                 T_IS_NOT_IDENTICAL    => 5,
                                 T_IS_SMALLER_OR_EQUAL => 5,
                                 T_IS_GREATER_OR_EQUAL => 5,
                                );

    /**
     * The token weightings.
     *
     * @var array(int => int)
     */
    public static $knownLengths = array(
                                   T_ABSTRACT                 => 8,
                                   T_AND_EQUAL                => 2,
                                   T_ARRAY                    => 5,
                                   T_AS                       => 2,
                                   T_BOOLEAN_AND              => 2,
                                   T_BOOLEAN_OR               => 2,
                                   T_BREAK                    => 5,
                                   T_CALLABLE                 => 8,
                                   T_CASE                     => 4,
                                   T_CATCH                    => 5,
                                   T_CLASS                    => 5,
                                   T_CLASS_C                  => 9,
                                   T_CLONE                    => 5,
                                   T_CONCAT_EQUAL             => 2,
                                   T_CONST                    => 5,
                                   T_CONTINUE                 => 8,
                                   T_CURLY_OPEN               => 2,
                                   T_DEC                      => 2,
                                   T_DECLARE                  => 7,
                                   T_DEFAULT                  => 7,
                                   T_DIR                      => 7,
                                   T_DIV_EQUAL                => 2,
                                   T_DO                       => 2,
                                   T_DOLLAR_OPEN_CURLY_BRACES => 2,
                                   T_DOUBLE_ARROW             => 2,
                                   T_DOUBLE_COLON             => 2,
                                   T_ECHO                     => 4,
                                   T_ELSE                     => 4,
                                   T_ELSEIF                   => 6,
                                   T_EMPTY                    => 5,
                                   T_ENDDECLARE               => 10,
                                   T_ENDFOR                   => 6,
                                   T_ENDFOREACH               => 10,
                                   T_ENDIF                    => 5,
                                   T_ENDSWITCH                => 9,
                                   T_ENDWHILE                 => 8,
                                   T_EVAL                     => 4,
                                   T_EXTENDS                  => 7,
                                   T_FILE                     => 8,
                                   T_FINAL                    => 5,
                                   T_FINALLY                  => 7,
                                   T_FOR                      => 3,
                                   T_FOREACH                  => 7,
                                   T_FUNCTION                 => 8,
                                   T_FUNC_C                   => 12,
                                   T_GLOBAL                   => 6,
                                   T_GOTO                     => 4,
                                   T_HALT_COMPILER            => 15,
                                   T_IF                       => 2,
                                   T_IMPLEMENTS               => 10,
                                   T_INC                      => 2,
                                   T_INCLUDE                  => 7,
                                   T_INCLUDE_ONCE             => 12,
                                   T_INSTANCEOF               => 10,
                                   T_INSTEADOF                => 9,
                                   T_INTERFACE                => 9,
                                   T_ISSET                    => 5,
                                   T_IS_EQUAL                 => 2,
                                   T_IS_GREATER_OR_EQUAL      => 2,
                                   T_IS_IDENTICAL             => 3,
                                   T_IS_NOT_EQUAL             => 2,
                                   T_IS_NOT_IDENTICAL         => 3,
                                   T_IS_SMALLER_OR_EQUAL      => 2,
                                   T_LINE                     => 8,
                                   T_LIST                     => 4,
                                   T_LOGICAL_AND              => 3,
                                   T_LOGICAL_OR               => 2,
                                   T_LOGICAL_XOR              => 3,
                                   T_METHOD_C                 => 10,
                                   T_MINUS_EQUAL              => 2,
                                   T_MOD_EQUAL                => 2,
                                   T_MUL_EQUAL                => 2,
                                   T_NAMESPACE                => 9,
                                   T_NS_C                     => 13,
                                   T_NS_SEPARATOR             => 1,
                                   T_NEW                      => 3,
                                   T_OBJECT_OPERATOR          => 2,
                                   T_OPEN_TAG_WITH_ECHO       => 3,
                                   T_OR_EQUAL                 => 2,
                                   T_PLUS_EQUAL               => 2,
                                   T_PRINT                    => 5,
                                   T_PRIVATE                  => 7,
                                   T_PUBLIC                   => 6,
                                   T_PROTECTED                => 9,
                                   T_REQUIRE                  => 7,
                                   T_REQUIRE_ONCE             => 12,
                                   T_RETURN                   => 6,
                                   T_STATIC                   => 6,
                                   T_SWITCH                   => 6,
                                   T_THROW                    => 5,
                                   T_TRAIT                    => 5,
                                   T_TRAIT_C                  => 9,
                                   T_TRY                      => 3,
                                   T_UNSET                    => 5,
                                   T_USE                      => 3,
                                   T_VAR                      => 3,
                                   T_WHILE                    => 5,
                                   T_XOR_EQUAL                => 2,
                                   T_YIELD                    => 5,
                                   T_OPEN_CURLY_BRACKET       => 1,
                                   T_CLOSE_CURLY_BRACKET      => 1,
                                   T_OPEN_SQUARE_BRACKET      => 1,
                                   T_CLOSE_SQUARE_BRACKET     => 1,
                                   T_OPEN_PARENTHESIS         => 1,
                                   T_CLOSE_PARENTHESIS        => 1,
                                   T_COLON                    => 1,
                                   T_STRING_CONCAT            => 1,
                                   T_INLINE_THEN              => 1,
                                   T_INLINE_ELSE              => 1,
                                   T_NULL                     => 4,
                                   T_FALSE                    => 5,
                                   T_TRUE                     => 4,
                                   T_SEMICOLON                => 1,
                                   T_EQUAL                    => 1,
                                   T_MULTIPLY                 => 1,
                                   T_DIVIDE                   => 1,
                                   T_PLUS                     => 1,
                                   T_MINUS                    => 1,
                                   T_MODULUS                  => 1,
                                   T_POWER                    => 1,
                                   T_BITWISE_AND              => 1,
                                   T_BITWISE_OR               => 1,
                                   T_SL                       => 2,
                                   T_SR                       => 2,
                                   T_SL_EQUAL                 => 3,
                                   T_SR_EQUAL                 => 3,
                                   T_ARRAY_HINT               => 5,
                                   T_GREATER_THAN             => 1,
                                   T_LESS_THAN                => 1,
                                   T_BOOLEAN_NOT              => 1,
                                   T_SELF                     => 4,
                                   T_PARENT                   => 6,
                                   T_COMMA                    => 1,
                                   T_THIS                     => 4,
                                   T_CLOSURE                  => 8,
                                   T_BACKTICK                 => 1,
                                   T_OPEN_SHORT_ARRAY         => 1,
                                   T_CLOSE_SHORT_ARRAY        => 1,
                                  );

    /**
     * Tokens that represent assignments.
     *
     * @var array(int)
     */
    public static $assignmentTokens = array(
                                       T_EQUAL        => T_EQUAL,
                                       T_AND_EQUAL    => T_AND_EQUAL,
                                       T_CONCAT_EQUAL => T_CONCAT_EQUAL,
                                       T_DIV_EQUAL    => T_DIV_EQUAL,
                                       T_MINUS_EQUAL  => T_MINUS_EQUAL,
                                       T_MOD_EQUAL    => T_MOD_EQUAL,
                                       T_MUL_EQUAL    => T_MUL_EQUAL,
                                       T_PLUS_EQUAL   => T_PLUS_EQUAL,
                                       T_XOR_EQUAL    => T_XOR_EQUAL,
                                       T_DOUBLE_ARROW => T_DOUBLE_ARROW,
                                       T_SL_EQUAL     => T_SL_EQUAL,
                                       T_SR_EQUAL     => T_SR_EQUAL,
                                      );

    /**
     * Tokens that represent equality comparisons.
     *
     * @var array(int)
     */
    public static $equalityTokens = array(
                                     T_IS_EQUAL            => T_IS_EQUAL,
                                     T_IS_NOT_EQUAL        => T_IS_NOT_EQUAL,
                                     T_IS_IDENTICAL        => T_IS_IDENTICAL,
                                     T_IS_NOT_IDENTICAL    => T_IS_NOT_IDENTICAL,
                                     T_IS_SMALLER_OR_EQUAL => T_IS_SMALLER_OR_EQUAL,
                                     T_IS_GREATER_OR_EQUAL => T_IS_GREATER_OR_EQUAL,
                                    );

    /**
     * Tokens that represent comparison operator.
     *
     * @var array(int)
     */
    public static $comparisonTokens = array(
                                       T_IS_EQUAL            => T_IS_EQUAL,
                                       T_IS_IDENTICAL        => T_IS_IDENTICAL,
                                       T_IS_NOT_EQUAL        => T_IS_NOT_EQUAL,
                                       T_IS_NOT_IDENTICAL    => T_IS_NOT_IDENTICAL,
                                       T_LESS_THAN           => T_LESS_THAN,
                                       T_GREATER_THAN        => T_GREATER_THAN,
                                       T_IS_SMALLER_OR_EQUAL => T_IS_SMALLER_OR_EQUAL,
                                       T_IS_GREATER_OR_EQUAL => T_IS_GREATER_OR_EQUAL,
                                      );

    /**
     * Tokens that represent arithmetic operators.
     *
     * @var array(int)
     */
    public static $arithmeticTokens = array(
                                       T_PLUS     => T_PLUS,
                                       T_MINUS    => T_MINUS,
                                       T_MULTIPLY => T_MULTIPLY,
                                       T_DIVIDE   => T_DIVIDE,
                                       T_MODULUS  => T_MODULUS,
                                      );

    /**
     * Tokens that represent casting.
     *
     * @var array(int)
     */
    public static $castTokens = array(
                                 T_INT_CAST    => T_INT_CAST,
                                 T_STRING_CAST => T_STRING_CAST,
                                 T_DOUBLE_CAST => T_DOUBLE_CAST,
                                 T_ARRAY_CAST  => T_ARRAY_CAST,
                                 T_BOOL_CAST   => T_BOOL_CAST,
                                 T_OBJECT_CAST => T_OBJECT_CAST,
                                 T_UNSET_CAST  => T_UNSET_CAST,
                                 T_BINARY_CAST => T_BINARY_CAST,
                                );

    /**
     * Token types that open parenthesis.
     *
     * @var array(int)
     */
    public static $parenthesisOpeners = array(
                                         T_ARRAY    => T_ARRAY,
                                         T_FUNCTION => T_FUNCTION,
                                         T_CLOSURE  => T_CLOSURE,
                                         T_WHILE    => T_WHILE,
                                         T_FOR      => T_FOR,
                                         T_FOREACH  => T_FOREACH,
                                         T_SWITCH   => T_SWITCH,
                                         T_IF       => T_IF,
                                         T_ELSEIF   => T_ELSEIF,
                                         T_CATCH    => T_CATCH,
                                        );

    /**
     * Tokens that are allowed to open scopes.
     *
     * @var array(int)
     */
    public static $scopeOpeners = array(
                                   T_CLASS     => T_CLASS,
                                   T_INTERFACE => T_INTERFACE,
                                   T_TRAIT     => T_TRAIT,
                                   T_NAMESPACE => T_NAMESPACE,
                                   T_FUNCTION  => T_FUNCTION,
                                   T_CLOSURE   => T_CLOSURE,
                                   T_IF        => T_IF,
                                   T_SWITCH    => T_SWITCH,
                                   T_CASE      => T_CASE,
                                   T_DEFAULT   => T_DEFAULT,
                                   T_WHILE     => T_WHILE,
                                   T_ELSE      => T_ELSE,
                                   T_ELSEIF    => T_ELSEIF,
                                   T_FOR       => T_FOR,
                                   T_FOREACH   => T_FOREACH,
                                   T_DO        => T_DO,
                                   T_TRY       => T_TRY,
                                   T_CATCH     => T_CATCH,
                                   T_PROPERTY  => T_PROPERTY,
                                   T_OBJECT    => T_OBJECT,
                                   T_USE       => T_USE,
                                  );

    /**
     * Tokens that represent scope modifiers.
     *
     * @var array(int)
     */
    public static $scopeModifiers = array(
                                     T_PRIVATE   => T_PRIVATE,
                                     T_PUBLIC    => T_PUBLIC,
                                     T_PROTECTED => T_PROTECTED,
                                    );

    /**
     * Tokens that can prefix a method name
     *
     * @var array(int)
     */
    public static $methodPrefixes = array(
                                     T_PRIVATE   => T_PRIVATE,
                                     T_PUBLIC    => T_PUBLIC,
                                     T_PROTECTED => T_PROTECTED,
                                     T_ABSTRACT  => T_ABSTRACT,
                                     T_STATIC    => T_STATIC,
                                     T_FINAL     => T_FINAL,
                                    );

    /**
     * Tokens that perform operations.
     *
     * @var array(int)
     */
    public static $operators = array(
                                T_MINUS       => T_MINUS,
                                T_PLUS        => T_PLUS,
                                T_MULTIPLY    => T_MULTIPLY,
                                T_DIVIDE      => T_DIVIDE,
                                T_MODULUS     => T_MODULUS,
                                T_POWER       => T_POWER,
                                T_BITWISE_AND => T_BITWISE_AND,
                                T_BITWISE_OR  => T_BITWISE_OR,
                                T_SL          => T_SL,
                                T_SR          => T_SR,
                               );

    /**
     * Tokens that perform boolean operations.
     *
     * @var array(int)
     */
    public static $booleanOperators = array(
                                       T_BOOLEAN_AND => T_BOOLEAN_AND,
                                       T_BOOLEAN_OR  => T_BOOLEAN_OR,
                                       T_LOGICAL_AND => T_LOGICAL_AND,
                                       T_LOGICAL_OR  => T_LOGICAL_OR,
                                       T_LOGICAL_XOR => T_LOGICAL_XOR,
                                      );

    /**
     * Tokens that open code blocks.
     *
     * @var array(int)
     */
    public static $blockOpeners = array(
                                   T_OPEN_CURLY_BRACKET  => T_OPEN_CURLY_BRACKET,
                                   T_OPEN_SQUARE_BRACKET => T_OPEN_SQUARE_BRACKET,
                                   T_OPEN_PARENTHESIS    => T_OPEN_PARENTHESIS,
                                   T_OBJECT              => T_OBJECT,
                                  );

    /**
     * Tokens that don't represent code.
     *
     * @var array(int)
     */
    public static $emptyTokens = array(
                                  T_WHITESPACE             => T_WHITESPACE,
                                  T_COMMENT                => T_COMMENT,
                                  T_DOC_COMMENT            => T_DOC_COMMENT,
                                  T_DOC_COMMENT_STAR       => T_DOC_COMMENT_STAR,
                                  T_DOC_COMMENT_WHITESPACE => T_DOC_COMMENT_WHITESPACE,
                                  T_DOC_COMMENT_TAG        => T_DOC_COMMENT_TAG,
                                  T_DOC_COMMENT_OPEN_TAG   => T_DOC_COMMENT_OPEN_TAG,
                                  T_DOC_COMMENT_CLOSE_TAG  => T_DOC_COMMENT_CLOSE_TAG,
                                  T_DOC_COMMENT_STRING     => T_DOC_COMMENT_STRING,
                                 );

    /**
     * Tokens that are comments.
     *
     * @var array(int)
     */
    public static $commentTokens = array(
                                    T_COMMENT                => T_COMMENT,
                                    T_DOC_COMMENT            => T_DOC_COMMENT,
                                    T_DOC_COMMENT_STAR       => T_DOC_COMMENT_STAR,
                                    T_DOC_COMMENT_WHITESPACE => T_DOC_COMMENT_WHITESPACE,
                                    T_DOC_COMMENT_TAG        => T_DOC_COMMENT_TAG,
                                    T_DOC_COMMENT_OPEN_TAG   => T_DOC_COMMENT_OPEN_TAG,
                                    T_DOC_COMMENT_CLOSE_TAG  => T_DOC_COMMENT_CLOSE_TAG,
                                    T_DOC_COMMENT_STRING     => T_DOC_COMMENT_STRING,
                                   );

    /**
     * Tokens that represent strings.
     *
     * Note that T_STRINGS are NOT represented in this list.
     *
     * @var array(int)
     */
    public static $stringTokens = array(
                                   T_CONSTANT_ENCAPSED_STRING => T_CONSTANT_ENCAPSED_STRING,
                                   T_DOUBLE_QUOTED_STRING     => T_DOUBLE_QUOTED_STRING,
                                  );

    /**
     * Tokens that represent brackets and parenthesis.
     *
     * @var array(int)
     */
    public static $bracketTokens = array(
                                    T_OPEN_CURLY_BRACKET   => T_OPEN_CURLY_BRACKET,
                                    T_CLOSE_CURLY_BRACKET  => T_CLOSE_CURLY_BRACKET,
                                    T_OPEN_SQUARE_BRACKET  => T_OPEN_SQUARE_BRACKET,
                                    T_CLOSE_SQUARE_BRACKET => T_CLOSE_SQUARE_BRACKET,
                                    T_OPEN_PARENTHESIS     => T_OPEN_PARENTHESIS,
                                    T_CLOSE_PARENTHESIS    => T_CLOSE_PARENTHESIS,
                                   );

    /**
     * Tokens that include files.
     *
     * @var array(int)
     */
    public static $includeTokens = array(
                                    T_REQUIRE_ONCE => T_REQUIRE_ONCE,
                                    T_REQUIRE      => T_REQUIRE,
                                    T_INCLUDE_ONCE => T_INCLUDE_ONCE,
                                    T_INCLUDE      => T_INCLUDE,
                                   );

    /**
     * Tokens that make up a heredoc string.
     *
     * @var array(int)
     */
    public static $heredocTokens = array(
                                    T_START_HEREDOC => T_START_HEREDOC,
                                    T_END_HEREDOC   => T_END_HEREDOC,
                                    T_HEREDOC       => T_HEREDOC,
                                    T_START_NOWDOC  => T_START_NOWDOC,
                                    T_END_NOWDOC    => T_END_NOWDOC,
                                    T_NOWDOC        => T_NOWDOC,
                                   );


    /**
     * A PHP_CodeSniffer_Tokens class cannot be constructed.
     *
     * Only static calls are allowed.
     */
    private function __construct()
    {

    }//end __construct()


    /**
     * Returns the highest weighted token type.
     *
     * Tokens are weighted by their approximate frequency of appearance in code
     * - the less frequently they appear in the code, the higher the weighting.
     * For example T_CLASS tokens appear very infrequently in a file, and
     * therefore have a high weighting.
     *
     * Returns false if there are no weightings for any of the specified tokens.
     *
     * @param array(int) $tokens The token types to get the highest weighted
     *                           type for.
     *
     * @return int The highest weighted token.
     */
    public static function getHighestWeightedToken(array $tokens)
    {
        $highest     = -1;
        $highestType = false;

        $weights = self::$weightings;

        foreach ($tokens as $token) {
            if (isset($weights[$token]) === true) {
                $weight = $weights[$token];
            } else {
                $weight = 0;
            }

            if ($weight > $highest) {
                $highest     = $weight;
                $highestType = $token;
            }
        }

        return $highestType;

    }//end getHighestWeightedToken()


}//end class
