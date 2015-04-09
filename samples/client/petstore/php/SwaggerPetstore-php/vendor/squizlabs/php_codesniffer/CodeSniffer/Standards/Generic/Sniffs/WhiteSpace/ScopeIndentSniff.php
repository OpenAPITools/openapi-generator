<?php
/**
 * Generic_Sniffs_Whitespace_ScopeIndentSniff.
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

/**
 * Generic_Sniffs_Whitespace_ScopeIndentSniff.
 *
 * Checks that control structures are structured correctly, and their content
 * is indented correctly. This sniff will throw errors if tabs are used
 * for indentation rather than spaces.
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
class Generic_Sniffs_WhiteSpace_ScopeIndentSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array(
                                   'PHP',
                                   'JS',
                                  );

    /**
     * The number of spaces code should be indented.
     *
     * @var int
     */
    public $indent = 4;

    /**
     * Does the indent need to be exactly right?
     *
     * If TRUE, indent needs to be exactly $indent spaces. If FALSE,
     * indent needs to be at least $indent spaces (but can be more).
     *
     * @var bool
     */
    public $exact = false;

    /**
     * Should tabs be used for indenting?
     *
     * If TRUE, fixes will be made using tabs instead of spaces.
     * The size of each tab is important, so it should be specified
     * using the --tab-width CLI argument.
     *
     * @var bool
     */
    public $tabIndent = false;

    /**
     * The --tab-width CLI value that is being used.
     *
     * @var int
     */
    private $_tabWidth = null;

    /**
     * List of tokens not needing to be checked for indentation.
     *
     * Useful to allow Sniffs based on this to easily ignore/skip some
     * tokens from verification. For example, inline HTML sections
     * or PHP open/close tags can escape from here and have their own
     * rules elsewhere.
     *
     * @var int[]
     */
    public $ignoreIndentationTokens = array();

    /**
     * List of tokens not needing to be checked for indentation.
     *
     * This is a cached copy of the public version of this var, which
     * can be set in a ruleset file, and some core ignored tokens.
     *
     * @var int[]
     */
    private $_ignoreIndentationTokens = array();

    /**
     * Any scope openers that should not cause an indent.
     *
     * @var int[]
     */
    protected $nonIndentingScopes = array();

    /**
     * Show debug output for this sniff.
     *
     * @var bool
     */
    private $_debug = false;


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        if (defined('PHP_CODESNIFFER_IN_TESTS') === true) {
            $this->_debug = false;
        }

        return array(T_OPEN_TAG);

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile All the tokens found in the document.
     * @param int                  $stackPtr  The position of the current token
     *                                        in the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        if ($this->_tabWidth === null) {
            $cliValues = $phpcsFile->phpcs->cli->getCommandLineValues();
            if (isset($cliValues['tabWidth']) === false || $cliValues['tabWidth'] === 0) {
                // We have no idea how wide tabs are, so assume 4 spaces for fixing.
                // It shouldn't really matter because indent checks elsewhere in the
                // standard should fix things up.
                $this->_tabWidth = 4;
            } else {
                $this->_tabWidth = $cliValues['tabWidth'];
            }
        }

        $currentIndent = 0;
        $lastOpenTag   = $stackPtr;
        $lastCloseTag  = null;
        $openScopes    = array();
        $adjustments   = array();

        $tokens        = $phpcsFile->getTokens();
        $currentIndent = ($tokens[$stackPtr]['column'] - 1);

        if (empty($this->_ignoreIndentationTokens) === true) {
            $this->_ignoreIndentationTokens = array(T_INLINE_HTML => true);
            foreach ($this->ignoreIndentationTokens as $token) {
                if (is_int($token) === false) {
                    if (defined($token) === false) {
                        continue;
                    }

                    $token = constant($token);
                }

                $this->_ignoreIndentationTokens[$token] = true;
            }
        }//end if

        $this->exact     = (bool) $this->exact;
        $this->tabIndent = (bool) $this->tabIndent;

        for ($i = ($stackPtr + 1); $i < $phpcsFile->numTokens; $i++) {
            if ($i === false) {
                // Something has gone very wrong; maybe a parse error.
                break;
            }

            $checkToken  = null;
            $checkIndent = null;
            $exact       = $this->exact;

            // Detect line changes and figure out where the indent is.
            if ($tokens[$i]['column'] === 1) {
                $trimmed = ltrim($tokens[$i]['content']);
                if ($trimmed === '') {
                    if (isset($tokens[($i + 1)]) === true
                        && $tokens[$i]['line'] === $tokens[($i + 1)]['line']
                    ) {
                        $checkToken  = ($i + 1);
                        $tokenIndent = ($tokens[($i + 1)]['column'] - 1);
                    }
                } else {
                    $checkToken  = $i;
                    $tokenIndent = (strlen($tokens[$i]['content']) - strlen($trimmed));
                }
            }

            // Closing parenthesis should just be indented to at least
            // the same level as where they were opened (but can be more).
            if ($checkToken !== null
                && $tokens[$checkToken]['code'] === T_CLOSE_PARENTHESIS
            ) {
                if ($this->_debug === true) {
                    $line = $tokens[$i]['line'];
                    echo "Closing parenthesis found on line $line".PHP_EOL;
                }

                $first       = $phpcsFile->findFirstOnLine(T_WHITESPACE, $tokens[$checkToken]['parenthesis_opener'], true);
                $checkIndent = ($tokens[$first]['column'] - 1);
                if (isset($adjustments[$first]) === true) {
                    $checkIndent += $adjustments[$first];
                }

                $exact = false;

                if ($this->_debug === true) {
                    $line = $tokens[$first]['line'];
                    $type = $tokens[$first]['type'];
                    echo "\t* first token on line $line is $type *".PHP_EOL;
                }

                $prev = $phpcsFile->findStartOfStatement($first);
                if ($prev !== $first) {
                    // This is not the start of the statement.
                    if ($this->_debug === true) {
                        $line = $tokens[$prev]['line'];
                        $type = $tokens[$prev]['type'];
                        echo "\t* previous is $type on line $line *".PHP_EOL;
                    }

                    $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $prev, true);
                    $prev  = $phpcsFile->findStartOfStatement($first);
                    $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $prev, true);
                    if ($this->_debug === true) {
                        $line = $tokens[$first]['line'];
                        $type = $tokens[$first]['type'];
                        echo "\t* amended first token is $type on line $line *".PHP_EOL;
                    }
                }

                // Don't force current indent to divisible because there could be custom
                // rules in place between parenthesis, such as with arrays.
                $currentIndent = ($tokens[$first]['column'] - 1);
                if (isset($adjustments[$first]) === true) {
                    $currentIndent += $adjustments[$first];
                }

                if ($this->_debug === true) {
                    echo "\t=> checking indent of $checkIndent; main indent set to $currentIndent".PHP_EOL;
                }
            }//end if

            // Closing parenthesis should just be indented to at least
            // the same level as where they were opened (but can be more).
            if ($checkToken !== null
                && $tokens[$checkToken]['code'] === T_CLOSE_SHORT_ARRAY
            ) {
                if ($this->_debug === true) {
                    $line = $tokens[$i]['line'];
                    echo "Closing short array bracket found on line $line".PHP_EOL;
                }

                $first       = $phpcsFile->findFirstOnLine(T_WHITESPACE, $tokens[$checkToken]['bracket_opener'], true);
                $checkIndent = ($tokens[$first]['column'] - 1);
                if (isset($adjustments[$first]) === true) {
                    $checkIndent += $adjustments[$first];
                }

                $exact = false;

                if ($this->_debug === true) {
                    $line = $tokens[$first]['line'];
                    $type = $tokens[$first]['type'];
                    echo "\t* first token on line $line is $type *".PHP_EOL;
                }

                $prev = $phpcsFile->findStartOfStatement($first);
                if ($prev !== $first) {
                    // This is not the start of the statement.
                    if ($this->_debug === true) {
                        $line = $tokens[$prev]['line'];
                        $type = $tokens[$prev]['type'];
                        echo "\t* previous is $type on line $line *".PHP_EOL;
                    }

                    $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $prev, true);
                    $prev  = $phpcsFile->findStartOfStatement($first);
                    $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $prev, true);
                    if ($this->_debug === true) {
                        $line = $tokens[$first]['line'];
                        $type = $tokens[$first]['type'];
                        echo "\t* amended first token is $type on line $line *".PHP_EOL;
                    }
                }

                // Don't force current indent to be divisible because there could be custom
                // rules in place for arrays.
                $currentIndent = ($tokens[$first]['column'] - 1);
                if (isset($adjustments[$first]) === true) {
                    $currentIndent += $adjustments[$first];
                }

                if ($this->_debug === true) {
                    echo "\t=> checking indent of $checkIndent; main indent set to $currentIndent".PHP_EOL;
                }
            }//end if

            // Adjust lines within scopes while auto-fixing.
            if ($checkToken !== null
                && $exact === false
                && (empty($tokens[$checkToken]['conditions']) === false
                || (isset($tokens[$checkToken]['scope_opener']) === true
                && $tokens[$checkToken]['scope_opener'] === $checkToken))
            ) {
                if (empty($tokens[$checkToken]['conditions']) === false) {
                    end($tokens[$checkToken]['conditions']);
                    $condition = key($tokens[$checkToken]['conditions']);
                } else {
                    $condition = $tokens[$checkToken]['scope_condition'];
                }

                $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $condition, true);

                if (isset($adjustments[$first]) === true
                    && (($adjustments[$first] < 0 && $tokenIndent > $currentIndent)
                    || ($adjustments[$first] > 0 && $tokenIndent < $currentIndent))
                ) {
                    $padding = ($tokenIndent + $adjustments[$first]);
                    if ($padding > 0) {
                        if ($this->tabIndent === true) {
                            $numTabs   = floor($padding / $this->_tabWidth);
                            $numSpaces = ($padding - ($numTabs * $this->_tabWidth));
                            $padding   = str_repeat("\t", $numTabs).str_repeat(' ', $numSpaces);
                        } else {
                            $padding = str_repeat(' ', $padding);
                        }
                    } else {
                        $padding = '';
                    }

                    if ($checkToken === $i) {
                        $phpcsFile->fixer->replaceToken($checkToken, $padding.$trimmed);
                    } else {
                        // Easier to just replace the entire indent.
                        $phpcsFile->fixer->replaceToken(($checkToken - 1), $padding);
                    }

                    if ($this->_debug === true) {
                        $length = strlen($padding);
                        $line   = $tokens[$checkToken]['line'];
                        $type   = $tokens[$checkToken]['type'];
                        echo "Indent adjusted to $length for $type on line $line".PHP_EOL;
                    }

                    $adjustments[$checkToken] = $adjustments[$first];

                    if ($this->_debug === true) {
                        $line = $tokens[$checkToken]['line'];
                        $type = $tokens[$checkToken]['type'];
                        echo "\t=> Add adjustment of ".$adjustments[$checkToken]." for token $checkToken ($type) on line $line".PHP_EOL;
                    }
                }//end if
            }//end if

            // Scope closers reset the required indent to the same level as the opening condition.
            if (($checkToken !== null
                && isset($openScopes[$checkToken]) === true
                || (isset($tokens[$checkToken]['scope_condition']) === true
                && isset($tokens[$checkToken]['scope_closer']) === true
                && $tokens[$checkToken]['scope_closer'] === $checkToken
                && $tokens[$checkToken]['line'] !== $tokens[$tokens[$checkToken]['scope_opener']]['line']))
                || ($checkToken === null
                && isset($openScopes[$i]) === true
                || (isset($tokens[$i]['scope_condition']) === true
                && isset($tokens[$i]['scope_closer']) === true
                && $tokens[$i]['scope_closer'] === $i
                && $tokens[$i]['line'] !== $tokens[$tokens[$i]['scope_opener']]['line']))
            ) {
                if ($this->_debug === true) {
                    if ($checkToken === null) {
                        $type = $tokens[$tokens[$i]['scope_condition']]['type'];
                        $line = $tokens[$i]['line'];
                    } else {
                        $type = $tokens[$tokens[$checkToken]['scope_condition']]['type'];
                        $line = $tokens[$checkToken]['line'];
                    }

                    echo "Close scope ($type) on line $line".PHP_EOL;
                }

                $scopeCloser = $checkToken;
                if ($scopeCloser === null) {
                    $scopeCloser = $i;
                } else {
                    array_pop($openScopes);
                }

                $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $tokens[$scopeCloser]['scope_condition'], true);

                $currentIndent = ($tokens[$first]['column'] - 1);
                if (isset($adjustments[$first]) === true) {
                    $currentIndent += $adjustments[$first];
                }

                // Make sure it is divisible by our expected indent.
                if ($tokens[$tokens[$scopeCloser]['scope_condition']]['code'] !== T_CLOSURE) {
                    $currentIndent = (int) (ceil($currentIndent / $this->indent) * $this->indent);
                }

                if ($this->_debug === true) {
                    echo "\t=> indent set to $currentIndent".PHP_EOL;
                }

                // We only check the indent of scope closers if they are
                // curly braces because other constructs tend to have different rules.
                if ($tokens[$scopeCloser]['code'] === T_CLOSE_CURLY_BRACKET) {
                    $exact = true;
                } else {
                    $checkToken = null;
                }
            }//end if

            // Handle scope for JS object notation.
            if ($phpcsFile->tokenizerType === 'JS'
                && (($checkToken !== null
                && $tokens[$checkToken]['code'] === T_CLOSE_OBJECT
                && $tokens[$checkToken]['line'] !== $tokens[$tokens[$checkToken]['bracket_opener']]['line'])
                || ($checkToken === null
                && $tokens[$i]['code'] === T_CLOSE_OBJECT
                && $tokens[$i]['line'] !== $tokens[$tokens[$i]['bracket_opener']]['line']))
            ) {
                if ($this->_debug === true) {
                    $line = $tokens[$i]['line'];
                    echo "Close JS object on line $line".PHP_EOL;
                }

                $scopeCloser = $checkToken;
                if ($scopeCloser === null) {
                    $scopeCloser = $i;
                } else {
                    array_pop($openScopes);
                }

                $parens = 0;
                if (isset($tokens[$scopeCloser]['nested_parenthesis']) === true
                    && empty($tokens[$scopeCloser]['nested_parenthesis']) === false
                ) {
                    end($tokens[$scopeCloser]['nested_parenthesis']);
                    $parens = key($tokens[$scopeCloser]['nested_parenthesis']);
                    if ($this->_debug === true) {
                        $line = $tokens[$parens]['line'];
                        echo "\t* token has nested parenthesis $parens on line $line *".PHP_EOL;
                    }
                }

                $condition = 0;
                if (isset($tokens[$scopeCloser]['conditions']) === true
                    && empty($tokens[$scopeCloser]['conditions']) === false
                ) {
                    end($tokens[$scopeCloser]['conditions']);
                    $condition = key($tokens[$scopeCloser]['conditions']);
                    if ($this->_debug === true) {
                        $line = $tokens[$condition]['line'];
                        $type = $tokens[$condition]['type'];
                        echo "\t* token is inside condition $condition ($type) on line $line *".PHP_EOL;
                    }
                }

                if ($parens > $condition) {
                    if ($this->_debug === true) {
                        echo "\t* using parenthesis *".PHP_EOL;
                    }

                    $first     = $phpcsFile->findFirstOnLine(T_WHITESPACE, $parens, true);
                    $condition = 0;
                } else if ($condition > 0) {
                    if ($this->_debug === true) {
                        echo "\t* using condition *".PHP_EOL;
                    }

                    $first  = $phpcsFile->findFirstOnLine(T_WHITESPACE, $condition, true);
                    $parens = 0;
                } else {
                    if ($this->_debug === true) {
                        $line = $tokens[$tokens[$scopeCloser]['bracket_opener']]['line'];
                        echo "\t* token is not in parenthesis or condition; using opener on line $line *".PHP_EOL;
                    }

                    $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $tokens[$scopeCloser]['bracket_opener'], true);
                }//end if

                $currentIndent = ($tokens[$first]['column'] - 1);
                if (isset($adjustments[$first]) === true) {
                    $currentIndent += $adjustments[$first];
                }

                if ($parens > 0 || $condition > 0) {
                    $checkIndent = ($tokens[$first]['column'] - 1);
                    if (isset($adjustments[$first]) === true) {
                        $checkIndent += $adjustments[$first];
                    }

                    if ($condition > 0) {
                        $checkIndent   += $this->indent;
                        $currentIndent += $this->indent;
                        $exact          = true;
                    }
                } else {
                    $checkIndent = $currentIndent;
                }

                // Make sure it is divisible by our expected indent.
                $currentIndent = (int) (ceil($currentIndent / $this->indent) * $this->indent);
                $checkIndent   = (int) (ceil($checkIndent / $this->indent) * $this->indent);

                if ($this->_debug === true) {
                    echo "\t=> checking indent of $checkIndent; main indent set to $currentIndent".PHP_EOL;
                }
            }//end if

            if ($checkToken !== null
                && isset(PHP_CodeSniffer_Tokens::$scopeOpeners[$tokens[$checkToken]['code']]) === true
                && in_array($tokens[$checkToken]['code'], $this->nonIndentingScopes) === false
                && isset($tokens[$checkToken]['scope_opener']) === true
            ) {
                $exact = true;

                $lastOpener = null;
                if (empty($openScopes) === false) {
                    end($openScopes);
                    $lastOpener = current($openScopes);
                }

                // A scope opener that shares a closer with another token (like multiple
                // CASEs using the same BREAK) needs to reduce the indent level so its
                // indent is checked correctly. It will then increase the indent again
                // (as all openers do) after being checked.
                if ($lastOpener !== null
                    && isset($tokens[$lastOpener]['scope_closer']) === true
                    && $tokens[$lastOpener]['level'] === $tokens[$checkToken]['level']
                    && $tokens[$lastOpener]['scope_closer'] === $tokens[$checkToken]['scope_closer']
                ) {
                    $currentIndent -= $this->indent;
                    if ($this->_debug === true) {
                        $line = $tokens[$i]['line'];
                        echo "Shared closer found on line $line".PHP_EOL;
                        echo "\t=> indent set to $currentIndent".PHP_EOL;
                    }
                }

                if ($tokens[$checkToken]['code'] === T_CLOSURE
                    && $tokenIndent > $currentIndent
                ) {
                    // The opener is indented more than needed, which is fine.
                    // But just check that it is divisible by our expected indent.
                    $checkIndent = (int) (ceil($tokenIndent / $this->indent) * $this->indent);
                    $exact       = false;

                    if ($this->_debug === true) {
                        $line = $tokens[$i]['line'];
                        echo "Closure found on line $line".PHP_EOL;
                        echo "\t=> checking indent of $checkIndent; main indent remains at $currentIndent".PHP_EOL;
                    }
                }
            }//end if

            // JS property indentation has to be exact or else if will break
            // things like function and object indentation.
            if ($checkToken !== null && $tokens[$checkToken]['code'] === T_PROPERTY) {
                $exact = true;
            }

            // PHP tags needs to be indented to exact column positions
            // so they don't cause problems with indent checks for the code
            // within them, but they don't need to line up with the current indent.
            if ($checkToken !== null
                && ($tokens[$checkToken]['code'] === T_OPEN_TAG
                || $tokens[$checkToken]['code'] === T_OPEN_TAG_WITH_ECHO
                || $tokens[$checkToken]['code'] === T_CLOSE_TAG)
            ) {
                $exact       = true;
                $checkIndent = ($tokens[$checkToken]['column'] - 1);
                $checkIndent = (int) (ceil($checkIndent / $this->indent) * $this->indent);
            }

            // Check the line indent.
            if ($checkIndent === null) {
                $checkIndent = $currentIndent;
            }

            $adjusted = false;
            if ($checkToken !== null
                && isset($this->_ignoreIndentationTokens[$tokens[$checkToken]['code']]) === false
                && (($tokenIndent !== $checkIndent && $exact === true)
                || ($tokenIndent < $checkIndent && $exact === false))
            ) {
                $type  = 'IncorrectExact';
                $error = 'Line indented incorrectly; expected ';
                if ($exact === false) {
                    $error .= 'at least ';
                    $type   = 'Incorrect';
                }

                if ($this->tabIndent === true) {
                    $error .= '%s tabs, found %s';
                    $data   = array(
                               floor($checkIndent / $this->_tabWidth),
                               floor($tokenIndent / $this->_tabWidth),
                              );
                } else {
                    $error .= '%s spaces, found %s';
                    $data   = array(
                               $checkIndent,
                               $tokenIndent,
                              );
                }

                if ($this->_debug === true) {
                    $line    = $tokens[$checkToken]['line'];
                    $message = vsprintf($error, $data);
                    echo "[Line $line] $message".PHP_EOL;
                }

                $fix = $phpcsFile->addFixableError($error, $checkToken, $type, $data);
                if ($fix === true || $this->_debug === true) {
                    $padding = '';
                    if ($this->tabIndent === true) {
                        $numTabs = floor($checkIndent / $this->_tabWidth);
                        if ($numTabs > 0) {
                            $numSpaces = ($checkIndent - ($numTabs * $this->_tabWidth));
                            $padding   = str_repeat("\t", $numTabs).str_repeat(' ', $numSpaces);
                        }
                    } else if ($checkIndent > 0) {
                        $padding = str_repeat(' ', $checkIndent);
                    }

                    if ($checkToken === $i) {
                        $accepted = $phpcsFile->fixer->replaceToken($checkToken, $padding.$trimmed);
                    } else {
                        // Easier to just replace the entire indent.
                        $accepted = $phpcsFile->fixer->replaceToken(($checkToken - 1), $padding);
                    }

                    if ($accepted === true) {
                        $adjustments[$checkToken] = ($checkIndent - $tokenIndent);
                        if ($this->_debug === true) {
                            $line = $tokens[$checkToken]['line'];
                            $type = $tokens[$checkToken]['type'];
                            echo "\t=> Add adjustment of ".$adjustments[$checkToken]." for token $checkToken ($type) on line $line".PHP_EOL;
                        }
                    }
                }//end if
            }//end if

            if ($checkToken !== null) {
                $i = $checkToken;
            }

            // Completely skip here/now docs as the indent is a part of the
            // content itself.
            if ($tokens[$i]['code'] === T_START_HEREDOC
                || $tokens[$i]['code'] === T_START_NOWDOC
            ) {
                $i = $phpcsFile->findNext(array(T_END_HEREDOC, T_END_NOWDOC), ($i + 1));
                continue;
            }

            // Completely skip multi-line strings as the indent is a part of the
            // content itself.
            if ($tokens[$i]['code'] === T_CONSTANT_ENCAPSED_STRING
                || $tokens[$i]['code'] === T_DOUBLE_QUOTED_STRING
            ) {
                $i = $phpcsFile->findNext($tokens[$i]['code'], ($i + 1), null, true);
                $i--;
                continue;
            }

            // Completely skip doc comments as they tend to have complex
            // indentation rules.
            if ($tokens[$i]['code'] === T_DOC_COMMENT_OPEN_TAG) {
                $i = $tokens[$i]['comment_closer'];
                continue;
            }

            // Open tags reset the indent level.
            if ($tokens[$i]['code'] === T_OPEN_TAG
                || $tokens[$i]['code'] === T_OPEN_TAG_WITH_ECHO
            ) {
                if ($this->_debug === true) {
                    $line = $tokens[$i]['line'];
                    echo "Open PHP tag found on line $line".PHP_EOL;
                }

                if ($checkToken === null) {
                    $first         = $phpcsFile->findFirstOnLine(T_WHITESPACE, $i, true);
                    $currentIndent = (strlen($tokens[$first]['content']) - strlen(ltrim($tokens[$first]['content'])));
                } else {
                    $currentIndent = ($tokens[$i]['column'] - 1);
                }

                $lastOpenTag = $i;

                if (isset($adjustments[$i]) === true) {
                    $currentIndent += $adjustments[$i];
                }

                // Make sure it is divisible by our expected indent.
                $currentIndent = (int) (ceil($currentIndent / $this->indent) * $this->indent);

                if ($this->_debug === true) {
                    echo "\t=> indent set to $currentIndent".PHP_EOL;
                }

                continue;
            }//end if

            // Close tags reset the indent level, unless they are closing a tag
            // opened on the same line.
            if ($tokens[$i]['code'] === T_CLOSE_TAG) {
                if ($this->_debug === true) {
                    $line = $tokens[$i]['line'];
                    echo "Close PHP tag found on line $line".PHP_EOL;
                }

                if ($tokens[$lastOpenTag]['line'] !== $tokens[$i]['line']) {
                    $currentIndent = ($tokens[$i]['column'] - 1);
                    $lastCloseTag  = $i;
                } else {
                    if ($lastCloseTag === null) {
                        $currentIndent = 0;
                    } else {
                        $currentIndent = ($tokens[$lastCloseTag]['column'] - 1);
                    }
                }

                if (isset($adjustments[$i]) === true) {
                    $currentIndent += $adjustments[$i];
                }

                // Make sure it is divisible by our expected indent.
                $currentIndent = (int) (ceil($currentIndent / $this->indent) * $this->indent);

                if ($this->_debug === true) {
                    echo "\t=> indent set to $currentIndent".PHP_EOL;
                }

                continue;
            }//end if

            // Closures set the indent based on their own indent level.
            if ($tokens[$i]['code'] === T_CLOSURE) {
                $closer = $tokens[$i]['scope_closer'];
                if ($tokens[$i]['line'] === $tokens[$closer]['line']) {
                    if ($this->_debug === true) {
                        $line = $tokens[$i]['line'];
                        echo "* ignoring single-line closure on line $line".PHP_EOL;
                    }

                    $i = $closer;
                    continue;
                }

                if ($this->_debug === true) {
                    $line = $tokens[$i]['line'];
                    echo "Open closure on line $line".PHP_EOL;
                }

                $first         = $phpcsFile->findFirstOnLine(T_WHITESPACE, $i, true);
                $currentIndent = (($tokens[$first]['column'] - 1) + $this->indent);

                if (isset($adjustments[$first]) === true) {
                    $currentIndent += $adjustments[$first];
                }

                // Make sure it is divisible by our expected indent.
                $currentIndent = (int) (floor($currentIndent / $this->indent) * $this->indent);
                $i = $tokens[$i]['scope_opener'];

                if ($this->_debug === true) {
                    echo "\t=> indent set to $currentIndent".PHP_EOL;
                }

                continue;
            }//end if

            // Scope openers increase the indent level.
            if (isset($tokens[$i]['scope_condition']) === true
                && isset($tokens[$i]['scope_opener']) === true
                && $tokens[$i]['scope_opener'] === $i
            ) {
                $closer = $tokens[$i]['scope_closer'];
                if ($tokens[$i]['line'] === $tokens[$closer]['line']) {
                    if ($this->_debug === true) {
                        $line = $tokens[$i]['line'];
                        $type = $tokens[$i]['type'];
                        echo "* ignoring single-line $type on line $line".PHP_EOL;
                    }

                    $i = $closer;
                    continue;
                }

                $condition = $tokens[$tokens[$i]['scope_condition']]['code'];
                if (isset(PHP_CodeSniffer_Tokens::$scopeOpeners[$condition]) === true
                    && in_array($condition, $this->nonIndentingScopes) === false
                ) {
                    if ($this->_debug === true) {
                        $line = $tokens[$i]['line'];
                        $type = $tokens[$tokens[$i]['scope_condition']]['type'];
                        echo "Open scope ($type) on line $line".PHP_EOL;
                    }

                    $currentIndent += $this->indent;
                    $openScopes[$tokens[$i]['scope_closer']] = $tokens[$i]['scope_condition'];

                    if ($this->_debug === true) {
                        echo "\t=> indent set to $currentIndent".PHP_EOL;
                    }

                    continue;
                }
            }//end if

            // JS objects set the indent level.
            if ($phpcsFile->tokenizerType === 'JS'
                && $tokens[$i]['code'] === T_OBJECT
            ) {
                $closer = $tokens[$i]['bracket_closer'];
                if ($tokens[$i]['line'] === $tokens[$closer]['line']) {
                    if ($this->_debug === true) {
                        $line = $tokens[$i]['line'];
                        echo "* ignoring single-line JS object on line $line".PHP_EOL;
                    }

                    $i = $closer;
                    continue;
                }

                if ($this->_debug === true) {
                    $line = $tokens[$i]['line'];
                    echo "Open JS object on line $line".PHP_EOL;
                }

                $first         = $phpcsFile->findFirstOnLine(T_WHITESPACE, $i, true);
                $currentIndent = (($tokens[$first]['column'] - 1) + $this->indent);
                if (isset($adjustments[$first]) === true) {
                    $currentIndent += $adjustments[$first];
                }

                // Make sure it is divisible by our expected indent.
                $currentIndent = (int) (ceil($currentIndent / $this->indent) * $this->indent);
                $openScopes[]  = $i;

                if ($this->_debug === true) {
                    echo "\t=> indent set to $currentIndent".PHP_EOL;
                }

                continue;
            }//end if

            // Closing a closure.
            if (isset($tokens[$i]['scope_condition']) === true
                && $tokens[$i]['scope_closer'] === $i
                && $tokens[$tokens[$i]['scope_condition']]['code'] === T_CLOSURE
            ) {
                if ($this->_debug === true) {
                    $line = $tokens[$i]['line'];
                    echo "Close closure on line $line".PHP_EOL;
                }

                $prev = false;

                $object = 0;
                if ($phpcsFile->tokenizerType === 'JS') {
                    $conditions = $tokens[$i]['conditions'];
                    krsort($conditions, SORT_NUMERIC);
                    foreach ($conditions as $token => $condition) {
                        if ($condition === T_OBJECT) {
                            $object = $token;
                            break;
                        }
                    }

                    if ($this->_debug === true && $object !== 0) {
                        $line = $tokens[$object]['line'];
                        echo "\t* token is inside JS object $object on line $line *".PHP_EOL;
                    }
                }

                $parens = 0;
                if (isset($tokens[$i]['nested_parenthesis']) === true
                    && empty($tokens[$i]['nested_parenthesis']) === false
                ) {
                    end($tokens[$i]['nested_parenthesis']);
                    $parens = key($tokens[$i]['nested_parenthesis']);
                    if ($this->_debug === true) {
                        $line = $tokens[$parens]['line'];
                        echo "\t* token has nested parenthesis $parens on line $line *".PHP_EOL;
                    }
                }

                $condition = 0;
                if (isset($tokens[$i]['conditions']) === true
                    && empty($tokens[$i]['conditions']) === false
                ) {
                    end($tokens[$i]['conditions']);
                    $condition = key($tokens[$i]['conditions']);
                    if ($this->_debug === true) {
                        $line = $tokens[$condition]['line'];
                        $type = $tokens[$condition]['type'];
                        echo "\t* token is inside condition $condition ($type) on line $line *".PHP_EOL;
                    }
                }

                if ($parens > $object && $parens > $condition) {
                    if ($this->_debug === true) {
                        echo "\t* using parenthesis *".PHP_EOL;
                    }

                    $prev      = $phpcsFile->findPrevious(PHP_CodeSniffer_Tokens::$emptyTokens, ($parens - 1), null, true);
                    $object    = 0;
                    $condition = 0;
                } else if ($object > 0 && $object >= $condition) {
                    if ($this->_debug === true) {
                        echo "\t* using object *".PHP_EOL;
                    }

                    $prev      = $object;
                    $parens    = 0;
                    $condition = 0;
                } else if ($condition > 0) {
                    if ($this->_debug === true) {
                        echo "\t* using condition *".PHP_EOL;
                    }

                    $prev   = $condition;
                    $object = 0;
                    $parens = 0;
                }//end if

                if ($prev === false) {
                    $prev = $phpcsFile->findPrevious(array(T_EQUAL, T_RETURN), ($tokens[$i]['scope_condition'] - 1), null, false, null, true);
                    if ($prev === false) {
                        $prev = $i;
                        if ($this->_debug === true) {
                            echo "\t* could not find a previous T_EQUAL or T_RETURN token; will use current token *".PHP_EOL;
                        }
                    }
                }

                if ($this->_debug === true) {
                    $line = $tokens[$prev]['line'];
                    $type = $tokens[$prev]['type'];
                    echo "\t* previous token is $type on line $line *".PHP_EOL;
                }

                $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $prev, true);
                if ($this->_debug === true) {
                    $line = $tokens[$first]['line'];
                    $type = $tokens[$first]['type'];
                    echo "\t* first token on line $line is $type *".PHP_EOL;
                }

                $prev = $phpcsFile->findStartOfStatement($first);
                if ($prev !== $first) {
                    // This is not the start of the statement.
                    if ($this->_debug === true) {
                        $line = $tokens[$prev]['line'];
                        $type = $tokens[$prev]['type'];
                        echo "\t* amended previous is $type on line $line *".PHP_EOL;
                    }

                    $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $prev, true);
                    if ($this->_debug === true) {
                        $line = $tokens[$first]['line'];
                        $type = $tokens[$first]['type'];
                        echo "\t* amended first token is $type on line $line *".PHP_EOL;
                    }
                }

                $currentIndent = ($tokens[$first]['column'] - 1);

                if ($object > 0 || $condition > 0) {
                    $currentIndent += $this->indent;
                }

                // Make sure it is divisible by our expected indent.
                $currentIndent = (int) (ceil($currentIndent / $this->indent) * $this->indent);

                if ($this->_debug === true) {
                    echo "\t=> indent set to $currentIndent".PHP_EOL;
                }
            }//end if
        }//end for

        // Don't process the rest of the file.
        return $phpcsFile->numTokens;

    }//end process()


}//end class
