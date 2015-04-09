<?php
/**
 * Tokenizes PHP code.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Tokenizes PHP code.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_Tokenizers_PHP
{

    /**
     * A list of tokens that are allowed to open a scope.
     *
     * This array also contains information about what kind of token the scope
     * opener uses to open and close the scope, if the token strictly requires
     * an opener, if the token can share a scope closer, and who it can be shared
     * with. An example of a token that shares a scope closer is a CASE scope.
     *
     * @var array
     */
    public $scopeOpeners = array(
                            T_IF            => array(
                                                'start'  => array(
                                                             T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET,
                                                             T_COLON              => T_COLON,
                                                            ),
                                                'end'    => array(
                                                             T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET,
                                                             T_ENDIF               => T_ENDIF,
                                                             T_ELSE                => T_ELSE,
                                                             T_ELSEIF              => T_ELSEIF,
                                                            ),
                                                'strict' => false,
                                                'shared' => false,
                                                'with'   => array(
                                                             T_ELSE   => T_ELSE,
                                                             T_ELSEIF => T_ELSEIF,
                                                            ),
                                               ),
                            T_TRY           => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_CATCH         => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_FINALLY       => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_ELSE          => array(
                                                'start'  => array(
                                                             T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET,
                                                             T_COLON              => T_COLON,
                                                            ),
                                                'end'    => array(
                                                             T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET,
                                                             T_ENDIF               => T_ENDIF,
                                                            ),
                                                'strict' => false,
                                                'shared' => false,
                                                'with'   => array(
                                                             T_IF     => T_IF,
                                                             T_ELSEIF => T_ELSEIF,
                                                            ),
                                               ),
                            T_ELSEIF        => array(
                                                'start'  => array(
                                                             T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET,
                                                             T_COLON              => T_COLON,
                                                            ),
                                                'end'    => array(
                                                             T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET,
                                                             T_ENDIF               => T_ENDIF,
                                                             T_ELSE                => T_ELSE,
                                                             T_ELSEIF              => T_ELSEIF,
                                                            ),
                                                'strict' => false,
                                                'shared' => false,
                                                'with'   => array(
                                                             T_IF   => T_IF,
                                                             T_ELSE => T_ELSE,
                                                            ),
                                               ),
                            T_FOR           => array(
                                                'start'  => array(
                                                             T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET,
                                                             T_COLON              => T_COLON,
                                                            ),
                                                'end'    => array(
                                                             T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET,
                                                             T_ENDFOR              => T_ENDFOR,
                                                            ),
                                                'strict' => false,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_FOREACH       => array(
                                                'start'  => array(
                                                             T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET,
                                                             T_COLON              => T_COLON,
                                                            ),
                                                'end'    => array(
                                                             T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET,
                                                             T_ENDFOREACH          => T_ENDFOREACH,
                                                            ),
                                                'strict' => false,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_INTERFACE     => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_FUNCTION      => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_CLASS         => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_TRAIT         => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_USE           => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => false,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_DECLARE       => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => false,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_NAMESPACE     => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => false,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_WHILE         => array(
                                                'start'  => array(
                                                             T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET,
                                                             T_COLON              => T_COLON,
                                                            ),
                                                'end'    => array(
                                                             T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET,
                                                             T_ENDWHILE            => T_ENDWHILE,
                                                            ),
                                                'strict' => false,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_DO            => array(
                                                'start'  => array(T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET),
                                                'end'    => array(T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_SWITCH        => array(
                                                'start'  => array(
                                                             T_OPEN_CURLY_BRACKET => T_OPEN_CURLY_BRACKET,
                                                             T_COLON              => T_COLON,
                                                            ),
                                                'end'    => array(
                                                             T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET,
                                                             T_ENDSWITCH           => T_ENDSWITCH,
                                                            ),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                            T_CASE          => array(
                                                'start'  => array(
                                                             T_COLON     => T_COLON,
                                                             T_SEMICOLON => T_SEMICOLON,
                                                            ),
                                                'end'    => array(
                                                             T_BREAK    => T_BREAK,
                                                             T_RETURN   => T_RETURN,
                                                             T_CONTINUE => T_CONTINUE,
                                                             T_THROW    => T_THROW,
                                                             T_EXIT     => T_EXIT,
                                                            ),
                                                'strict' => true,
                                                'shared' => true,
                                                'with'   => array(
                                                             T_DEFAULT => T_DEFAULT,
                                                             T_CASE    => T_CASE,
                                                             T_SWITCH  => T_SWITCH,
                                                            ),
                                               ),
                            T_DEFAULT       => array(
                                                'start'  => array(
                                                             T_COLON     => T_COLON,
                                                             T_SEMICOLON => T_SEMICOLON,
                                                            ),
                                                'end'    => array(
                                                             T_BREAK    => T_BREAK,
                                                             T_RETURN   => T_RETURN,
                                                             T_CONTINUE => T_CONTINUE,
                                                             T_THROW    => T_THROW,
                                                             T_EXIT     => T_EXIT,
                                                            ),
                                                'strict' => true,
                                                'shared' => true,
                                                'with'   => array(
                                                             T_CASE   => T_CASE,
                                                             T_SWITCH => T_SWITCH,
                                                            ),
                                               ),
                            T_START_HEREDOC => array(
                                                'start'  => array(T_START_HEREDOC => T_START_HEREDOC),
                                                'end'    => array(T_END_HEREDOC => T_END_HEREDOC),
                                                'strict' => true,
                                                'shared' => false,
                                                'with'   => array(),
                                               ),
                           );

    /**
     * A list of tokens that end the scope.
     *
     * This array is just a unique collection of the end tokens
     * from the _scopeOpeners array. The data is duplicated here to
     * save time during parsing of the file.
     *
     * @var array
     */
    public $endScopeTokens = array(
                              T_CLOSE_CURLY_BRACKET => T_CLOSE_CURLY_BRACKET,
                              T_ENDIF               => T_ENDIF,
                              T_ENDFOR              => T_ENDFOR,
                              T_ENDFOREACH          => T_ENDFOREACH,
                              T_ENDWHILE            => T_ENDWHILE,
                              T_ENDSWITCH           => T_ENDSWITCH,
                              T_BREAK               => T_BREAK,
                              T_END_HEREDOC         => T_END_HEREDOC,
                             );

    /**
     * A cache of different token types, resolved into arrays.
     *
     * @var array()
     * @see standardiseToken()
     */
    private static $_resolveTokenCache = array();


    /**
     * Creates an array of tokens when given some PHP code.
     *
     * Starts by using token_get_all() but does a lot of extra processing
     * to insert information about the context of the token.
     *
     * @param string $string  The string to tokenize.
     * @param string $eolChar The EOL character to use for splitting strings.
     *
     * @return array
     */
    public function tokenizeString($string, $eolChar='\n')
    {
        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo "\t*** START PHP TOKENIZING ***".PHP_EOL;
            $isWin = false;
            if (strtoupper(substr(PHP_OS, 0, 3)) === 'WIN') {
                $isWin = true;
            }
        }

        $tokens      = @token_get_all($string);
        $finalTokens = array();

        $newStackPtr       = 0;
        $numTokens         = count($tokens);
        $lastNotEmptyToken = 0;

        $insideInlineIf = array();

        $commentTokenizer = new PHP_CodeSniffer_Tokenizers_Comment();

        for ($stackPtr = 0; $stackPtr < $numTokens; $stackPtr++) {
            $token        = (array) $tokens[$stackPtr];
            $tokenIsArray = isset($token[1]);

            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                if ($tokenIsArray === true) {
                    $type    = token_name($token[0]);
                    $content = PHP_CodeSniffer::prepareForOutput($token[1]);
                } else {
                    $newToken = self::resolveSimpleToken($token[0]);
                    $type     = $newToken['type'];
                    $content  = PHP_CodeSniffer::prepareForOutput($token[0]);
                }

                echo "\tProcess token ";
                if ($tokenIsArray === true) {
                    echo "[$stackPtr]";
                } else {
                    echo " $stackPtr ";
                }

                echo ": $type => $content";
            }//end if

            if ($newStackPtr > 0 && $finalTokens[($newStackPtr - 1)]['code'] !== T_WHITESPACE) {
                $lastNotEmptyToken = ($newStackPtr - 1);
            }

            /*
                If we are using \r\n newline characters, the \r and \n are sometimes
                split over two tokens. This normally occurs after comments. We need
                to merge these two characters together so that our line endings are
                consistent for all lines.
            */

            if ($tokenIsArray === true && substr($token[1], -1) === "\r") {
                if (isset($tokens[($stackPtr + 1)]) === true
                    && is_array($tokens[($stackPtr + 1)]) === true
                    && $tokens[($stackPtr + 1)][1][0] === "\n"
                ) {
                    $token[1] .= "\n";
                    if (PHP_CODESNIFFER_VERBOSITY > 1) {
                        if ($isWin === true) {
                            echo '\n';
                        } else {
                            echo "\033[30;1m\\n\033[0m";
                        }
                    }

                    if ($tokens[($stackPtr + 1)][1] === "\n") {
                        // This token's content has been merged into the previous,
                        // so we can skip it.
                        $tokens[($stackPtr + 1)] = '';
                    } else {
                        $tokens[($stackPtr + 1)][1] = substr($tokens[($stackPtr + 1)][1], 1);
                    }
                }
            }//end if

            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo PHP_EOL;
            }

            /*
                Parse doc blocks into something that can be easily iterated over.
            */

            if ($tokenIsArray === true && $token[0] === T_DOC_COMMENT) {
                $commentTokens = $commentTokenizer->tokenizeString($token[1], $eolChar, $newStackPtr);
                foreach ($commentTokens as $commentToken) {
                    $finalTokens[$newStackPtr] = $commentToken;
                    $newStackPtr++;
                }

                continue;
            }

            /*
                If this is a double quoted string, PHP will tokenize the whole
                thing which causes problems with the scope map when braces are
                within the string. So we need to merge the tokens together to
                provide a single string.
            */

            if ($tokenIsArray === false && ($token[0] === '"' || $token[0] === 'b"')) {
                // Binary casts need a special token.
                if ($token[0] === 'b"') {
                    $finalTokens[$newStackPtr] = array(
                                                  'code'    => T_BINARY_CAST,
                                                  'type'    => 'T_BINARY_CAST',
                                                  'content' => 'b',
                                                 );
                    $newStackPtr++;
                }

                $tokenContent = '"';
                $nestedVars   = array();
                for ($i = ($stackPtr + 1); $i < $numTokens; $i++) {
                    $subToken        = (array) $tokens[$i];
                    $subTokenIsArray = isset($subToken[1]);

                    if ($subTokenIsArray === true) {
                        $tokenContent .= $subToken[1];
                        if ($subToken[1] === '{'
                            && $subToken[0] !== T_ENCAPSED_AND_WHITESPACE
                        ) {
                            $nestedVars[] = $i;
                        }
                    } else {
                        $tokenContent .= $subToken[0];
                        if ($subToken[0] === '}') {
                            array_pop($nestedVars);
                        }
                    }

                    if ($subTokenIsArray === false
                        && $subToken[0] === '"'
                        && empty($nestedVars) === true
                    ) {
                        // We found the other end of the double quoted string.
                        break;
                    }
                }//end for

                $stackPtr = $i;

                // Convert each line within the double quoted string to a
                // new token, so it conforms with other multiple line tokens.
                $tokenLines = explode($eolChar, $tokenContent);
                $numLines   = count($tokenLines);
                $newToken   = array();

                for ($j = 0; $j < $numLines; $j++) {
                    $newToken['content'] = $tokenLines[$j];
                    if ($j === ($numLines - 1)) {
                        if ($tokenLines[$j] === '') {
                            break;
                        }
                    } else {
                        $newToken['content'] .= $eolChar;
                    }

                    $newToken['code']          = T_DOUBLE_QUOTED_STRING;
                    $newToken['type']          = 'T_DOUBLE_QUOTED_STRING';
                    $finalTokens[$newStackPtr] = $newToken;
                    $newStackPtr++;
                }

                // Continue, as we're done with this token.
                continue;
            }//end if

            /*
                If this is a heredoc, PHP will tokenize the whole
                thing which causes problems when heredocs don't
                contain real PHP code, which is almost never.
                We want to leave the start and end heredoc tokens
                alone though.
            */

            if ($tokenIsArray === true && $token[0] === T_START_HEREDOC) {
                // Add the start heredoc token to the final array.
                $finalTokens[$newStackPtr] = self::standardiseToken($token);

                // Check if this is actually a nowdoc and use a different token
                // to help the sniffs.
                $nowdoc = false;
                if ($token[1][3] === "'") {
                    $finalTokens[$newStackPtr]['code'] = T_START_NOWDOC;
                    $finalTokens[$newStackPtr]['type'] = 'T_START_NOWDOC';
                    $nowdoc = true;
                }

                $newStackPtr++;

                $tokenContent = '';
                for ($i = ($stackPtr + 1); $i < $numTokens; $i++) {
                    $subTokenIsArray = is_array($tokens[$i]);
                    if ($subTokenIsArray === true
                        && $tokens[$i][0] === T_END_HEREDOC
                    ) {
                        // We found the other end of the heredoc.
                        break;
                    }

                    if ($subTokenIsArray === true) {
                        $tokenContent .= $tokens[$i][1];
                    } else {
                        $tokenContent .= $tokens[$i];
                    }
                }

                $stackPtr = $i;

                // Convert each line within the heredoc to a
                // new token, so it conforms with other multiple line tokens.
                $tokenLines = explode($eolChar, $tokenContent);
                $numLines   = count($tokenLines);
                $newToken   = array();

                for ($j = 0; $j < $numLines; $j++) {
                    $newToken['content'] = $tokenLines[$j];
                    if ($j === ($numLines - 1)) {
                        if ($tokenLines[$j] === '') {
                            break;
                        }
                    } else {
                        $newToken['content'] .= $eolChar;
                    }

                    if ($nowdoc === true) {
                        $newToken['code'] = T_NOWDOC;
                        $newToken['type'] = 'T_NOWDOC';
                    } else {
                        $newToken['code'] = T_HEREDOC;
                        $newToken['type'] = 'T_HEREDOC';
                    }

                    $finalTokens[$newStackPtr] = $newToken;
                    $newStackPtr++;
                }//end for

                // Add the end heredoc token to the final array.
                $finalTokens[$newStackPtr] = self::standardiseToken($tokens[$stackPtr]);

                if ($nowdoc === true) {
                    $finalTokens[$newStackPtr]['code'] = T_END_NOWDOC;
                    $finalTokens[$newStackPtr]['type'] = 'T_END_NOWDOC';
                    $nowdoc = true;
                }

                $newStackPtr++;

                // Continue, as we're done with this token.
                continue;
            }//end if

            /*
                Before PHP 5.6, the ... operator was tokenized as three
                T_STRING_CONCAT tokens in a row. So look for and combine
                these tokens in earlier versions.
            */

            if ($tokenIsArray === false
                && $token[0] === '.'
                && isset($tokens[($stackPtr + 1)]) === true
                && isset($tokens[($stackPtr + 2)]) === true
                && $tokens[($stackPtr + 1)] === '.'
                && $tokens[($stackPtr + 2)] === '.'
            ) {
                $newToken            = array();
                $newToken['code']    = T_ELLIPSIS;
                $newToken['type']    = 'T_ELLIPSIS';
                $newToken['content'] = '...';
                $finalTokens[$newStackPtr] = $newToken;

                $newStackPtr++;
                $stackPtr += 2;
                continue;
            }

            /*
                PHP doesn't assign a token to goto labels, so we have to.
                These are just string tokens with a single colon after them. Double
                colons are already tokenized and so don't interfere with this check.
                But we do have to account for CASE statements, that look just like
                goto labels.
            */

            if ($tokenIsArray === true
                && $token[0] === T_STRING
                && $tokens[($stackPtr + 1)] === ':'
                && $tokens[($stackPtr - 1)][0] !== T_PAAMAYIM_NEKUDOTAYIM
            ) {
                $stopTokens = array(
                               T_CASE               => true,
                               T_SEMICOLON          => true,
                               T_OPEN_CURLY_BRACKET => true,
                               T_INLINE_THEN        => true,
                              );

                for ($x = ($newStackPtr - 1); $x > 0; $x--) {
                    if (isset($stopTokens[$finalTokens[$x]['code']]) === true) {
                        break;
                    }
                }

                if ($finalTokens[$x]['code'] !== T_CASE
                    && $finalTokens[$x]['code'] !== T_INLINE_THEN
                ) {
                    $finalTokens[$newStackPtr] = array(
                                                  'content' => $token[1].':',
                                                  'code'    => T_GOTO_LABEL,
                                                  'type'    => 'T_GOTO_LABEL',
                                                 );

                    if (PHP_CODESNIFFER_VERBOSITY > 1) {
                        echo "\t\t* token $stackPtr changed from T_STRING to T_GOTO_LABEL".PHP_EOL;
                        echo "\t\t* skipping T_COLON token ".($stackPtr + 1).PHP_EOL;
                    }

                    $newStackPtr++;
                    $stackPtr++;
                    continue;
                }
            }//end if

            /*
                HHVM 3.5 tokenizes "else[\s]+if" as a T_ELSEIF token while PHP
                proper only tokenizes "elseif" as a T_ELSEIF token. So split
                up the HHVM token to make it looks like proper PHP.
            */

            if ($tokenIsArray === true
                && $token[0] === T_ELSEIF
                && strtolower($token[1]) !== 'elseif'
            ) {
                $finalTokens[$newStackPtr] = array(
                                              'content' => substr($token[1], 0, 4),
                                              'code'    => T_ELSE,
                                              'type'    => 'T_ELSE',
                                             );

                $newStackPtr++;
                $finalTokens[$newStackPtr] = array(
                                              'content' => substr($token[1], 4, -2),
                                              'code'    => T_WHITESPACE,
                                              'type'    => 'T_WHITESPACE',
                                             );

                $newStackPtr++;
                $finalTokens[$newStackPtr] = array(
                                              'content' => substr($token[1], -2),
                                              'code'    => T_IF,
                                              'type'    => 'T_IF',
                                             );

                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo "\t\t* token $stackPtr changed from T_ELSEIF to T_ELSE/T_WHITESPACE/T_IF".PHP_EOL;
                }

                $newStackPtr++;
                continue;
            }//end if

            /*
                If this token has newlines in its content, split each line up
                and create a new token for each line. We do this so it's easier
                to ascertain where errors occur on a line.
                Note that $token[1] is the token's content.
            */

            if ($tokenIsArray === true && strpos($token[1], $eolChar) !== false) {
                $tokenLines = explode($eolChar, $token[1]);
                $numLines   = count($tokenLines);
                $newToken   = array(
                               'type'    => token_name($token[0]),
                               'code'    => $token[0],
                               'content' => '',
                              );

                for ($i = 0; $i < $numLines; $i++) {
                    $newToken['content'] = $tokenLines[$i];
                    if ($i === ($numLines - 1)) {
                        if ($tokenLines[$i] === '') {
                            break;
                        }
                    } else {
                        $newToken['content'] .= $eolChar;
                    }

                    $finalTokens[$newStackPtr] = $newToken;
                    $newStackPtr++;
                }
            } else {
                if ($tokenIsArray === true && $token[0] === T_STRING) {
                    // Some T_STRING tokens should remain that way
                    // due to their context.
                    $context = array(
                                T_OBJECT_OPERATOR      => true,
                                T_FUNCTION             => true,
                                T_CLASS                => true,
                                T_EXTENDS              => true,
                                T_IMPLEMENTS           => true,
                                T_NEW                  => true,
                                T_CONST                => true,
                                T_NS_SEPARATOR         => true,
                                T_USE                  => true,
                                T_NAMESPACE            => true,
                                T_PAAMAYIM_NEKUDOTAYIM => true,
                               );
                    if (isset($context[$finalTokens[$lastNotEmptyToken]['code']]) === true) {
                        $finalTokens[$newStackPtr] = array(
                                                      'content' => $token[1],
                                                      'code'    => T_STRING,
                                                      'type'    => 'T_STRING',
                                                     );
                        $newStackPtr++;
                        continue;
                    }
                }//end if

                $newToken = null;
                if ($tokenIsArray === false) {
                    if (isset(self::$_resolveTokenCache[$token[0]]) === true) {
                        $newToken = self::$_resolveTokenCache[$token[0]];
                    }
                } else {
                    $cacheKey = null;
                    if ($token[0] === T_STRING) {
                        $cacheKey = strtolower($token[1]);
                    } else if ($token[0] !== T_CURLY_OPEN) {
                        $cacheKey = $token[0];
                    }

                    if ($cacheKey !== null && isset(self::$_resolveTokenCache[$cacheKey]) === true) {
                        $newToken            = self::$_resolveTokenCache[$cacheKey];
                        $newToken['content'] = $token[1];
                    }
                }

                if ($newToken === null) {
                    $newToken = self::standardiseToken($token);
                }

                // Convert colons that are actually the ELSE component of an
                // inline IF statement.
                if ($newToken['code'] === T_INLINE_THEN) {
                    $insideInlineIf[] = $stackPtr;
                } else if (empty($insideInlineIf) === false && $newToken['code'] === T_COLON) {
                    array_pop($insideInlineIf);
                    $newToken['code'] = T_INLINE_ELSE;
                    $newToken['type'] = 'T_INLINE_ELSE';
                }

                // This is a special condition for T_ARRAY tokens used for
                // type hinting function arguments as being arrays. We want to keep
                // the parenthesis map clean, so let's tag these tokens as
                // T_ARRAY_HINT.
                if ($newToken['code'] === T_ARRAY) {
                    // Recalculate number of tokens.
                    for ($i = $stackPtr; $i < $numTokens; $i++) {
                        if ($tokens[$i] === '(') {
                            break;
                        } else if ($tokens[$i][0] === T_VARIABLE) {
                            $newToken['code'] = T_ARRAY_HINT;
                            $newToken['type'] = 'T_ARRAY_HINT';
                            break;
                        }
                    }
                }

                // This is a special case for the PHP 5.5 classname::class syntax
                // where "class" should be T_STRING instead of T_CLASS.
                if ($newToken['code'] === T_CLASS
                    && $finalTokens[($newStackPtr - 1)]['code'] === T_DOUBLE_COLON
                ) {
                    $newToken['code'] = T_STRING;
                    $newToken['type'] = 'T_STRING';
                }

                // This is a special case for PHP 5.6 use function and use const
                // where "function" and "const" should be T_STRING instead of T_FUNCTION
                // and T_CONST.
                if (($newToken['code'] === T_FUNCTION
                    || $newToken['code'] === T_CONST)
                    && $finalTokens[$lastNotEmptyToken]['code'] === T_USE
                ) {
                    $newToken['code'] = T_STRING;
                    $newToken['type'] = 'T_STRING';
                }

                $finalTokens[$newStackPtr] = $newToken;
                $newStackPtr++;
            }//end if
        }//end for

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo "\t*** END PHP TOKENIZING ***".PHP_EOL;
        }

        return $finalTokens;

    }//end tokenizeString()


    /**
     * Performs additional processing after main tokenizing.
     *
     * This additional processing checks for CASE statements that are using curly
     * braces for scope openers and closers. It also turns some T_FUNCTION tokens
     * into T_CLOSURE when they are not standard function definitions. It also
     * detects short array syntax and converts those square brackets into new tokens.
     * It also corrects some usage of the static and class keywords.
     *
     * @param array  $tokens  The array of tokens to process.
     * @param string $eolChar The EOL character to use for splitting strings.
     *
     * @return void
     */
    public function processAdditional(&$tokens, $eolChar)
    {
        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo "\t*** START ADDITIONAL PHP PROCESSING ***".PHP_EOL;
        }

        $numTokens = count($tokens);
        for ($i = ($numTokens - 1); $i >= 0; $i--) {
            // Check for any unset scope conditions due to alternate IF/ENDIF syntax.
            if (isset($tokens[$i]['scope_opener']) === true
                && isset($tokens[$i]['scope_condition']) === false
            ) {
                $tokens[$i]['scope_condition'] = $tokens[$tokens[$i]['scope_opener']]['scope_condition'];
            }

            // Looking for functions that are actually closures.
            if ($tokens[$i]['code'] === T_FUNCTION && isset($tokens[$i]['scope_opener']) === true) {
                for ($x = ($i + 1); $x < $numTokens; $x++) {
                    if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$x]['code']]) === false) {
                        break;
                    }
                }

                if ($tokens[$x]['code'] === T_OPEN_PARENTHESIS) {
                    $tokens[$i]['code'] = T_CLOSURE;
                    $tokens[$i]['type'] = 'T_CLOSURE';
                    if (PHP_CODESNIFFER_VERBOSITY > 1) {
                        $line = $tokens[$i]['line'];
                        echo "\t* token $i on line $line changed from T_FUNCTION to T_CLOSURE".PHP_EOL;
                    }

                    for ($x = ($tokens[$i]['scope_opener'] + 1); $x < $tokens[$i]['scope_closer']; $x++) {
                        if (isset($tokens[$x]['conditions'][$i]) === false) {
                            continue;
                        }

                        $tokens[$x]['conditions'][$i] = T_CLOSURE;
                        if (PHP_CODESNIFFER_VERBOSITY > 1) {
                            $type = $tokens[$x]['type'];
                            echo "\t\t* cleaned $x ($type) *".PHP_EOL;
                        }
                    }
                }

                continue;
            } else if ($tokens[$i]['code'] === T_OPEN_SQUARE_BRACKET) {
                // Unless there is a variable or a bracket before this token,
                // it is the start of an array being defined using the short syntax.
                for ($x = ($i - 1); $x > 0; $x--) {
                    if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$x]['code']]) === false) {
                        break;
                    }
                }

                $allowed = array(
                            T_CLOSE_CURLY_BRACKET  => T_CLOSE_CURLY_BRACKET,
                            T_CLOSE_SQUARE_BRACKET => T_CLOSE_SQUARE_BRACKET,
                            T_CLOSE_PARENTHESIS    => T_CLOSE_PARENTHESIS,
                            T_VARIABLE             => T_VARIABLE,
                            T_STRING               => T_STRING,
                           );

                if (isset($allowed[$tokens[$x]['code']]) === false
                    && isset($tokens[$i]['bracket_closer']) === true
                ) {
                    $tokens[$i]['code'] = T_OPEN_SHORT_ARRAY;
                    $tokens[$i]['type'] = 'T_OPEN_SHORT_ARRAY';

                    $closer = $tokens[$i]['bracket_closer'];
                    $tokens[$closer]['code'] = T_CLOSE_SHORT_ARRAY;
                    $tokens[$closer]['type'] = 'T_CLOSE_SHORT_ARRAY';
                    if (PHP_CODESNIFFER_VERBOSITY > 1) {
                        $line = $tokens[$i]['line'];
                        echo "\t* token $i on line $line changed from T_OPEN_SQUARE_BRACKET to T_OPEN_SHORT_ARRAY".PHP_EOL;
                        $line = $tokens[$closer]['line'];
                        echo "\t* token $closer on line $line changed from T_CLOSE_SQUARE_BRACKET to T_CLOSE_SHORT_ARRAY".PHP_EOL;
                    }
                }

                continue;
            } else if ($tokens[$i]['code'] === T_STATIC) {
                for ($x = ($i - 1); $x > 0; $x--) {
                    if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$x]['code']]) === false) {
                        break;
                    }
                }

                if ($tokens[$x]['code'] === T_INSTANCEOF) {
                    $tokens[$i]['code'] = T_STRING;
                    $tokens[$i]['type'] = 'T_STRING';

                    if (PHP_CODESNIFFER_VERBOSITY > 1) {
                        $line = $tokens[$i]['line'];
                        echo "\t* token $i on line $line changed from T_STATIC to T_STRING".PHP_EOL;
                    }
                }

                continue;
            } else if ($tokens[$i]['code'] === T_ECHO && $tokens[$i]['content'] === '<?=') {
                // HHVM tokenizes <?= as T_ECHO but it should be T_OPEN_TAG_WITH_ECHO.
                $tokens[$i]['code'] = T_OPEN_TAG_WITH_ECHO;
                $tokens[$i]['type'] = 'T_OPEN_TAG_WITH_ECHO';

                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    $line = $tokens[$i]['line'];
                    echo "\t* token $i on line $line changed from T_ECHO to T_OPEN_TAG_WITH_ECHO".PHP_EOL;
                }
            }//end if

            if (($tokens[$i]['code'] !== T_CASE
                && $tokens[$i]['code'] !== T_DEFAULT)
                || isset($tokens[$i]['scope_opener']) === false
            ) {
                // Only interested in CASE and DEFAULT statements from here on in.
                continue;
            }

            $scopeOpener = $tokens[$i]['scope_opener'];
            $scopeCloser = $tokens[$i]['scope_closer'];

            // If the first char after the opener is a curly brace
            // and that brace has been ignored, it is actually
            // opening this case statement and the opener and closer are
            // probably set incorrectly.
            for ($x = ($scopeOpener + 1); $x < $numTokens; $x++) {
                if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$x]['code']]) === false) {
                    // Non-whitespace content.
                    break;
                }
            }

            if ($tokens[$x]['code'] === T_CASE) {
                // Special case for multiple CASE statements that share the same
                // closer. Because we are going backwards through the file, this next
                // CASE statement is already fixed, so just use its closer and don't
                // worry about fixing anything.
                $newCloser = $tokens[$x]['scope_closer'];
                $tokens[$i]['scope_closer'] = $newCloser;
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    $oldType = $tokens[$scopeCloser]['type'];
                    $newType = $tokens[$newCloser]['type'];
                    $line    = $tokens[$i]['line'];
                    echo "\t* token $i (T_CASE) on line $line closer changed from $scopeCloser ($oldType) to $newCloser ($newType)".PHP_EOL;
                }

                continue;
            }

            if ($tokens[$x]['code'] !== T_OPEN_CURLY_BRACKET
                || isset($tokens[$x]['scope_condition']) === true
            ) {
                // Not a CASE with a curly brace opener.
                continue;
            }

            // The closer for this CASE/DEFAULT should be the closing curly brace and
            // not whatever it already is. The opener needs to be the opening curly
            // brace so everything matches up.
            $newCloser = $tokens[$x]['bracket_closer'];
            $tokens[$i]['scope_closer']            = $newCloser;
            $tokens[$x]['scope_closer']            = $newCloser;
            $tokens[$i]['scope_opener']            = $x;
            $tokens[$x]['scope_condition']         = $i;
            $tokens[$newCloser]['scope_condition'] = $i;
            $tokens[$newCloser]['scope_opener']    = $x;
            $tokens[$newCloser]['scope_closer']    = $newCloser;
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                $line      = $tokens[$i]['line'];
                $tokenType = $tokens[$i]['type'];

                $oldType = $tokens[$scopeOpener]['type'];
                $newType = $tokens[$x]['type'];
                echo "\t* token $i ($tokenType) on line $line opener changed from $scopeOpener ($oldType) to $x ($newType)".PHP_EOL;

                $oldType = $tokens[$scopeCloser]['type'];
                $newType = $tokens[$newCloser]['type'];
                echo "\t* token $i ($tokenType) on line $line closer changed from $scopeCloser ($oldType) to $newCloser ($newType)".PHP_EOL;
            }

            // Now fix up all the tokens that think they are
            // inside the CASE/DEFAULT statement when they are really outside.
            for ($x = $newCloser; $x < $scopeCloser; $x++) {
                foreach ($tokens[$x]['conditions'] as $num => $oldCond) {
                    if ($oldCond === $tokens[$i]['code']) {
                        $oldConditions = $tokens[$x]['conditions'];
                        unset($tokens[$x]['conditions'][$num]);

                        if (PHP_CODESNIFFER_VERBOSITY > 1) {
                            $type     = $tokens[$x]['type'];
                            $oldConds = '';
                            foreach ($oldConditions as $condition) {
                                $oldConds .= token_name($condition).',';
                            }

                            $oldConds = rtrim($oldConds, ',');

                            $newConds = '';
                            foreach ($tokens[$x]['conditions'] as $condition) {
                                $newConds .= token_name($condition).',';
                            }

                            $newConds = rtrim($newConds, ',');

                            echo "\t\t* cleaned $x ($type) *".PHP_EOL;
                            echo "\t\t\t=> conditions changed from $oldConds to $newConds".PHP_EOL;
                        }

                        break;
                    }//end if
                }//end foreach
            }//end for
        }//end for

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo "\t*** END ADDITIONAL PHP PROCESSING ***".PHP_EOL;
        }

    }//end processAdditional()


    /**
     * Takes a token produced from <code>token_get_all()</code> and produces a
     * more uniform token.
     *
     * @param string|array $token The token to convert.
     *
     * @return array The new token.
     */
    public static function standardiseToken($token)
    {
        if (isset($token[1]) === false) {
            if (isset(self::$_resolveTokenCache[$token[0]]) === true) {
                return self::$_resolveTokenCache[$token[0]];
            }
        } else {
            $cacheKey = null;
            if ($token[0] === T_STRING) {
                $cacheKey = strtolower($token[1]);
            } else if ($token[0] !== T_CURLY_OPEN) {
                $cacheKey = $token[0];
            }

            if ($cacheKey !== null && isset(self::$_resolveTokenCache[$cacheKey]) === true) {
                $newToken            = self::$_resolveTokenCache[$cacheKey];
                $newToken['content'] = $token[1];
                return $newToken;
            }
        }

        if (isset($token[1]) === false) {
            return self::resolveSimpleToken($token[0]);
        }

        if ($token[0] === T_STRING) {
            switch ($cacheKey) {
            case 'false':
                $newToken['type'] = 'T_FALSE';
                break;
            case 'true':
                $newToken['type'] = 'T_TRUE';
                break;
            case 'null':
                $newToken['type'] = 'T_NULL';
                break;
            case 'self':
                $newToken['type'] = 'T_SELF';
                break;
            case 'parent':
                $newToken['type'] = 'T_PARENT';
                break;
            default:
                $newToken['type'] = 'T_STRING';
                break;
            }

            $newToken['code'] = constant($newToken['type']);

            self::$_resolveTokenCache[$cacheKey] = $newToken;
        } else if ($token[0] === T_CURLY_OPEN) {
            $newToken = array(
                         'code' => T_OPEN_CURLY_BRACKET,
                         'type' => 'T_OPEN_CURLY_BRACKET',
                        );
        } else {
            $newToken = array(
                         'code' => $token[0],
                         'type' => token_name($token[0]),
                        );

            self::$_resolveTokenCache[$token[0]] = $newToken;
        }//end if

        $newToken['content'] = $token[1];
        return $newToken;

    }//end standardiseToken()


    /**
     * Converts simple tokens into a format that conforms to complex tokens
     * produced by token_get_all().
     *
     * Simple tokens are tokens that are not in array form when produced from
     * token_get_all().
     *
     * @param string $token The simple token to convert.
     *
     * @return array The new token in array format.
     */
    public static function resolveSimpleToken($token)
    {
        $newToken = array();

        switch ($token) {
        case '{':
            $newToken['type'] = 'T_OPEN_CURLY_BRACKET';
            break;
        case '}':
            $newToken['type'] = 'T_CLOSE_CURLY_BRACKET';
            break;
        case '[':
            $newToken['type'] = 'T_OPEN_SQUARE_BRACKET';
            break;
        case ']':
            $newToken['type'] = 'T_CLOSE_SQUARE_BRACKET';
            break;
        case '(':
            $newToken['type'] = 'T_OPEN_PARENTHESIS';
            break;
        case ')':
            $newToken['type'] = 'T_CLOSE_PARENTHESIS';
            break;
        case ':':
            $newToken['type'] = 'T_COLON';
            break;
        case '.':
            $newToken['type'] = 'T_STRING_CONCAT';
            break;
        case '?':
            $newToken['type'] = 'T_INLINE_THEN';
            break;
        case ';':
            $newToken['type'] = 'T_SEMICOLON';
            break;
        case '=':
            $newToken['type'] = 'T_EQUAL';
            break;
        case '*':
            $newToken['type'] = 'T_MULTIPLY';
            break;
        case '/':
            $newToken['type'] = 'T_DIVIDE';
            break;
        case '+':
            $newToken['type'] = 'T_PLUS';
            break;
        case '-':
            $newToken['type'] = 'T_MINUS';
            break;
        case '%':
            $newToken['type'] = 'T_MODULUS';
            break;
        case '^':
            $newToken['type'] = 'T_POWER';
            break;
        case '&':
            $newToken['type'] = 'T_BITWISE_AND';
            break;
        case '|':
            $newToken['type'] = 'T_BITWISE_OR';
            break;
        case '<':
            $newToken['type'] = 'T_LESS_THAN';
            break;
        case '>':
            $newToken['type'] = 'T_GREATER_THAN';
            break;
        case '!':
            $newToken['type'] = 'T_BOOLEAN_NOT';
            break;
        case ',':
            $newToken['type'] = 'T_COMMA';
            break;
        case '@':
            $newToken['type'] = 'T_ASPERAND';
            break;
        case '$':
            $newToken['type'] = 'T_DOLLAR';
            break;
        case '`':
            $newToken['type'] = 'T_BACKTICK';
            break;
        default:
            $newToken['type'] = 'T_NONE';
            break;
        }//end switch

        $newToken['code']    = constant($newToken['type']);
        $newToken['content'] = $token;

        self::$_resolveTokenCache[$token] = $newToken;
        return $newToken;

    }//end resolveSimpleToken()


}//end class
