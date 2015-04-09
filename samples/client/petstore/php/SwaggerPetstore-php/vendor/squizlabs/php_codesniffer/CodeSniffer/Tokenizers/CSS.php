<?php
/**
 * Tokenizes CSS code.
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

if (class_exists('PHP_CodeSniffer_Tokenizers_PHP', true) === false) {
    throw new Exception('Class PHP_CodeSniffer_Tokenizers_PHP not found');
}

/**
 * Tokenizes CSS code.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_Tokenizers_CSS extends PHP_CodeSniffer_Tokenizers_PHP
{


    /**
     * Creates an array of tokens when given some CSS code.
     *
     * Uses the PHP tokenizer to do all the tricky work
     *
     * @param string $string  The string to tokenize.
     * @param string $eolChar The EOL character to use for splitting strings.
     *
     * @return array
     */
    public function tokenizeString($string, $eolChar='\n')
    {
        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo "\t*** START CSS TOKENIZING ***".PHP_EOL;
        }

        // If the content doesn't have an EOL char on the end, add one so
        // the open and close tags we add are parsed correctly.
        $eolAdded = false;
        if (substr($string, (strlen($eolChar) * -1)) !== $eolChar) {
            $string  .= $eolChar;
            $eolAdded = true;
        }

        $string = str_replace('<?php', '^PHPCS_CSS_T_OPEN_TAG^', $string);
        $string = str_replace('?>', '^PHPCS_CSS_T_CLOSE_TAG^', $string);
        $tokens = parent::tokenizeString('<?php '.$string.'?>', $eolChar);

        $finalTokens    = array();
        $finalTokens[0] = array(
                           'code'    => T_OPEN_TAG,
                           'type'    => 'T_OPEN_TAG',
                           'content' => '',
                          );

        $newStackPtr      = 1;
        $numTokens        = count($tokens);
        $multiLineComment = false;
        for ($stackPtr = 1; $stackPtr < $numTokens; $stackPtr++) {
            $token = $tokens[$stackPtr];

            // CSS files don't have lists or break tags, so convert these to
            // standard strings early so they can be converted into T_STYLE
            // tokens and joined with other strings if needed.
            if ($token['code'] === T_BREAK || $token['code'] === T_LIST) {
                $token['type'] = 'T_STRING';
                $token['code'] = T_STRING;
            }

            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                $type    = $token['type'];
                $content = PHP_CodeSniffer::prepareForOutput($token['content']);
                echo "\tProcess token $stackPtr: $type => $content".PHP_EOL;
            }

            if ($token['code'] === T_POWER
                && $tokens[($stackPtr + 1)]['content'] === 'PHPCS_CSS_T_OPEN_TAG'
            ) {
                $content = '<?php';
                for ($stackPtr = ($stackPtr + 3); $stackPtr < $numTokens; $stackPtr++) {
                    if ($tokens[$stackPtr]['code'] === T_POWER
                        && $tokens[($stackPtr + 1)]['content'] === 'PHPCS_CSS_T_CLOSE_TAG'
                    ) {
                        // Add the end tag and ignore the * we put at the end.
                        $content  .= '?>';
                        $stackPtr += 2;
                        break;
                    } else {
                        $content .= $tokens[$stackPtr]['content'];
                    }
                }

                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo "\t\t=> Found embedded PHP code: ";
                    $cleanContent = PHP_CodeSniffer::prepareForOutput($content);
                    echo $cleanContent.PHP_EOL;
                }

                $finalTokens[$newStackPtr] = array(
                                              'type'    => 'T_EMBEDDED_PHP',
                                              'code'    => T_EMBEDDED_PHP,
                                              'content' => $content,
                                             );

                $newStackPtr++;
                continue;
            }//end if

            if ($token['code'] === T_GOTO_LABEL) {
                // Convert these back to T_STRING followed by T_COLON so we can
                // more easily process style definitions.
                $finalTokens[$newStackPtr] = array(
                                              'type'    => 'T_STRING',
                                              'code'    => T_STRING,
                                              'content' => substr($token['content'], 0, -1),
                                             );
                $newStackPtr++;
                $finalTokens[$newStackPtr] = array(
                                              'type'    => 'T_COLON',
                                              'code'    => T_COLON,
                                              'content' => ':',
                                             );
                $newStackPtr++;
                continue;
            }

            if ($token['code'] === T_FUNCTION) {
                // There are no functions in CSS, so convert this to a string.
                $finalTokens[$newStackPtr] = array(
                                              'type'    => 'T_STRING',
                                              'code'    => T_STRING,
                                              'content' => $token['content'],
                                             );

                $newStackPtr++;
                continue;
            }

            if ($token['code'] === T_COMMENT
                && substr($token['content'], 0, 2) === '/*'
            ) {
                // Multi-line comment. Record it so we can ignore other
                // comment tags until we get out of this one.
                $multiLineComment = true;
            }

            if ($token['code'] === T_COMMENT
                && $multiLineComment === false
                && (substr($token['content'], 0, 2) === '//'
                || $token['content']{0} === '#')
            ) {
                $content       = ltrim($token['content'], '#/');
                $commentTokens
                    = parent::tokenizeString('<?php '.$content.'?>', $eolChar);

                // The first and last tokens are the open/close tags.
                array_shift($commentTokens);
                array_pop($commentTokens);

                if ($token['content']{0} === '#') {
                    // The # character is not a comment in CSS files, so
                    // determine what it means in this context.
                    $firstContent = $commentTokens[0]['content'];

                    // If the first content is just a number, it is probably a
                    // colour like 8FB7DB, which PHP splits into 8 and FB7DB.
                    if (($commentTokens[0]['code'] === T_LNUMBER
                        || $commentTokens[0]['code'] === T_DNUMBER)
                        && $commentTokens[1]['code'] === T_STRING
                    ) {
                        $firstContent .= $commentTokens[1]['content'];
                        array_shift($commentTokens);
                    }

                    // If the first content looks like a colour and not a class
                    // definition, join the tokens together.
                    if (preg_match('/^[ABCDEF0-9]+$/i', $firstContent) === 1
                        && $commentTokens[1]['content'] !== '-'
                    ) {
                        array_shift($commentTokens);
                        // Work out what we trimmed off above and remember to re-add it.
                        $trimmed = substr($token['content'], 0, (strlen($token['content']) - strlen($content)));
                        $finalTokens[$newStackPtr] = array(
                                                      'type'    => 'T_COLOUR',
                                                      'code'    => T_COLOUR,
                                                      'content' => $trimmed.$firstContent,
                                                     );
                    } else {
                        $finalTokens[$newStackPtr] = array(
                                                      'type'    => 'T_HASH',
                                                      'code'    => T_HASH,
                                                      'content' => '#',
                                                     );
                    }
                } else {
                    $finalTokens[$newStackPtr] = array(
                                                  'type'    => 'T_STRING',
                                                  'code'    => T_STRING,
                                                  'content' => '//',
                                                 );
                }//end if

                $newStackPtr++;

                foreach ($commentTokens as $tokenData) {
                    if ($tokenData['code'] === T_COMMENT
                        && (substr($tokenData['content'], 0, 2) === '//'
                        || $tokenData['content']{0} === '#')
                    ) {
                        // This is a comment in a comment, so it needs
                        // to go through the whole process again.
                        $tokens[$stackPtr]['content'] = $tokenData['content'];
                        $stackPtr--;
                        break;
                    }

                    $finalTokens[$newStackPtr] = $tokenData;
                    $newStackPtr++;
                }

                continue;
            }//end if

            if ($token['code'] === T_COMMENT
                && substr($token['content'], -2) === '*/'
            ) {
                // Multi-line comment is done.
                $multiLineComment = false;
            }

            $finalTokens[$newStackPtr] = $token;
            $newStackPtr++;
        }//end for

        // A flag to indicate if we are inside a style definition,
        // which is defined using curly braces. I'm assuming you can't
        // have nested curly brackets.
        $inStyleDef = false;

        $numTokens = count($finalTokens);
        for ($stackPtr = 0; $stackPtr < $numTokens; $stackPtr++) {
            $token = $finalTokens[$stackPtr];

            switch ($token['code']) {
            case T_OPEN_CURLY_BRACKET:
                $inStyleDef = true;
                break;
            case T_CLOSE_CURLY_BRACKET:
                $inStyleDef = false;
                break;
            case T_MINUS:
                // Minus signs are often used instead of spaces inside
                // class names, IDs and styles.
                if ($finalTokens[($stackPtr + 1)]['code'] === T_STRING) {
                    if ($finalTokens[($stackPtr - 1)]['code'] === T_STRING) {
                        $newContent = $finalTokens[($stackPtr - 1)]['content'].'-'.$finalTokens[($stackPtr + 1)]['content'];

                        $finalTokens[($stackPtr - 1)]['content'] = $newContent;
                        unset($finalTokens[$stackPtr]);
                        unset($finalTokens[($stackPtr + 1)]);
                        $stackPtr -= 2;
                    } else {
                        $newContent = '-'.$finalTokens[($stackPtr + 1)]['content'];

                        $finalTokens[($stackPtr + 1)]['content'] = $newContent;
                        unset($finalTokens[$stackPtr]);
                        $stackPtr--;
                    }

                    $finalTokens = array_values($finalTokens);
                    $numTokens   = count($finalTokens);
                } else if ($finalTokens[($stackPtr + 1)]['code'] === T_LNUMBER) {
                    // They can also be used to provide negative numbers.
                    $finalTokens[($stackPtr + 1)]['content']
                        = '-'.$finalTokens[($stackPtr + 1)]['content'];
                    unset($finalTokens[$stackPtr]);

                    $finalTokens = array_values($finalTokens);
                    $numTokens   = count($finalTokens);
                }//end if

                break;
            case T_COLON:
                // Only interested in colons that are defining styles.
                if ($inStyleDef === false) {
                    break;
                }

                for ($x = ($stackPtr - 1); $x >= 0; $x--) {
                    if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$finalTokens[$x]['code']]) === false) {
                        break;
                    }
                }

                $finalTokens[$x]['type'] = 'T_STYLE';
                $finalTokens[$x]['code'] = T_STYLE;
                break;
            case T_STRING:
                if (strtolower($token['content']) === 'url') {
                    // Find the next content.
                    for ($x = ($stackPtr + 1); $x < $numTokens; $x++) {
                        if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$finalTokens[$x]['code']]) === false) {
                            break;
                        }
                    }

                    // Needs to be in the format "url(" for it to be a URL.
                    if ($finalTokens[$x]['code'] !== T_OPEN_PARENTHESIS) {
                        continue;
                    }

                    // Make sure the content isn't empty.
                    for ($y = ($x + 1); $y < $numTokens; $y++) {
                        if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$finalTokens[$y]['code']]) === false) {
                            break;
                        }
                    }

                    if ($finalTokens[$y]['code'] === T_CLOSE_PARENTHESIS) {
                        continue;
                    }

                    // Join all the content together inside the url() statement.
                    $newContent = '';
                    for ($i = ($x + 2); $i < $numTokens; $i++) {
                        if ($finalTokens[$i]['code'] === T_CLOSE_PARENTHESIS) {
                            break;
                        }

                        $newContent .= $finalTokens[$i]['content'];
                        unset($finalTokens[$i]);
                    }

                    // If the content inside the "url()" is in double quotes
                    // there will only be one token and so we don't have to do
                    // anything except change its type. If it is not empty,
                    // we need to do some token merging.
                    $finalTokens[($x + 1)]['type'] = 'T_URL';
                    $finalTokens[($x + 1)]['code'] = T_URL;

                    if ($newContent !== '') {
                        $finalTokens[($x + 1)]['content'] .= $newContent;

                        $finalTokens = array_values($finalTokens);
                        $numTokens   = count($finalTokens);
                    }
                }//end if

                break;
            default:
                // Nothing special to be done with this token.
                break;
            }//end switch
        }//end for

        // Blank out the content of the end tag.
        $finalTokens[($numTokens - 1)]['content'] = '';

        if ($eolAdded === true) {
            // Strip off the extra EOL char we added for tokenizing.
            $finalTokens[($numTokens - 2)]['content'] = substr(
                $finalTokens[($numTokens - 2)]['content'],
                0,
                (strlen($eolChar) * -1)
            );

            if ($finalTokens[($numTokens - 2)]['content'] === '') {
                unset($finalTokens[($numTokens - 2)]);
                $finalTokens = array_values($finalTokens);
                $numTokens   = count($finalTokens);
            }
        }

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo "\t*** END CSS TOKENIZING ***".PHP_EOL;
        }

        return $finalTokens;

    }//end tokenizeString()


    /**
     * Performs additional processing after main tokenizing.
     *
     * @param array  $tokens  The array of tokens to process.
     * @param string $eolChar The EOL character to use for splitting strings.
     *
     * @return void
     */
    public function processAdditional(&$tokens, $eolChar)
    {
        /*
            We override this method because we don't want the PHP version to
            run during CSS processing because it is wasted processing time.
        */

    }//end processAdditional()


}//end class
