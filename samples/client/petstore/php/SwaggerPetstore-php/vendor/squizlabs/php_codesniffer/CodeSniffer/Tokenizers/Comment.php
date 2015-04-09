<?php
/**
 * Tokenizes doc block comments.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2012 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Tokenizes doc block comments.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2012 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_Tokenizers_Comment
{


    /**
     * Creates an array of tokens when given some PHP code.
     *
     * Starts by using token_get_all() but does a lot of extra processing
     * to insert information about the context of the token.
     *
     * @param string $string   The string to tokenize.
     * @param string $eolChar  The EOL character to use for splitting strings.
     * @param int    $stackPtr The position of the first token in the file.
     *
     * @return array
     */
    public function tokenizeString($string, $eolChar, $stackPtr)
    {
        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo "\t\t*** START COMMENT TOKENIZING ***".PHP_EOL;
        }

        $tokens   = array();
        $numChars = strlen($string);

        /*
            Doc block comments start with /*, but typically contain an
            extra star when they are used for function and class comments.
        */

        for ($c = 0; $c < $numChars; $c++) {
            if ($string[$c] !== '/' && $string[$c] !== '*') {
                break;
            }
        }

        $openTag           = substr($string, 0, $c);
        $tokens[$stackPtr] = array(
                              'content'      => $openTag,
                              'code'         => T_DOC_COMMENT_OPEN_TAG,
                              'type'         => 'T_DOC_COMMENT_OPEN_TAG',
                              'comment_tags' => array(),
                             );

        $openPtr = $stackPtr;
        $stackPtr++;

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            $content = PHP_CodeSniffer::prepareForOutput($openTag);
            echo "\t\tCreate comment token: T_DOC_COMMENT_OPEN_TAG => $content".PHP_EOL;
        }

        /*
            Strip off the close tag so it doesn't interfere with any
            of our comment line processing. The token will be added to the
            stack just before we return it.
        */

        for ($i = ($numChars - 1); $i > $c; $i--) {
            if ($string[$i] !== '/' && $string[$i] !== '*') {
                break;
            }
        }

        $i++;
        $closeTag = array(
                     'content'        => substr($string, $i),
                     'code'           => T_DOC_COMMENT_CLOSE_TAG,
                     'type'           => 'T_DOC_COMMENT_CLOSE_TAG',
                     'comment_opener' => $openPtr,
                    );

        $string   = substr($string, 0, $i);
        $numChars = strlen($string);

        /*
            Process each line of the comment.
        */

        while ($c < $numChars) {
            $lineTokens = $this->_processLine($string, $eolChar, $c, $numChars);
            foreach ($lineTokens as $lineToken) {
                $tokens[$stackPtr] = $lineToken;
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    $content = PHP_CodeSniffer::prepareForOutput($lineToken['content']);
                    $type    = $lineToken['type'];
                    echo "\t\tCreate comment token: $type => $content".PHP_EOL;
                }

                if ($lineToken['code'] === T_DOC_COMMENT_TAG) {
                    $tokens[$openPtr]['comment_tags'][] = $stackPtr;
                }

                $c += strlen($lineToken['content']);
                $stackPtr++;
            }

            if ($c === $numChars) {
                break;
            }

            // We've started a new line, so process the indent.
            $space = $this->_collectWhitespace($string, $c, $numChars);
            if ($space !== null) {
                $tokens[$stackPtr] = $space;
                $stackPtr++;
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    $content = PHP_CodeSniffer::prepareForOutput($space['content']);
                    $type    = $lineToken['type'];
                    echo "\t\tCreate comment token: T_DOC_COMMENT_WHITESPACE => $content".PHP_EOL;
                }

                $c += strlen($space['content']);
                if ($c === $numChars) {
                    break;
                }
            }

            if ($string[$c] === '*') {
                // This is a function or class doc block line.
                $c++;
                $tokens[$stackPtr] = array(
                                      'content' => '*',
                                      'code'    => T_DOC_COMMENT_STAR,
                                      'type'    => 'T_DOC_COMMENT_STAR',
                                     );

                $stackPtr++;

                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo "\t\tCreate comment token: T_DOC_COMMENT_STAR => *".PHP_EOL;
                }
            }

            // Now we are ready to process the actual content of the line.
            // So round we go.
        }//end while

        $tokens[$stackPtr] = $closeTag;
        $tokens[$openPtr]['comment_closer'] = $stackPtr;
        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            $content = PHP_CodeSniffer::prepareForOutput($closeTag['content']);
            echo "\t\tCreate comment token: T_DOC_COMMENT_CLOSE_TAG => $content".PHP_EOL;
        }

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo "\t\t*** END COMMENT TOKENIZING ***".PHP_EOL;
        }

        return $tokens;

    }//end tokenizeString()


    /**
     * Process a single line of a comment.
     *
     * @param string $string  The comment string being tokenized.
     * @param string $eolChar The EOL character to use for splitting strings.
     * @param int    $start   The position in the string to start processing.
     * @param int    $end     The position in the string to end processing.
     *
     * @return array
     */
    private function _processLine($string, $eolChar, $start, $end)
    {
        $tokens = array();

        // Collect content padding.
        $space = $this->_collectWhitespace($string, $start, $end);
        if ($space !== null) {
            $tokens[] = $space;
            $start   += strlen($space['content']);
        }

        if (isset($string[$start]) === false) {
            return $tokens;
        }

        if ($string[$start] === '@') {
            // The content up until the first whitespace is the tag name.
            $matches = array();
            preg_match('/@[^\s]+/', $string, $matches, 0, $start);
            if (isset($matches[0]) === true) {
                $tagName  = $matches[0];
                $start   += strlen($tagName);
                $tokens[] = array(
                             'content' => $tagName,
                             'code'    => T_DOC_COMMENT_TAG,
                             'type'    => 'T_DOC_COMMENT_TAG',
                            );

                // Then there will be some whitespace.
                $space = $this->_collectWhitespace($string, $start, $end);
                if ($space !== null) {
                    $tokens[] = $space;
                    $start   += strlen($space['content']);
                }
            }
        }//end if

        // Process the rest of the line.
        $eol = strpos($string, $eolChar, $start);
        if ($eol === false) {
            $eol = $end;
        }

        if ($eol > $start) {
            $tokens[] = array(
                         'content' => substr($string, $start, ($eol - $start)),
                         'code'    => T_DOC_COMMENT_STRING,
                         'type'    => 'T_DOC_COMMENT_STRING',
                        );
        }

        if ($eol !== $end) {
            $tokens[] = array(
                         'content' => substr($string, $eol, strlen($eolChar)),
                         'code'    => T_DOC_COMMENT_WHITESPACE,
                         'type'    => 'T_DOC_COMMENT_WHITESPACE',
                        );
        }

        return $tokens;

    }//end _processLine()


    /**
     * Collect consecutive whitespace into a single token.
     *
     * @param string $string The comment string being tokenized.
     * @param int    $start  The position in the string to start processing.
     * @param int    $end    The position in the string to end processing.
     *
     * @return array|null
     */
    private function _collectWhitespace($string, $start, $end)
    {
        $space = '';
        for ($start; $start < $end; $start++) {
            if ($string[$start] !== ' ' && $string[$start] !== "\t") {
                break;
            }

            $space .= $string[$start];
        }

        if ($space === '') {
            return null;
        }

        $token = array(
                  'content' => $space,
                  'code'    => T_DOC_COMMENT_WHITESPACE,
                  'type'    => 'T_DOC_COMMENT_WHITESPACE',
                 );

        return $token;

    }//end _collectWhitespace()


}//end class
