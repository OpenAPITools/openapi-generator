<?php
/**
 * Generic_Sniffs_Commenting_FixmeSniff.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Sam Graham <php-codesniffer@illusori.co.uk>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Generic_Sniffs_Commenting_FixmeSniff.
 *
 * Warns about FIXME comments.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Sam Graham <php-codesniffer@illusori.co.uk>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_Commenting_FixmeSniff implements PHP_CodeSniffer_Sniff
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
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return PHP_CodeSniffer_Tokens::$commentTokens;

    }//end register()


    /**
     * Processes this sniff, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token
     *                                        in the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        $content = $tokens[$stackPtr]['content'];
        $matches = array();
        if (preg_match('/(?:\A|[^\p{L}]+)fixme([^\p{L}]+(.*)|\Z)/ui', $content, $matches) !== 0) {
            // Clear whitespace and some common characters not required at
            // the end of a fixme message to make the error more informative.
            $type         = 'CommentFound';
            $fixmeMessage = trim($matches[1]);
            $fixmeMessage = trim($fixmeMessage, '-:[](). ');
            $error        = 'Comment refers to a FIXME task';
            $data         = array($fixmeMessage);
            if ($fixmeMessage !== '') {
                $type   = 'TaskFound';
                $error .= ' "%s"';
            }

            $phpcsFile->addError($error, $stackPtr, $type, $data);
        }

    }//end process()


}//end class
