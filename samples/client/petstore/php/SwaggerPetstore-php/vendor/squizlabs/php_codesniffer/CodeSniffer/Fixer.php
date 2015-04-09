<?php
/**
 * A helper class for fixing errors.
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
 * A helper class for fixing errors.
 *
 * Provides helper functions that act upon a token array and modify the file
 * content.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2012 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_Fixer
{

    /**
     * Is the fixer enabled and fixing a file?
     *
     * Sniffs should check this value to ensure they are not
     * doing extra processing to prepare for a fix when fixing is
     * not required.
     *
     * @var boolean
     */
    public $enabled = false;

    /**
     * The number of times we have looped over a file.
     *
     * @var int
     */
    public $loops = 0;

    /**
     * The file being fixed.
     *
     * @var PHP_CodeSniffer_File
     */
    private $_currentFile = null;

    /**
     * The list of tokens that make up the file contents.
     *
     * This is a simplified list which just contains the token content and nothing
     * else. This is the array that is updated as fixes are made, not the file's
     * token array. Imploding this array will give you the file content back.
     *
     * @var array(int => string)
     */
    private $_tokens = array();

    /**
     * A list of tokens that have already been fixed.
     *
     * We don't allow the same token to be fixed more than once each time
     * through a file as this can easily cause conflicts between sniffs.
     *
     * @var array(int)
     */
    private $_fixedTokens = array();

    /**
     * The last value of each fixed token.
     *
     * If a token is being "fixed" back to its last value, the fix is
     * probably conflicting with another.
     *
     * @var array(int => string)
     */
    private $_oldTokenValues = array();

    /**
     * A list of tokens that have been fixed during a changeset.
     *
     * All changes in changeset must be able to be applied, or else
     * the entire changeset is rejected.
     *
     * @var array()
     */
    private $_changeset = array();

    /**
     * Is there an open changeset.
     *
     * @var boolean
     */
    private $_inChangeset = false;

    /**
     * Is the current fixing loop in conflict?
     *
     * @var boolean
     */
    private $_inConflict = false;

    /**
     * The number of fixes that have been performed.
     *
     * @var int
     */
    private $_numFixes = 0;


    /**
     * Starts fixing a new file.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being fixed.
     *
     * @return void
     */
    public function startFile($phpcsFile)
    {
        $this->_currentFile = $phpcsFile;
        $this->_numFixes    = 0;
        $this->_fixedTokens = array();

        $tokens        = $phpcsFile->getTokens();
        $this->_tokens = array();
        foreach ($tokens as $index => $token) {
            if (isset($token['orig_content']) === true) {
                $this->_tokens[$index] = $token['orig_content'];
            } else {
                $this->_tokens[$index] = $token['content'];
            }
        }

    }//end startFile()


    /**
     * Attempt to fix the file by processing it until no fixes are made.
     *
     * @return boolean
     */
    public function fixFile()
    {
        $fixable = $this->_currentFile->getFixableCount();
        if ($fixable === 0) {
            // Nothing to fix.
            return false;
        }

        $stdin     = false;
        $cliValues = $this->_currentFile->phpcs->cli->getCommandLineValues();
        if (empty($cliValues['files']) === true) {
            $stdin = true;
        }

        $this->enabled = true;

        $this->loops = 0;
        while ($this->loops < 50) {
            ob_start();

            // Only needed once file content has changed.
            $contents = $this->getContents();

            if (PHP_CODESNIFFER_VERBOSITY > 2) {
                @ob_end_clean();
                echo '---START FILE CONTENT---'.PHP_EOL;
                $lines = explode($this->_currentFile->eolChar, $contents);
                $max   = strlen(count($lines));
                foreach ($lines as $lineNum => $line) {
                    $lineNum++;
                    echo str_pad($lineNum, $max, ' ', STR_PAD_LEFT).'|'.$line.PHP_EOL;
                }

                echo '--- END FILE CONTENT ---'.PHP_EOL;
                ob_start();
            }

            $this->_inConflict = false;
            $this->_currentFile->refreshTokenListeners();
            $this->_currentFile->start($contents);
            ob_end_clean();

            $this->loops++;

            if (PHP_CODESNIFFER_CBF === true && $stdin === false) {
                echo "\r".str_repeat(' ', 80)."\r";
                echo "\t=> Fixing file: $this->_numFixes/$fixable violations remaining [made $this->loops pass";
                if ($this->loops > 1) {
                    echo 'es';
                }

                echo ']... ';
            }

            if ($this->_numFixes === 0 && $this->_inConflict === false) {
                // Nothing left to do.
                break;
            } else if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo "\t* fixed $this->_numFixes violations, starting loop ".($this->loops + 1).' *'.PHP_EOL;
            }
        }//end while

        $this->enabled = false;

        if ($this->_numFixes > 0) {
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                @ob_end_clean();
                echo "\t*** Reached maximum number of loops with $this->_numFixes violations left unfixed ***".PHP_EOL;
                ob_start();
            }

            return false;
        }

        return true;

    }//end fixFile()


    /**
     * Generates a text diff of the original file and the new content.
     *
     * @param string  $filePath Optional file path to diff the file against.
     *                          If not specified, the original version of the
     *                          file will be used.
     * @param boolean $colors   Print colored output or not.
     *
     * @return string
     */
    public function generateDiff($filePath=null, $colors=true)
    {
        if ($filePath === null) {
            $filePath = $this->_currentFile->getFilename();
        }

        $cwd      = getcwd().DIRECTORY_SEPARATOR;
        $filename = str_replace($cwd, '', $filePath);
        $contents = $this->getContents();

        if (function_exists('sys_get_temp_dir') === true) {
            // This is needed for HHVM support, but only available from 5.2.1.
            $tempName  = tempnam(sys_get_temp_dir(), 'phpcs-fixer');
            $fixedFile = fopen($tempName, 'w');
        } else {
            $fixedFile = tmpfile();
            $data      = stream_get_meta_data($fixedFile);
            $tempName  = $data['uri'];
        }

        fwrite($fixedFile, $contents);

        // We must use something like shell_exec() because whitespace at the end
        // of lines is critical to diff files.
        $cmd  = "diff -u -L\"$filename\" -LPHP_CodeSniffer \"$filename\" \"$tempName\"";
        $diff = shell_exec($cmd);

        fclose($fixedFile);
        if (is_file($tempName) === true) {
            unlink($tempName);
        }

        if ($colors === false) {
            return $diff;
        }

        $diffLines = explode(PHP_EOL, $diff);
        if (count($diffLines) === 1) {
            // Seems to be required for cygwin.
            $diffLines = explode("\n", $diff);
        }

        $diff = array();
        foreach ($diffLines as $line) {
            if (isset($line[0]) === true) {
                switch ($line[0]) {
                case '-':
                    $diff[] = "\033[31m$line\033[0m";
                    break;
                case '+':
                    $diff[] = "\033[32m$line\033[0m";
                    break;
                default:
                    $diff[] = $line;
                }
            }
        }

        $diff = implode(PHP_EOL, $diff);

        return $diff;

    }//end generateDiff()


    /**
     * Get a count of fixes that have been performed on the file.
     *
     * This value is reset every time a new file is started, or an existing
     * file is restarted.
     *
     * @return int
     */
    public function getFixCount()
    {
        return $this->_numFixes;

    }//end getFixCount()


    /**
     * Get the current content of the file, as a string.
     *
     * @return string
     */
    public function getContents()
    {
        $contents = implode($this->_tokens);
        return $contents;

    }//end getContents()


    /**
     * Get the current fixed content of a token.
     *
     * This function takes changesets into account so should be used
     * instead of directly accessing the token array.
     *
     * @param int $stackPtr The position of the token in the token stack.
     *
     * @return string
     */
    public function getTokenContent($stackPtr)
    {
        if ($this->_inChangeset === true
            && isset($this->_changeset[$stackPtr]) === true
        ) {
            return $this->_changeset[$stackPtr];
        } else {
            return $this->_tokens[$stackPtr];
        }

    }//end getTokenContent()


    /**
     * Start recording actions for a changeset.
     *
     * @return void
     */
    public function beginChangeset()
    {
        if ($this->_inConflict === true) {
            return false;
        }

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            $bt    = debug_backtrace();
            $sniff = $bt[1]['class'];
            $line  = $bt[0]['line'];

            @ob_end_clean();
            echo "\t=> Changeset started by $sniff (line $line)".PHP_EOL;
            ob_start();
        }

        $this->_changeset   = array();
        $this->_inChangeset = true;

    }//end beginChangeset()


    /**
     * Stop recording actions for a changeset, and apply logged changes.
     *
     * @return boolean
     */
    public function endChangeset()
    {
        if ($this->_inConflict === true) {
            return false;
        }

        $this->_inChangeset = false;

        $success = true;
        $applied = array();
        foreach ($this->_changeset as $stackPtr => $content) {
            $success = $this->replaceToken($stackPtr, $content);
            if ($success === false) {
                break;
            } else {
                $applied[] = $stackPtr;
            }
        }

        if ($success === false) {
            // Rolling back all changes.
            foreach ($applied as $stackPtr) {
                $this->revertToken($stackPtr);
            }

            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                @ob_end_clean();
                echo "\t=> Changeset failed to apply".PHP_EOL;
                ob_start();
            }
        } else if (PHP_CODESNIFFER_VERBOSITY > 1) {
            $fixes = count($this->_changeset);
            @ob_end_clean();
            echo "\t=> Changeset ended: $fixes changes applied".PHP_EOL;
            ob_start();
        }

        $this->_changeset = array();

    }//end endChangeset()


    /**
     * Replace the entire contents of a token.
     *
     * @param int    $stackPtr The position of the token in the token stack.
     * @param string $content  The new content of the token.
     *
     * @return bool If the change was accepted.
     */
    public function replaceToken($stackPtr, $content)
    {
        if ($this->_inConflict === true) {
            return false;
        }

        if ($this->_inChangeset === false
            && isset($this->_fixedTokens[$stackPtr]) === true
        ) {
            $indent = "\t";
            if (empty($this->_changeset) === false) {
                $indent .= "\t";
            }

            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                @ob_end_clean();
                echo "$indent* token $stackPtr has already been modified, skipping *".PHP_EOL;
                ob_start();
            }

            return false;
        }

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            $bt = debug_backtrace();
            if ($bt[1]['class'] === 'PHP_CodeSniffer_Fixer') {
                $sniff = $bt[2]['class'];
                $line  = $bt[1]['line'];
            } else {
                $sniff = $bt[1]['class'];
                $line  = $bt[0]['line'];
            }

            $tokens     = $this->_currentFile->getTokens();
            $type       = $tokens[$stackPtr]['type'];
            $oldContent = PHP_CodeSniffer::prepareForOutput($this->_tokens[$stackPtr]);
            $newContent = PHP_CodeSniffer::prepareForOutput($content);
            if (trim($this->_tokens[$stackPtr]) === '' && isset($this->_tokens[($stackPtr + 1)]) === true) {
                // Add some context for whitespace only changes.
                $append      = PHP_CodeSniffer::prepareForOutput($this->_tokens[($stackPtr + 1)]);
                $oldContent .= $append;
                $newContent .= $append;
            }
        }//end if

        if ($this->_inChangeset === true) {
            $this->_changeset[$stackPtr] = $content;

            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                @ob_end_clean();
                echo "\t\tQ: $sniff (line $line) replaced token $stackPtr ($type) \"$oldContent\" => \"$newContent\"".PHP_EOL;
                ob_start();
            }

            return true;
        }

        if (isset($this->_oldTokenValues[$stackPtr]) === false) {
            $this->_oldTokenValues[$stackPtr] = array(
                                                 'curr' => $content,
                                                 'prev' => $this->_tokens[$stackPtr],
                                                 'loop' => $this->loops,
                                                );
        } else {
            if ($this->_oldTokenValues[$stackPtr]['prev'] === $content) {
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    $indent = "\t";
                    if (empty($this->_changeset) === false) {
                        $indent .= "\t";
                    }

                    $loop = $this->_oldTokenValues[$stackPtr]['loop'];

                    @ob_end_clean();
                    echo "$indent**** $sniff (line $line) has possible conflict with another sniff on loop $loop; caused by the following change ****".PHP_EOL;
                    echo "$indent**** replaced token $stackPtr ($type) \"$oldContent\" => \"$newContent\" ****".PHP_EOL;
                }

                if ($this->_oldTokenValues[$stackPtr]['loop'] >= ($this->loops - 1)) {
                    $this->_inConflict = true;
                    if (PHP_CODESNIFFER_VERBOSITY > 1) {
                        echo "$indent**** ignoring all changes until next loop ****".PHP_EOL;
                    }
                }

                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    ob_start();
                }

                return false;
            }//end if

            $this->_oldTokenValues[$stackPtr]['prev'] = $this->_oldTokenValues[$stackPtr]['curr'];
            $this->_oldTokenValues[$stackPtr]['curr'] = $content;
            $this->_oldTokenValues[$stackPtr]['loop'] = $this->loops;
        }//end if

        $this->_fixedTokens[$stackPtr] = $this->_tokens[$stackPtr];
        $this->_tokens[$stackPtr]      = $content;
        $this->_numFixes++;

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            $indent = "\t";
            if (empty($this->_changeset) === false) {
                $indent .= "\tA: ";
            }

            @ob_end_clean();
            echo "$indent$sniff (line $line) replaced token $stackPtr ($type) \"$oldContent\" => \"$newContent\"".PHP_EOL;
            ob_start();
        }

        return true;

    }//end replaceToken()


    /**
     * Reverts the previous fix made to a token.
     *
     * @param int $stackPtr The position of the token in the token stack.
     *
     * @return bool If a change was reverted.
     */
    public function revertToken($stackPtr)
    {
        if (isset($this->_fixedTokens[$stackPtr]) === false) {
            return false;
        }

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            $bt = debug_backtrace();
            if ($bt[1]['class'] === 'PHP_CodeSniffer_Fixer') {
                $sniff = $bt[2]['class'];
                $line  = $bt[1]['line'];
            } else {
                $sniff = $bt[1]['class'];
                $line  = $bt[0]['line'];
            }

            $tokens     = $this->_currentFile->getTokens();
            $type       = $tokens[$stackPtr]['type'];
            $oldContent = PHP_CodeSniffer::prepareForOutput($this->_tokens[$stackPtr]);
            $newContent = PHP_CodeSniffer::prepareForOutput($this->_fixedTokens[$stackPtr]);
            if (trim($this->_tokens[$stackPtr]) === '' && isset($tokens[($stackPtr + 1)]) === true) {
                // Add some context for whitespace only changes.
                $append      = PHP_CodeSniffer::prepareForOutput($this->_tokens[($stackPtr + 1)]);
                $oldContent .= $append;
                $newContent .= $append;
            }
        }//end if

        $this->_tokens[$stackPtr] = $this->_fixedTokens[$stackPtr];
        unset($this->_fixedTokens[$stackPtr]);
        $this->_numFixes--;

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            $indent = "\t";
            if (empty($this->_changeset) === false) {
                $indent .= "\tR: ";
            }

            @ob_end_clean();
            echo "$indent$sniff (line $line) reverted token $stackPtr ($type) \"$oldContent\" => \"$newContent\"".PHP_EOL;
            ob_start();
        }

        return true;

    }//end revertToken()


    /**
     * Replace the content of a token with a part of its current content.
     *
     * @param int $stackPtr The position of the token in the token stack.
     * @param int $start    The first character to keep.
     * @param int $length   The number of chacters to keep. If NULL, the content of
     *                      the token from $start to the end of the content is kept.
     *
     * @return bool If the change was accepted.
     */
    public function substrToken($stackPtr, $start, $length=null)
    {
        $current = $this->getTokenContent($stackPtr);

        if ($length === null) {
            $newContent = substr($current, $start);
        } else {
            $newContent = substr($current, $start, $length);
        }

        return $this->replaceToken($stackPtr, $newContent);

    }//end substrToken()


    /**
     * Adds a newline to end of a token's content.
     *
     * @param int $stackPtr The position of the token in the token stack.
     *
     * @return bool If the change was accepted.
     */
    public function addNewline($stackPtr)
    {
        $current = $this->getTokenContent($stackPtr);
        return $this->replaceToken($stackPtr, $current.$this->_currentFile->eolChar);

    }//end addNewline()


    /**
     * Adds a newline to the start of a token's content.
     *
     * @param int $stackPtr The position of the token in the token stack.
     *
     * @return bool If the change was accepted.
     */
    public function addNewlineBefore($stackPtr)
    {
        $current = $this->getTokenContent($stackPtr);
        return $this->replaceToken($stackPtr, $this->_currentFile->eolChar.$current);

    }//end addNewlineBefore()


    /**
     * Adds content to the end of a token's current content.
     *
     * @param int    $stackPtr The position of the token in the token stack.
     * @param string $content  The content to add.
     *
     * @return bool If the change was accepted.
     */
    public function addContent($stackPtr, $content)
    {
        $current = $this->getTokenContent($stackPtr);
        return $this->replaceToken($stackPtr, $current.$content);

    }//end addContent()


    /**
     * Adds content to the start of a token's current content.
     *
     * @param int    $stackPtr The position of the token in the token stack.
     * @param string $content  The content to add.
     *
     * @return bool If the change was accepted.
     */
    public function addContentBefore($stackPtr, $content)
    {
        $current = $this->getTokenContent($stackPtr);
        return $this->replaceToken($stackPtr, $content.$current);

    }//end addContentBefore()


}//end class
