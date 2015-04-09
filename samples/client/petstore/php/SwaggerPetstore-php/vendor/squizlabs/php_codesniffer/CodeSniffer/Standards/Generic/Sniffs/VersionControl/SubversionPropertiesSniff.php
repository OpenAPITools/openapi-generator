<?php
/**
 * Generic_Sniffs_VersionControl_SubversionPropertiesSniff.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Jack Bates <ms419@freezone.co.uk>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Generic_Sniffs_VersionControl_SubversionPropertiesSniff.
 *
 * Tests that the correct Subversion properties are set.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Jack Bates <ms419@freezone.co.uk>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_VersionControl_SubversionPropertiesSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * The Subversion properties that should be set.
     *
     * Key of array is the SVN property and the value is the
     * exact value the property should have or NULL if the
     * property should just be set but the value is not fixed.
     *
     * @var array
     */
    protected $properties = array(
                             'svn:keywords'  => 'Author Id Revision',
                             'svn:eol-style' => 'native',
                            );


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_OPEN_TAG);

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
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

        // Make sure this is the first PHP open tag so we don't process the
        // same file twice.
        $prevOpenTag = $phpcsFile->findPrevious(T_OPEN_TAG, ($stackPtr - 1));
        if ($prevOpenTag !== false) {
            return;
        }

        $path       = $phpcsFile->getFileName();
        $properties = $this->getProperties($path);
        if ($properties === null) {
            // Not under version control.
            return;
        }

        $allProperties = ($properties + $this->properties);
        foreach ($allProperties as $key => $value) {
            if (isset($properties[$key]) === true
                && isset($this->properties[$key]) === false
            ) {
                $error = 'Unexpected Subversion property "%s" = "%s"';
                $data  = array(
                          $key,
                          $properties[$key],
                         );
                $phpcsFile->addError($error, $stackPtr, 'Unexpected', $data);
                continue;
            }

            if (isset($properties[$key]) === false
                && isset($this->properties[$key]) === true
            ) {
                $error = 'Missing Subversion property "%s" = "%s"';
                $data  = array(
                          $key,
                          $this->properties[$key],
                         );
                $phpcsFile->addError($error, $stackPtr, 'Missing', $data);
                continue;
            }

            if ($properties[$key] !== null
                && $properties[$key] !== $this->properties[$key]
            ) {
                $error = 'Subversion property "%s" = "%s" does not match "%s"';
                $data  = array(
                          $key,
                          $properties[$key],
                          $this->properties[$key],
                         );
                $phpcsFile->addError($error, $stackPtr, 'NoMatch', $data);
            }
        }//end foreach

    }//end process()


    /**
     * Returns the Subversion properties which are actually set on a path.
     *
     * Returns NULL if the file is not under version control.
     *
     * @param string $path The path to return Subversion properties on.
     *
     * @return array
     * @throws PHP_CodeSniffer_Exception If Subversion properties file could
     *                                   not be opened.
     */
    protected function getProperties($path)
    {
        $properties = array();

        $paths   = array();
        $paths[] = dirname($path).'/.svn/props/'.basename($path).'.svn-work';
        $paths[] = dirname($path).'/.svn/prop-base/'.basename($path).'.svn-base';

        $foundPath = false;
        foreach ($paths as $path) {
            if (file_exists($path) === true) {
                $foundPath = true;

                $handle = fopen($path, 'r');
                if ($handle === false) {
                    $error = 'Error opening file; could not get Subversion properties';
                    throw new PHP_CodeSniffer_Exception($error);
                }

                while (feof($handle) === false) {
                    // Read a key length line. Might be END, though.
                    $buffer = trim(fgets($handle));

                    // Check for the end of the hash.
                    if ($buffer === 'END') {
                        break;
                    }

                    // Now read that much into a buffer.
                    $key = fread($handle, substr($buffer, 2));

                    // Suck up extra newline after key data.
                    fgetc($handle);

                    // Read a value length line.
                    $buffer = trim(fgets($handle));

                    // Now read that much into a buffer.
                    $length = substr($buffer, 2);
                    if ($length === '0') {
                        // Length of value is ZERO characters, so
                        // value is actually empty.
                        $value = '';
                    } else {
                        $value = fread($handle, $length);
                    }

                    // Suck up extra newline after value data.
                    fgetc($handle);

                    $properties[$key] = $value;
                }//end while

                fclose($handle);
            }//end if
        }//end foreach

        if ($foundPath === false) {
            return null;
        }

        return $properties;

    }//end getProperties()


}//end class
