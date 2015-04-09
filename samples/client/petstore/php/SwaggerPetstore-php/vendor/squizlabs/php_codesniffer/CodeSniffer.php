<?php
/**
 * PHP_CodeSniffer tokenizes PHP code and detects violations of a
 * defined set of coding standards.
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

spl_autoload_register(array('PHP_CodeSniffer', 'autoload'));

if (class_exists('PHP_CodeSniffer_Exception', true) === false) {
    throw new Exception('Class PHP_CodeSniffer_Exception not found');
}

if (class_exists('PHP_CodeSniffer_File', true) === false) {
    throw new PHP_CodeSniffer_Exception('Class PHP_CodeSniffer_File not found');
}

if (class_exists('PHP_CodeSniffer_Fixer', true) === false) {
    throw new PHP_CodeSniffer_Exception('Class PHP_CodeSniffer_Fixer not found');
}

if (class_exists('PHP_CodeSniffer_Tokens', true) === false) {
    throw new PHP_CodeSniffer_Exception('Class PHP_CodeSniffer_Tokens not found');
}

if (class_exists('PHP_CodeSniffer_CLI', true) === false) {
    throw new PHP_CodeSniffer_Exception('Class PHP_CodeSniffer_CLI not found');
}

if (interface_exists('PHP_CodeSniffer_Sniff', true) === false) {
    throw new PHP_CodeSniffer_Exception('Interface PHP_CodeSniffer_Sniff not found');
}

/**
 * PHP_CodeSniffer tokenizes PHP code and detects violations of a
 * defined set of coding standards.
 *
 * Standards are specified by classes that implement the PHP_CodeSniffer_Sniff
 * interface. A sniff registers what token types it wishes to listen for, then
 * PHP_CodeSniffer encounters that token, the sniff is invoked and passed
 * information about where the token was found in the stack, and the token stack
 * itself.
 *
 * Sniff files and their containing class must be prefixed with Sniff, and
 * have an extension of .php.
 *
 * Multiple PHP_CodeSniffer operations can be performed by re-calling the
 * process function with different parameters.
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
class PHP_CodeSniffer
{

    /**
     * The current version.
     *
     * @var string
     */
    const VERSION = '2.3.0';

    /**
     * Package stability; either stable, beta or alpha.
     *
     * @var string
     */
    const STABILITY = 'stable';

    /**
     * The file or directory that is currently being processed.
     *
     * @var string
     */
    protected $file = '';

    /**
     * The directories that the processed rulesets are in.
     *
     * This is declared static because it is also used in the
     * autoloader to look for sniffs outside the PHPCS install.
     * This way, standards designed to be installed inside PHPCS can
     * also be used from outside the PHPCS Standards directory.
     *
     * @var string
     */
    protected static $rulesetDirs = array();

    /**
     * The CLI object controlling the run.
     *
     * @var PHP_CodeSniffer_CLI
     */
    public $cli = null;

    /**
     * The Reporting object controlling report generation.
     *
     * @var PHP_CodeSniffer_Reporting
     */
    public $reporting = null;

    /**
     * An array of sniff objects that are being used to check files.
     *
     * @var array(PHP_CodeSniffer_Sniff)
     */
    protected $listeners = array();

    /**
     * An array of sniffs that are being used to check files.
     *
     * @var array(string)
     */
    protected $sniffs = array();

    /**
     * The listeners array, indexed by token type.
     *
     * @var array
     */
    private $_tokenListeners = array();

    /**
     * An array of rules from the ruleset.xml file.
     *
     * It may be empty, indicating that the ruleset does not override
     * any of the default sniff settings.
     *
     * @var array
     */
    protected $ruleset = array();

    /**
     * An array of patterns to use for skipping files.
     *
     * @var array
     */
    protected $ignorePatterns = array();

    /**
     * An array of extensions for files we will check.
     *
     * @var array
     */
    public $allowedFileExtensions = array();

    /**
     * An array of default extensions and associated tokenizers.
     *
     * If no extensions are set, these will be used as the defaults.
     * If extensions are set, these will be used when the correct tokenizer
     * can not be determined, such as when checking a passed filename instead
     * of files in a directory.
     *
     * @var array
     */
    public $defaultFileExtensions = array(
                                     'php' => 'PHP',
                                     'inc' => 'PHP',
                                     'js'  => 'JS',
                                     'css' => 'CSS',
                                    );

    /**
     * An array of variable types for param/var we will check.
     *
     * @var array(string)
     */
    public static $allowedTypes = array(
                                   'array',
                                   'boolean',
                                   'float',
                                   'integer',
                                   'mixed',
                                   'object',
                                   'string',
                                   'resource',
                                   'callable',
                                  );


    /**
     * Constructs a PHP_CodeSniffer object.
     *
     * @param int    $verbosity   The verbosity level.
     *                            1: Print progress information.
     *                            2: Print tokenizer debug information.
     *                            3: Print sniff debug information.
     * @param int    $tabWidth    The number of spaces each tab represents.
     *                            If greater than zero, tabs will be replaced
     *                            by spaces before testing each file.
     * @param string $encoding    The charset of the sniffed files.
     *                            This is important for some reports that output
     *                            with utf-8 encoding as you don't want it double
     *                            encoding messages.
     * @param bool   $interactive If TRUE, will stop after each file with errors
     *                            and wait for user input.
     *
     * @see process()
     */
    public function __construct(
        $verbosity=0,
        $tabWidth=0,
        $encoding='iso-8859-1',
        $interactive=false
    ) {
        if ($verbosity !== null) {
            $this->setVerbosity($verbosity);
        }

        if ($tabWidth !== null) {
            $this->setTabWidth($tabWidth);
        }

        if ($encoding !== null) {
            $this->setEncoding($encoding);
        }

        if ($interactive !== null) {
            $this->setInteractive($interactive);
        }

        if (defined('PHPCS_DEFAULT_ERROR_SEV') === false) {
            define('PHPCS_DEFAULT_ERROR_SEV', 5);
        }

        if (defined('PHPCS_DEFAULT_WARN_SEV') === false) {
            define('PHPCS_DEFAULT_WARN_SEV', 5);
        }

        if (defined('PHP_CODESNIFFER_CBF') === false) {
            define('PHP_CODESNIFFER_CBF', false);
        }

        // Set default CLI object in case someone is running us
        // without using the command line script.
        $this->cli = new PHP_CodeSniffer_CLI();
        $this->cli->errorSeverity   = PHPCS_DEFAULT_ERROR_SEV;
        $this->cli->warningSeverity = PHPCS_DEFAULT_WARN_SEV;
        $this->cli->dieOnUnknownArg = false;

        $this->reporting = new PHP_CodeSniffer_Reporting();

    }//end __construct()


    /**
     * Autoload static method for loading classes and interfaces.
     *
     * @param string $className The name of the class or interface.
     *
     * @return void
     */
    public static function autoload($className)
    {
        if (substr($className, 0, 4) === 'PHP_') {
            $newClassName = substr($className, 4);
        } else {
            $newClassName = $className;
        }

        $path = str_replace(array('_', '\\'), DIRECTORY_SEPARATOR, $newClassName).'.php';

        if (is_file(dirname(__FILE__).DIRECTORY_SEPARATOR.$path) === true) {
            // Check standard file locations based on class name.
            include dirname(__FILE__).DIRECTORY_SEPARATOR.$path;
            return;
        } else {
            // Check for included sniffs.
            $installedPaths = PHP_CodeSniffer::getInstalledStandardPaths();
            foreach ($installedPaths as $installedPath) {
                if (is_file($installedPath.DIRECTORY_SEPARATOR.$path) === true) {
                    include $installedPath.DIRECTORY_SEPARATOR.$path;
                    return;
                }
            }

            // Check standard file locations based on the loaded rulesets.
            foreach (self::$rulesetDirs as $rulesetDir) {
                if (is_file(dirname($rulesetDir).DIRECTORY_SEPARATOR.$path) === true) {
                    include_once dirname($rulesetDir).DIRECTORY_SEPARATOR.$path;
                    return;
                }
            }
        }//end if

        // Everything else.
        @include $path;

    }//end autoload()


    /**
     * Sets the verbosity level.
     *
     * @param int $verbosity The verbosity level.
     *                       1: Print progress information.
     *                       2: Print tokenizer debug information.
     *                       3: Print sniff debug information.
     *
     * @return void
     */
    public function setVerbosity($verbosity)
    {
        if (defined('PHP_CODESNIFFER_VERBOSITY') === false) {
            define('PHP_CODESNIFFER_VERBOSITY', $verbosity);
        }

    }//end setVerbosity()


    /**
     * Sets the tab width.
     *
     * @param int $tabWidth The number of spaces each tab represents.
     *                      If greater than zero, tabs will be replaced
     *                      by spaces before testing each file.
     *
     * @return void
     */
    public function setTabWidth($tabWidth)
    {
        if (defined('PHP_CODESNIFFER_TAB_WIDTH') === false) {
            define('PHP_CODESNIFFER_TAB_WIDTH', $tabWidth);
        }

    }//end setTabWidth()


    /**
     * Sets the encoding.
     *
     * @param string $encoding The charset of the sniffed files.
     *                         This is important for some reports that output
     *                         with utf-8 encoding as you don't want it double
     *                         encoding messages.
     *
     * @return void
     */
    public function setEncoding($encoding)
    {
        if (defined('PHP_CODESNIFFER_ENCODING') === false) {
            define('PHP_CODESNIFFER_ENCODING', $encoding);
        }

    }//end setEncoding()


    /**
     * Sets the interactive flag.
     *
     * @param bool $interactive If TRUE, will stop after each file with errors
     *                          and wait for user input.
     *
     * @return void
     */
    public function setInteractive($interactive)
    {
        if (defined('PHP_CODESNIFFER_INTERACTIVE') === false) {
            define('PHP_CODESNIFFER_INTERACTIVE', $interactive);
        }

    }//end setInteractive()


    /**
     * Sets an array of file extensions that we will allow checking of.
     *
     * If the extension is one of the defaults, a specific tokenizer
     * will be used. Otherwise, the PHP tokenizer will be used for
     * all extensions passed.
     *
     * @param array $extensions An array of file extensions.
     *
     * @return void
     */
    public function setAllowedFileExtensions(array $extensions)
    {
        $newExtensions = array();
        foreach ($extensions as $ext) {
            $slash = strpos($ext, '/');
            if ($slash !== false) {
                // They specified the tokenizer too.
                list($ext, $tokenizer) = explode('/', $ext);
                $newExtensions[$ext]   = strtoupper($tokenizer);
                continue;
            }

            if (isset($this->allowedFileExtensions[$ext]) === true) {
                $newExtensions[$ext] = $this->allowedFileExtensions[$ext];
            } else if (isset($this->defaultFileExtensions[$ext]) === true) {
                $newExtensions[$ext] = $this->defaultFileExtensions[$ext];
            } else {
                $newExtensions[$ext] = 'PHP';
            }
        }

        $this->allowedFileExtensions = $newExtensions;

    }//end setAllowedFileExtensions()


    /**
     * Sets an array of ignore patterns that we use to skip files and folders.
     *
     * Patterns are not case sensitive.
     *
     * @param array $patterns An array of ignore patterns. The pattern is the key
     *                        and the value is either "absolute" or "relative",
     *                        depending on how the pattern should be applied to a
     *                        file path.
     *
     * @return void
     */
    public function setIgnorePatterns(array $patterns)
    {
        $this->ignorePatterns = $patterns;

    }//end setIgnorePatterns()


    /**
     * Gets the array of ignore patterns.
     *
     * Optionally takes a listener to get ignore patterns specified
     * for that sniff only.
     *
     * @param string $listener The listener to get patterns for. If NULL, all
     *                         patterns are returned.
     *
     * @return array
     */
    public function getIgnorePatterns($listener=null)
    {
        if ($listener === null) {
            return $this->ignorePatterns;
        }

        if (isset($this->ignorePatterns[$listener]) === true) {
            return $this->ignorePatterns[$listener];
        }

        return array();

    }//end getIgnorePatterns()


    /**
     * Sets the internal CLI object.
     *
     * @param object $cli The CLI object controlling the run.
     *
     * @return void
     */
    public function setCli($cli)
    {
        $this->cli = $cli;

    }//end setCli()


    /**
     * Start a PHP_CodeSniffer run.
     *
     * @param string|array $files        The files and directories to process. For
     *                                   directories, each sub directory will also
     *                                   be traversed for source files.
     * @param string|array $standards    The set of code sniffs we are testing
     *                                   against.
     * @param array        $restrictions The sniff codes to restrict the
     *                                   violations to.
     * @param boolean      $local        If true, don't recurse into directories.
     *
     * @return void
     */
    public function process($files, $standards, array $restrictions=array(), $local=false)
    {
        $files = (array) $files;
        $this->initStandard($standards, $restrictions);
        $this->processFiles($files, $local);

    }//end process()


    /**
     * Initialise the standard that the run will use.
     *
     * @param string|array $standards    The set of code sniffs we are testing
     *                                   against.
     * @param array        $restrictions The sniff codes to restrict the
     *
     * @return void
     */
    public function initStandard($standards, array $restrictions=array())
    {
        $standards = (array) $standards;

        // Reset the members.
        $this->listeners       = array();
        $this->sniffs          = array();
        $this->ruleset         = array();
        $this->_tokenListeners = array();
        self::$rulesetDirs     = array();

        // Ensure this option is enabled or else line endings will not always
        // be detected properly for files created on a Mac with the /r line ending.
        ini_set('auto_detect_line_endings', true);

        $sniffs = array();
        foreach ($standards as $standard) {
            $installed = $this->getInstalledStandardPath($standard);
            if ($installed !== null) {
                $standard = $installed;
            } else {
                $standard = self::realpath($standard);
                if (is_dir($standard) === true
                    && is_file(self::realpath($standard.DIRECTORY_SEPARATOR.'ruleset.xml')) === true
                ) {
                    $standard = self::realpath($standard.DIRECTORY_SEPARATOR.'ruleset.xml');
                }
            }

            if (PHP_CODESNIFFER_VERBOSITY === 1) {
                $ruleset = simplexml_load_string(file_get_contents($standard));
                if ($ruleset !== false) {
                    $standardName = (string) $ruleset['name'];
                }

                echo "Registering sniffs in the $standardName standard... ";
                if (count($standards) > 1 || PHP_CODESNIFFER_VERBOSITY > 2) {
                    echo PHP_EOL;
                }
            }

            $sniffs = array_merge($sniffs, $this->processRuleset($standard));
        }//end foreach

        $sniffRestrictions = array();
        foreach ($restrictions as $sniffCode) {
            $parts = explode('.', strtolower($sniffCode));
            $sniffRestrictions[] = $parts[0].'_sniffs_'.$parts[1].'_'.$parts[2].'sniff';
        }

        $this->registerSniffs($sniffs, $sniffRestrictions);
        $this->populateTokenListeners();

        if (PHP_CODESNIFFER_VERBOSITY === 1) {
            $numSniffs = count($this->sniffs);
            echo "DONE ($numSniffs sniffs registered)".PHP_EOL;
        }

    }//end initStandard()


    /**
     * Processes the files/directories that PHP_CodeSniffer was constructed with.
     *
     * @param string|array $files The files and directories to process. For
     *                            directories, each sub directory will also
     *                            be traversed for source files.
     * @param boolean      $local If true, don't recurse into directories.
     *
     * @return void
     * @throws PHP_CodeSniffer_Exception If files are invalid.
     */
    public function processFiles($files, $local=false)
    {
        $files        = (array) $files;
        $cliValues    = $this->cli->getCommandLineValues();
        $showProgress = $cliValues['showProgress'];
        $useColors    = $cliValues['colors'];

        if (PHP_CODESNIFFER_VERBOSITY > 0) {
            echo 'Creating file list... ';
        }

        if (empty($this->allowedFileExtensions) === true) {
            $this->allowedFileExtensions = $this->defaultFileExtensions;
        }

        $todo     = $this->getFilesToProcess($files, $local);
        $numFiles = count($todo);

        if (PHP_CODESNIFFER_VERBOSITY > 0) {
            echo "DONE ($numFiles files in queue)".PHP_EOL;
        }

        $numProcessed = 0;
        $dots         = 0;
        $maxLength    = strlen($numFiles);
        $lastDir      = '';
        foreach ($todo as $file) {
            $this->file = $file;
            $currDir    = dirname($file);
            if ($lastDir !== $currDir) {
                if (PHP_CODESNIFFER_VERBOSITY > 0 || PHP_CODESNIFFER_CBF === true) {
                    echo 'Changing into directory '.$currDir.PHP_EOL;
                }

                $lastDir = $currDir;
            }

            $phpcsFile = $this->processFile($file, null);
            $numProcessed++;

            if (PHP_CODESNIFFER_VERBOSITY > 0
                || PHP_CODESNIFFER_INTERACTIVE === true
                || $showProgress === false
            ) {
                continue;
            }

            // Show progress information.
            if ($phpcsFile === null) {
                echo 'S';
            } else {
                $errors   = $phpcsFile->getErrorCount();
                $warnings = $phpcsFile->getWarningCount();
                if ($errors > 0) {
                    if ($useColors === true) {
                        echo "\033[31m";
                    }

                    echo 'E';
                } else if ($warnings > 0) {
                    if ($useColors === true) {
                        echo "\033[33m";
                    }

                    echo 'W';
                } else {
                    echo '.';
                }

                if ($useColors === true) {
                    echo "\033[0m";
                }
            }//end if

            $dots++;
            if ($dots === 60) {
                $padding = ($maxLength - strlen($numProcessed));
                echo str_repeat(' ', $padding);
                $percent = round(($numProcessed / $numFiles) * 100);
                echo " $numProcessed / $numFiles ($percent%)".PHP_EOL;
                $dots = 0;
            }
        }//end foreach

        if (PHP_CODESNIFFER_VERBOSITY === 0
            && PHP_CODESNIFFER_INTERACTIVE === false
            && $showProgress === true
        ) {
            echo PHP_EOL.PHP_EOL;
        }

    }//end processFiles()


    /**
     * Processes a single ruleset and returns a list of the sniffs it represents.
     *
     * Rules founds within the ruleset are processed immediately, but sniff classes
     * are not registered by this method.
     *
     * @param string $rulesetPath The path to a ruleset XML file.
     * @param int    $depth       How many nested processing steps we are in. This
     *                            is only used for debug output.
     *
     * @return array
     * @throws PHP_CodeSniffer_Exception If the ruleset path is invalid.
     */
    public function processRuleset($rulesetPath, $depth=0)
    {
        $rulesetPath = self::realpath($rulesetPath);
        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo str_repeat("\t", $depth);
            echo "Processing ruleset $rulesetPath".PHP_EOL;
        }

        $ruleset = simplexml_load_string(file_get_contents($rulesetPath));
        if ($ruleset === false) {
            throw new PHP_CodeSniffer_Exception("Ruleset $rulesetPath is not valid");
        }

        $ownSniffs      = array();
        $includedSniffs = array();
        $excludedSniffs = array();
        $cliValues      = $this->cli->getCommandLineValues();

        $rulesetDir          = dirname($rulesetPath);
        self::$rulesetDirs[] = $rulesetDir;

        if (is_dir($rulesetDir.DIRECTORY_SEPARATOR.'Sniffs') === true) {
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\tAdding sniff files from \"/.../".basename($rulesetDir)."/Sniffs/\" directory".PHP_EOL;
            }

            $ownSniffs = $this->_expandSniffDirectory($rulesetDir.DIRECTORY_SEPARATOR.'Sniffs', $depth);
        }

        foreach ($ruleset->rule as $rule) {
            if (isset($rule['ref']) === false
                || $this->_shouldProcessElement($rule) === false
            ) {
                continue;
            }

            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\tProcessing rule \"".$rule['ref'].'"'.PHP_EOL;
            }

            $includedSniffs = array_merge(
                $includedSniffs,
                $this->_expandRulesetReference($rule['ref'], $rulesetDir, $depth)
            );

            if (isset($rule->exclude) === true) {
                foreach ($rule->exclude as $exclude) {
                    if ($this->_shouldProcessElement($exclude) === false) {
                        continue;
                    }

                    if (PHP_CODESNIFFER_VERBOSITY > 1) {
                        echo str_repeat("\t", $depth);
                        echo "\t\tExcluding rule \"".$exclude['name'].'"'.PHP_EOL;
                    }

                    // Check if a single code is being excluded, which is a shortcut
                    // for setting the severity of the message to 0.
                    $parts = explode('.', $exclude['name']);
                    if (count($parts) === 4) {
                        $this->ruleset[(string) $exclude['name']]['severity'] = 0;
                        if (PHP_CODESNIFFER_VERBOSITY > 1) {
                            echo str_repeat("\t", $depth);
                            echo "\t\t=> severity set to 0".PHP_EOL;
                        }
                    } else {
                        $excludedSniffs = array_merge(
                            $excludedSniffs,
                            $this->_expandRulesetReference($exclude['name'], $rulesetDir, ($depth + 1))
                        );
                    }
                }//end foreach
            }//end if

            $this->_processRule($rule, $depth);
        }//end foreach

        // Process custom command line arguments.
        $cliArgs = array();
        foreach ($ruleset->{'arg'} as $arg) {
            if ($this->_shouldProcessElement($arg) === false) {
                continue;
            }

            if (isset($arg['name']) === true) {
                $argString = '--'.(string) $arg['name'];
                if (isset($arg['value']) === true) {
                    $argString .= '='.(string) $arg['value'];
                }
            } else {
                $argString = '-'.(string) $arg['value'];
            }

            $cliArgs[] = $argString;

            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\t=> set command line value $argString".PHP_EOL;
            }
        }//end foreach

        if (empty($cliValues['files']) === true) {
            // Process hard-coded file paths.
            foreach ($ruleset->{'file'} as $file) {
                $file      = (string) $file;
                $cliArgs[] = $file;
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo str_repeat("\t", $depth);
                    echo "\t=> added \"$file\" to the file list".PHP_EOL;
                }
            }
        }

        if (empty($cliArgs) === false) {
            $this->cli->setCommandLineValues($cliArgs);
        }

        // Process custom sniff config settings.
        foreach ($ruleset->{'config'} as $config) {
            if ($this->_shouldProcessElement($config) === false) {
                continue;
            }

            $this->setConfigData((string) $config['name'], (string) $config['value'], true);
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\t=> set config value ".(string) $config['name'].': '.(string) $config['value'].PHP_EOL;
            }
        }

        // Process custom ignore pattern rules.
        foreach ($ruleset->{'exclude-pattern'} as $pattern) {
            if ($this->_shouldProcessElement($pattern) === false) {
                continue;
            }

            if (isset($pattern['type']) === false) {
                $pattern['type'] = 'absolute';
            }

            $this->ignorePatterns[(string) $pattern] = (string) $pattern['type'];
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\t=> added global ".(string) $pattern['type'].' ignore pattern: '.(string) $pattern.PHP_EOL;
            }
        }

        $includedSniffs = array_unique(array_merge($ownSniffs, $includedSniffs));
        $excludedSniffs = array_unique($excludedSniffs);

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            $included = count($includedSniffs);
            $excluded = count($excludedSniffs);
            echo str_repeat("\t", $depth);
            echo "=> Ruleset processing complete; included $included sniffs and excluded $excluded".PHP_EOL;
        }

        // Merge our own sniff list with our externally included
        // sniff list, but filter out any excluded sniffs.
        $files = array();
        foreach ($includedSniffs as $sniff) {
            if (in_array($sniff, $excludedSniffs) === true) {
                continue;
            } else {
                $files[] = self::realpath($sniff);
            }
        }

        return $files;

    }//end processRuleset()


    /**
     * Expands a directory into a list of sniff files within.
     *
     * @param string $directory The path to a directory.
     * @param int    $depth     How many nested processing steps we are in. This
     *                          is only used for debug output.
     *
     * @return array
     */
    private function _expandSniffDirectory($directory, $depth=0)
    {
        $sniffs = array();

        if (defined('RecursiveDirectoryIterator::FOLLOW_SYMLINKS') === true) {
            $rdi = new RecursiveDirectoryIterator($directory, RecursiveDirectoryIterator::FOLLOW_SYMLINKS);
        } else {
            $rdi = new RecursiveDirectoryIterator($directory);
        }

        $di = new RecursiveIteratorIterator($rdi, 0, RecursiveIteratorIterator::CATCH_GET_CHILD);

        $dirLen = strlen($directory);

        foreach ($di as $file) {
            $filename = $file->getFilename();

            // Skip hidden files.
            if (substr($filename, 0, 1) === '.') {
                continue;
            }

            // We are only interested in PHP and sniff files.
            $fileParts = explode('.', $filename);
            if (array_pop($fileParts) !== 'php') {
                continue;
            }

            $basename = basename($filename, '.php');
            if (substr($basename, -5) !== 'Sniff') {
                continue;
            }

            $path = $file->getPathname();

            // Skip files in hidden directories within the Sniffs directory of this
            // standard. We use the offset with strpos() to allow hidden directories
            // before, valid example:
            // /home/foo/.composer/vendor/drupal/coder/coder_sniffer/Drupal/Sniffs/...
            if (strpos($path, DIRECTORY_SEPARATOR.'.', $dirLen) !== false) {
                continue;
            }

            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\t\t=> $path".PHP_EOL;
            }

            $sniffs[] = $path;
        }//end foreach

        return $sniffs;

    }//end _expandSniffDirectory()


    /**
     * Expands a ruleset reference into a list of sniff files.
     *
     * @param string $ref        The reference from the ruleset XML file.
     * @param string $rulesetDir The directory of the ruleset XML file, used to
     *                           evaluate relative paths.
     * @param int    $depth      How many nested processing steps we are in. This
     *                           is only used for debug output.
     *
     * @return array
     * @throws PHP_CodeSniffer_Exception If the reference is invalid.
     */
    private function _expandRulesetReference($ref, $rulesetDir, $depth=0)
    {
        // Ignore internal sniffs codes as they are used to only
        // hide and change internal messages.
        if (substr($ref, 0, 9) === 'Internal.') {
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\t\t* ignoring internal sniff code *".PHP_EOL;
            }

            return array();
        }

        // As sniffs can't begin with a full stop, assume references in
        // this format are relative paths and attempt to convert them
        // to absolute paths. If this fails, let the reference run through
        // the normal checks and have it fail as normal.
        if (substr($ref, 0, 1) === '.') {
            $realpath = self::realpath($rulesetDir.'/'.$ref);
            if ($realpath !== false) {
                $ref = $realpath;
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo str_repeat("\t", $depth);
                    echo "\t\t=> $ref".PHP_EOL;
                }
            }
        }

        // As sniffs can't begin with a tilde, assume references in
        // this format at relative to the user's home directory.
        if (substr($ref, 0, 2) === '~/') {
            $realpath = self::realpath($ref);
            if ($realpath !== false) {
                $ref = $realpath;
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo str_repeat("\t", $depth);
                    echo "\t\t=> $ref".PHP_EOL;
                }
            }
        }

        if (is_file($ref) === true) {
            if (substr($ref, -9) === 'Sniff.php') {
                // A single external sniff.
                self::$rulesetDirs[] = dirname(dirname(dirname($ref)));
                return array($ref);
            }
        } else {
            // See if this is a whole standard being referenced.
            $path = $this->getInstalledStandardPath($ref);
            if (self::isPharFile($path) === true && strpos($path, 'ruleset.xml') === false) {
                // If the ruleset exists inside the phar file, use it.
                if (file_exists($path.DIRECTORY_SEPARATOR.'ruleset.xml') === true) {
                    $path = $path.DIRECTORY_SEPARATOR.'ruleset.xml';
                } else {
                    $path = null;
                }
            }

            if ($path !== null) {
                $ref = $path;
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo str_repeat("\t", $depth);
                    echo "\t\t=> $ref".PHP_EOL;
                }
            } else if (is_dir($ref) === false) {
                // Work out the sniff path.
                $sepPos = strpos($ref, DIRECTORY_SEPARATOR);
                if ($sepPos !== false) {
                    $stdName = substr($ref, 0, $sepPos);
                    $path    = substr($ref, $sepPos);
                } else {
                    $parts   = explode('.', $ref);
                    $stdName = $parts[0];
                    if (count($parts) === 1) {
                        // A whole standard?
                        $path = '';
                    } else if (count($parts) === 2) {
                        // A directory of sniffs?
                        $path = DIRECTORY_SEPARATOR.'Sniffs'.DIRECTORY_SEPARATOR.$parts[1];
                    } else {
                        // A single sniff?
                        $path = DIRECTORY_SEPARATOR.'Sniffs'.DIRECTORY_SEPARATOR.$parts[1].DIRECTORY_SEPARATOR.$parts[2].'Sniff.php';
                    }
                }

                $newRef  = false;
                $stdPath = $this->getInstalledStandardPath($stdName);
                if ($stdPath !== null && $path !== '') {
                    if (self::isPharFile($stdPath) === true
                        && strpos($stdPath, 'ruleset.xml') === false
                    ) {
                        // Phar files can only return the directory,
                        // since ruleset can be omitted if building one standard.
                        $newRef = self::realpath($stdPath.$path);
                    } else {
                        $newRef = self::realpath(dirname($stdPath).$path);
                    }
                }

                if ($newRef === false) {
                    // The sniff is not locally installed, so check if it is being
                    // referenced as a remote sniff outside the install. We do this
                    // by looking through all directories where we have found ruleset
                    // files before, looking for ones for this particular standard,
                    // and seeing if it is in there.
                    foreach (self::$rulesetDirs as $dir) {
                        if (strtolower(basename($dir)) !== strtolower($stdName)) {
                            continue;
                        }

                        $newRef = self::realpath($dir.$path);

                        if ($newRef !== false) {
                            $ref = $newRef;
                        }
                    }
                } else {
                    $ref = $newRef;
                }

                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo str_repeat("\t", $depth);
                    echo "\t\t=> $ref".PHP_EOL;
                }
            }//end if
        }//end if

        if (is_dir($ref) === true) {
            if (is_file($ref.DIRECTORY_SEPARATOR.'ruleset.xml') === true) {
                // We are referencing an external coding standard.
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo str_repeat("\t", $depth);
                    echo "\t\t* rule is referencing a standard using directory name; processing *".PHP_EOL;
                }

                return $this->processRuleset($ref.DIRECTORY_SEPARATOR.'ruleset.xml', ($depth + 2));
            } else {
                // We are referencing a whole directory of sniffs.
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo str_repeat("\t", $depth);
                    echo "\t\t* rule is referencing a directory of sniffs *".PHP_EOL;
                    echo str_repeat("\t", $depth);
                    echo "\t\tAdding sniff files from directory".PHP_EOL;
                }

                return $this->_expandSniffDirectory($ref, ($depth + 1));
            }
        } else {
            if (is_file($ref) === false) {
                $error = "Referenced sniff \"$ref\" does not exist";
                throw new PHP_CodeSniffer_Exception($error);
            }

            if (substr($ref, -9) === 'Sniff.php') {
                // A single sniff.
                return array($ref);
            } else {
                // Assume an external ruleset.xml file.
                if (PHP_CODESNIFFER_VERBOSITY > 1) {
                    echo str_repeat("\t", $depth);
                    echo "\t\t* rule is referencing a standard using ruleset path; processing *".PHP_EOL;
                }

                return $this->processRuleset($ref, ($depth + 2));
            }
        }//end if

    }//end _expandRulesetReference()


    /**
     * Processes a rule from a ruleset XML file, overriding built-in defaults.
     *
     * @param SimpleXMLElement $rule  The rule object from a ruleset XML file.
     * @param int              $depth How many nested processing steps we are in.
     *                                This is only used for debug output.
     *
     * @return void
     */
    private function _processRule($rule, $depth=0)
    {
        $code = (string) $rule['ref'];

        // Custom severity.
        if (isset($rule->severity) === true
            && $this->_shouldProcessElement($rule->severity) === true
        ) {
            if (isset($this->ruleset[$code]) === false) {
                $this->ruleset[$code] = array();
            }

            $this->ruleset[$code]['severity'] = (int) $rule->severity;
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\t\t=> severity set to ".(int) $rule->severity.PHP_EOL;
            }
        }

        // Custom message type.
        if (isset($rule->type) === true
            && $this->_shouldProcessElement($rule->type) === true
        ) {
            if (isset($this->ruleset[$code]) === false) {
                $this->ruleset[$code] = array();
            }

            $this->ruleset[$code]['type'] = (string) $rule->type;
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\t\t=> message type set to ".(string) $rule->type.PHP_EOL;
            }
        }

        // Custom message.
        if (isset($rule->message) === true
            && $this->_shouldProcessElement($rule->message) === true
        ) {
            if (isset($this->ruleset[$code]) === false) {
                $this->ruleset[$code] = array();
            }

            $this->ruleset[$code]['message'] = (string) $rule->message;
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\t\t=> message set to ".(string) $rule->message.PHP_EOL;
            }
        }

        // Custom properties.
        if (isset($rule->properties) === true
            && $this->_shouldProcessElement($rule->properties) === true
        ) {
            foreach ($rule->properties->property as $prop) {
                if ($this->_shouldProcessElement($prop) === false) {
                    continue;
                }

                if (isset($this->ruleset[$code]) === false) {
                    $this->ruleset[$code] = array(
                                             'properties' => array(),
                                            );
                } else if (isset($this->ruleset[$code]['properties']) === false) {
                    $this->ruleset[$code]['properties'] = array();
                }

                $name = (string) $prop['name'];
                if (isset($prop['type']) === true
                    && (string) $prop['type'] === 'array'
                ) {
                    $value  = (string) $prop['value'];
                    $values = array();
                    foreach (explode(',', $value) as $val) {
                        $v = '';

                        list($k,$v) = explode('=>', $val.'=>');
                        if ($v !== '') {
                            $values[$k] = $v;
                        } else {
                            $values[] = $k;
                        }
                    }

                    $this->ruleset[$code]['properties'][$name] = $values;
                    if (PHP_CODESNIFFER_VERBOSITY > 1) {
                        echo str_repeat("\t", $depth);
                        echo "\t\t=> array property \"$name\" set to \"$value\"".PHP_EOL;
                    }
                } else {
                    $this->ruleset[$code]['properties'][$name] = (string) $prop['value'];
                    if (PHP_CODESNIFFER_VERBOSITY > 1) {
                        echo str_repeat("\t", $depth);
                        echo "\t\t=> property \"$name\" set to \"".(string) $prop['value'].'"'.PHP_EOL;
                    }
                }//end if
            }//end foreach
        }//end if

        // Ignore patterns.
        foreach ($rule->{'exclude-pattern'} as $pattern) {
            if ($this->_shouldProcessElement($pattern) === false) {
                continue;
            }

            if (isset($this->ignorePatterns[$code]) === false) {
                $this->ignorePatterns[$code] = array();
            }

            if (isset($pattern['type']) === false) {
                $pattern['type'] = 'absolute';
            }

            $this->ignorePatterns[$code][(string) $pattern] = (string) $pattern['type'];
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo str_repeat("\t", $depth);
                echo "\t\t=> added sniff-specific ".(string) $pattern['type'].' ignore pattern: '.(string) $pattern.PHP_EOL;
            }
        }

    }//end _processRule()


    /**
     * Determine if an element should be processed or ignored.
     *
     * @param SimpleXMLElement $element An object from a ruleset XML file.
     * @param int              $depth   How many nested processing steps we are in.
     *                                  This is only used for debug output.
     *
     * @return bool
     */
    private function _shouldProcessElement($element, $depth=0)
    {
        if (isset($element['phpcbf-only']) === false
            && isset($element['phpcs-only']) === false
        ) {
            // No exceptions are being made.
            return true;
        }

        if (PHP_CODESNIFFER_CBF === true
            && isset($element['phpcbf-only']) === true
            && (string) $element['phpcbf-only'] === 'true'
        ) {
            return true;
        }

        if (PHP_CODESNIFFER_CBF === false
            && isset($element['phpcs-only']) === true
            && (string) $element['phpcs-only'] === 'true'
        ) {
            return true;
        }

        return false;

    }//end _shouldProcessElement()


    /**
     * Loads and stores sniffs objects used for sniffing files.
     *
     * @param array $files        Paths to the sniff files to register.
     * @param array $restrictions The sniff class names to restrict the allowed
     *                            listeners to.
     *
     * @return void
     */
    public function registerSniffs($files, $restrictions)
    {
        $listeners = array();

        foreach ($files as $file) {
            // Work out where the position of /StandardName/Sniffs/... is
            // so we can determine what the class will be called.
            $sniffPos = strrpos($file, DIRECTORY_SEPARATOR.'Sniffs'.DIRECTORY_SEPARATOR);
            if ($sniffPos === false) {
                continue;
            }

            $slashPos = strrpos(substr($file, 0, $sniffPos), DIRECTORY_SEPARATOR);
            if ($slashPos === false) {
                continue;
            }

            $className = substr($file, ($slashPos + 1));
            $className = substr($className, 0, -4);
            $className = str_replace(DIRECTORY_SEPARATOR, '_', $className);

            // If they have specified a list of sniffs to restrict to, check
            // to see if this sniff is allowed.
            if (empty($restrictions) === false
                && in_array(strtolower($className), $restrictions) === false
            ) {
                continue;
            }

            include_once $file;

            // Support the use of PHP namespaces. If the class name we included
            // contains namespace separators instead of underscores, use this as the
            // class name from now on.
            $classNameNS = str_replace('_', '\\', $className);
            if (class_exists($classNameNS, false) === true) {
                $className = $classNameNS;
            }

            // Skip abstract classes.
            $reflection = new ReflectionClass($className);
            if ($reflection->isAbstract() === true) {
                continue;
            }

            $listeners[$className] = $className;

            if (PHP_CODESNIFFER_VERBOSITY > 2) {
                echo "Registered $className".PHP_EOL;
            }
        }//end foreach

        $this->sniffs = $listeners;

    }//end registerSniffs()


    /**
     * Populates the array of PHP_CodeSniffer_Sniff's for this file.
     *
     * @return void
     * @throws PHP_CodeSniffer_Exception If sniff registration fails.
     */
    public function populateTokenListeners()
    {
        // Construct a list of listeners indexed by token being listened for.
        $this->_tokenListeners = array();

        foreach ($this->sniffs as $listenerClass) {
            // Work out the internal code for this sniff. Detect usage of namespace
            // separators instead of underscores to support PHP namespaces.
            if (strstr($listenerClass, '\\') === false) {
                $parts = explode('_', $listenerClass);
            } else {
                $parts = explode('\\', $listenerClass);
            }

            $code = $parts[0].'.'.$parts[2].'.'.$parts[3];
            $code = substr($code, 0, -5);

            $this->listeners[$listenerClass] = new $listenerClass();

            // Set custom properties.
            if (isset($this->ruleset[$code]['properties']) === true) {
                foreach ($this->ruleset[$code]['properties'] as $name => $value) {
                    $this->setSniffProperty($listenerClass, $name, $value);
                }
            }

            $tokenizers = array();
            $vars       = get_class_vars($listenerClass);
            if (isset($vars['supportedTokenizers']) === true) {
                foreach ($vars['supportedTokenizers'] as $tokenizer) {
                    $tokenizers[$tokenizer] = $tokenizer;
                }
            } else {
                $tokenizers = array('PHP' => 'PHP');
            }

            $tokens = $this->listeners[$listenerClass]->register();
            if (is_array($tokens) === false) {
                $msg = "Sniff $listenerClass register() method must return an array";
                throw new PHP_CodeSniffer_Exception($msg);
            }

            $parts          = explode('_', str_replace('\\', '_', $listenerClass));
            $listenerSource = $parts[0].'.'.$parts[2].'.'.substr($parts[3], 0, -5);
            $ignorePatterns = array();
            $patterns       = $this->getIgnorePatterns($listenerSource);
            foreach ($patterns as $pattern => $type) {
                // While there is support for a type of each pattern
                // (absolute or relative) we don't actually support it here.
                $replacements = array(
                                 '\\,' => ',',
                                 '*'   => '.*',
                                );

                $ignorePatterns[] = strtr($pattern, $replacements);
            }

            foreach ($tokens as $token) {
                if (isset($this->_tokenListeners[$token]) === false) {
                    $this->_tokenListeners[$token] = array();
                }

                if (isset($this->_tokenListeners[$token][$listenerClass]) === false) {
                    $this->_tokenListeners[$token][$listenerClass] = array(
                                                                      'class'      => $listenerClass,
                                                                      'source'     => $listenerSource,
                                                                      'tokenizers' => $tokenizers,
                                                                      'ignore'     => $ignorePatterns,
                                                                     );
                }
            }
        }//end foreach

    }//end populateTokenListeners()


    /**
     * Set a single property for a sniff.
     *
     * @param string $listenerClass The class name of the sniff.
     * @param string $name          The name of the property to change.
     * @param string $value         The new value of the property.
     *
     * @return void
     */
    public function setSniffProperty($listenerClass, $name, $value)
    {
        // Setting a property for a sniff we are not using.
        if (isset($this->listeners[$listenerClass]) === false) {
            return;
        }

        $name = trim($name);
        if (is_string($value) === true) {
            $value = trim($value);
        }

        // Special case for booleans.
        if ($value === 'true') {
            $value = true;
        } else if ($value === 'false') {
            $value = false;
        }

        $this->listeners[$listenerClass]->$name = $value;

    }//end setSniffProperty()


    /**
     * Get a list of files that will be processed.
     *
     * If passed directories, this method will find all files within them.
     * The method will also perform file extension and ignore pattern filtering.
     *
     * @param string  $paths A list of file or directory paths to process.
     * @param boolean $local If true, only process 1 level of files in directories
     *
     * @return array
     * @throws Exception If there was an error opening a directory.
     * @see    shouldProcessFile()
     */
    public function getFilesToProcess($paths, $local=false)
    {
        $files = array();

        foreach ($paths as $path) {
            if (is_dir($path) === true || self::isPharFile($path) === true) {
                if (self::isPharFile($path) === true) {
                    $path = 'phar://'.$path;
                }

                if ($local === true) {
                    $di = new DirectoryIterator($path);
                } else {
                    $di = new RecursiveIteratorIterator(
                        new RecursiveDirectoryIterator($path),
                        0,
                        RecursiveIteratorIterator::CATCH_GET_CHILD
                    );
                }

                foreach ($di as $file) {
                    // Check if the file exists after all symlinks are resolved.
                    $filePath = self::realpath($file->getPathname());
                    if ($filePath === false) {
                        continue;
                    }

                    if (is_dir($filePath) === true) {
                        continue;
                    }

                    if ($this->shouldProcessFile($file->getPathname(), $path) === false) {
                        continue;
                    }

                    $files[] = $file->getPathname();
                }//end foreach
            } else {
                if ($this->shouldIgnoreFile($path, dirname($path)) === true) {
                    continue;
                }

                $files[] = $path;
            }//end if
        }//end foreach

        return $files;

    }//end getFilesToProcess()


    /**
     * Checks filtering rules to see if a file should be checked.
     *
     * Checks both file extension filters and path ignore filters.
     *
     * @param string $path    The path to the file being checked.
     * @param string $basedir The directory to use for relative path checks.
     *
     * @return bool
     */
    public function shouldProcessFile($path, $basedir)
    {
        // Check that the file's extension is one we are checking.
        // We are strict about checking the extension and we don't
        // let files through with no extension or that start with a dot.
        $fileName  = basename($path);
        $fileParts = explode('.', $fileName);
        if ($fileParts[0] === $fileName || $fileParts[0] === '') {
            return false;
        }

        // Checking multi-part file extensions, so need to create a
        // complete extension list and make sure one is allowed.
        $extensions = array();
        array_shift($fileParts);
        foreach ($fileParts as $part) {
            $extensions[implode('.', $fileParts)] = 1;
            array_shift($fileParts);
        }

        $matches = array_intersect_key($extensions, $this->allowedFileExtensions);
        if (empty($matches) === true) {
            return false;
        }

        // If the file's path matches one of our ignore patterns, skip it.
        if ($this->shouldIgnoreFile($path, $basedir) === true) {
            return false;
        }

        return true;

    }//end shouldProcessFile()


    /**
     * Checks filtering rules to see if a file should be ignored.
     *
     * @param string $path    The path to the file being checked.
     * @param string $basedir The directory to use for relative path checks.
     *
     * @return bool
     */
    public function shouldIgnoreFile($path, $basedir)
    {
        $relativePath = $path;
        if (strpos($path, $basedir) === 0) {
            // The +1 cuts off the directory separator as well.
            $relativePath = substr($path, (strlen($basedir) + 1));
        }

        foreach ($this->ignorePatterns as $pattern => $type) {
            if (is_array($type) === true) {
                // A sniff specific ignore pattern.
                continue;
            }

            // Maintains backwards compatibility in case the ignore pattern does
            // not have a relative/absolute value.
            if (is_int($pattern) === true) {
                $pattern = $type;
                $type    = 'absolute';
            }

            $replacements = array(
                             '\\,' => ',',
                             '*'   => '.*',
                            );

            // We assume a / directory separator, as do the exclude rules
            // most developers write, so we need a special case for any system
            // that is different.
            if (DIRECTORY_SEPARATOR === '\\') {
                $replacements['/'] = '\\\\';
            }

            $pattern = strtr($pattern, $replacements);

            if ($type === 'relative') {
                $testPath = $relativePath;
            } else {
                $testPath = $path;
            }

            $pattern = '`'.$pattern.'`i';
            if (preg_match($pattern, $testPath) === 1) {
                return true;
            }
        }//end foreach

        return false;

    }//end shouldIgnoreFile()


    /**
     * Run the code sniffs over a single given file.
     *
     * Processes the file and runs the PHP_CodeSniffer sniffs to verify that it
     * conforms with the standard. Returns the processed file object, or NULL
     * if no file was processed due to error.
     *
     * @param string $file     The file to process.
     * @param string $contents The contents to parse. If NULL, the content
     *                         is taken from the file system.
     *
     * @return PHP_CodeSniffer_File
     * @throws PHP_CodeSniffer_Exception If the file could not be processed.
     * @see    _processFile()
     */
    public function processFile($file, $contents=null)
    {
        if ($contents === null && file_exists($file) === false) {
            throw new PHP_CodeSniffer_Exception("Source file $file does not exist");
        }

        $filePath = self::realpath($file);
        if ($filePath === false) {
            $filePath = $file;
        }

        // Before we go and spend time tokenizing this file, just check
        // to see if there is a tag up top to indicate that the whole
        // file should be ignored. It must be on one of the first two lines.
        $firstContent = $contents;
        if ($contents === null && is_readable($filePath) === true) {
            $handle = fopen($filePath, 'r');
            if ($handle !== false) {
                $firstContent  = fgets($handle);
                $firstContent .= fgets($handle);
                fclose($handle);

                if (strpos($firstContent, '@codingStandardsIgnoreFile') !== false) {
                    // We are ignoring the whole file.
                    if (PHP_CODESNIFFER_VERBOSITY > 0) {
                        echo 'Ignoring '.basename($filePath).PHP_EOL;
                    }

                    return null;
                }
            }
        }//end if

        try {
            $phpcsFile = $this->_processFile($file, $contents);
        } catch (Exception $e) {
            $trace = $e->getTrace();

            $filename = $trace[0]['args'][0];
            if (is_object($filename) === true
                && get_class($filename) === 'PHP_CodeSniffer_File'
            ) {
                $filename = $filename->getFilename();
            } else if (is_numeric($filename) === true) {
                // See if we can find the PHP_CodeSniffer_File object.
                foreach ($trace as $data) {
                    if (isset($data['args'][0]) === true
                        && ($data['args'][0] instanceof PHP_CodeSniffer_File) === true
                    ) {
                        $filename = $data['args'][0]->getFilename();
                    }
                }
            } else if (is_string($filename) === false) {
                $filename = (string) $filename;
            }

            $errorMessage = '"'.$e->getMessage().'" at '.$e->getFile().':'.$e->getLine();
            $error        = "An error occurred during processing; checking has been aborted. The error message was: $errorMessage";

            $phpcsFile = new PHP_CodeSniffer_File(
                $filename,
                $this->_tokenListeners,
                $this->ruleset,
                $this
            );

            $phpcsFile->addError($error, null);
        }//end try

        $cliValues = $this->cli->getCommandLineValues();

        if (PHP_CODESNIFFER_INTERACTIVE === false) {
            // Cache the report data for this file so we can unset it to save memory.
            $this->reporting->cacheFileReport($phpcsFile, $cliValues);
            $phpcsFile->cleanUp();
            return $phpcsFile;
        }

        /*
            Running interactively.
            Print the error report for the current file and then wait for user input.
        */

        // Get current violations and then clear the list to make sure
        // we only print violations for a single file each time.
        $numErrors = null;
        while ($numErrors !== 0) {
            $numErrors = ($phpcsFile->getErrorCount() + $phpcsFile->getWarningCount());
            if ($numErrors === 0) {
                continue;
            }

            $reportClass = $this->reporting->factory('full');
            $reportData  = $this->reporting->prepareFileReport($phpcsFile);
            $reportClass->generateFileReport($reportData, $phpcsFile, $cliValues['showSources'], $cliValues['reportWidth']);

            echo '<ENTER> to recheck, [s] to skip or [q] to quit : ';
            $input = fgets(STDIN);
            $input = trim($input);

            switch ($input) {
            case 's':
                break(2);
            case 'q':
                exit(0);
                break;
            default:
                // Repopulate the sniffs because some of them save their state
                // and only clear it when the file changes, but we are rechecking
                // the same file.
                $this->populateTokenListeners();
                $phpcsFile = $this->_processFile($file, $contents);
                break;
            }
        }//end while

        return $phpcsFile;

    }//end processFile()


    /**
     * Process the sniffs for a single file.
     *
     * Does raw processing only. No interactive support or error checking.
     *
     * @param string $file     The file to process.
     * @param string $contents The contents to parse. If NULL, the content
     *                         is taken from the file system.
     *
     * @return PHP_CodeSniffer_File
     * @see    processFile()
     */
    private function _processFile($file, $contents)
    {
        $stdin     = false;
        $cliValues = $this->cli->getCommandLineValues();
        if (empty($cliValues['files']) === true) {
            $stdin = true;
        }

        if (PHP_CODESNIFFER_VERBOSITY > 0 || (PHP_CODESNIFFER_CBF === true && $stdin === false)) {
            $startTime = microtime(true);
            echo 'Processing '.basename($file).' ';
            if (PHP_CODESNIFFER_VERBOSITY > 1) {
                echo PHP_EOL;
            }
        }

        $phpcsFile = new PHP_CodeSniffer_File(
            $file,
            $this->_tokenListeners,
            $this->ruleset,
            $this
        );

        $phpcsFile->start($contents);

        if (PHP_CODESNIFFER_VERBOSITY > 0 || (PHP_CODESNIFFER_CBF === true && $stdin === false)) {
            $timeTaken = ((microtime(true) - $startTime) * 1000);
            if ($timeTaken < 1000) {
                $timeTaken = round($timeTaken);
                echo "DONE in {$timeTaken}ms";
            } else {
                $timeTaken = round(($timeTaken / 1000), 2);
                echo "DONE in $timeTaken secs";
            }

            if (PHP_CODESNIFFER_CBF === true) {
                $errors = $phpcsFile->getFixableCount();
                echo " ($errors fixable violations)".PHP_EOL;
            } else {
                $errors   = $phpcsFile->getErrorCount();
                $warnings = $phpcsFile->getWarningCount();
                echo " ($errors errors, $warnings warnings)".PHP_EOL;
            }
        }

        return $phpcsFile;

    }//end _processFile()


    /**
     * Generates documentation for a coding standard.
     *
     * @param string $standard  The standard to generate docs for
     * @param array  $sniffs    A list of sniffs to limit the docs to.
     * @param string $generator The name of the generator class to use.
     *
     * @return void
     */
    public function generateDocs($standard, array $sniffs=array(), $generator='Text')
    {
        if (class_exists('PHP_CodeSniffer_DocGenerators_'.$generator, true) === false) {
            throw new PHP_CodeSniffer_Exception('Class PHP_CodeSniffer_DocGenerators_'.$generator.' not found');
        }

        $class     = "PHP_CodeSniffer_DocGenerators_$generator";
        $generator = new $class($standard, $sniffs);

        $generator->generate();

    }//end generateDocs()


    /**
     * Gets the array of PHP_CodeSniffer_Sniff's.
     *
     * @return PHP_CodeSniffer_Sniff[]
     */
    public function getSniffs()
    {
        return $this->listeners;

    }//end getSniffs()


    /**
     * Gets the array of PHP_CodeSniffer_Sniff's indexed by token type.
     *
     * @return array
     */
    public function getTokenSniffs()
    {
        return $this->_tokenListeners;

    }//end getTokenSniffs()


    /**
     * Returns true if the specified string is in the camel caps format.
     *
     * @param string  $string      The string the verify.
     * @param boolean $classFormat If true, check to see if the string is in the
     *                             class format. Class format strings must start
     *                             with a capital letter and contain no
     *                             underscores.
     * @param boolean $public      If true, the first character in the string
     *                             must be an a-z character. If false, the
     *                             character must be an underscore. This
     *                             argument is only applicable if $classFormat
     *                             is false.
     * @param boolean $strict      If true, the string must not have two capital
     *                             letters next to each other. If false, a
     *                             relaxed camel caps policy is used to allow
     *                             for acronyms.
     *
     * @return boolean
     */
    public static function isCamelCaps(
        $string,
        $classFormat=false,
        $public=true,
        $strict=true
    ) {
        // Check the first character first.
        if ($classFormat === false) {
            $legalFirstChar = '';
            if ($public === false) {
                $legalFirstChar = '[_]';
            }

            if ($strict === false) {
                // Can either start with a lowercase letter, or multiple uppercase
                // in a row, representing an acronym.
                $legalFirstChar .= '([A-Z]{2,}|[a-z])';
            } else {
                $legalFirstChar .= '[a-z]';
            }
        } else {
            $legalFirstChar = '[A-Z]';
        }

        if (preg_match("/^$legalFirstChar/", $string) === 0) {
            return false;
        }

        // Check that the name only contains legal characters.
        $legalChars = 'a-zA-Z0-9';
        if (preg_match("|[^$legalChars]|", substr($string, 1)) > 0) {
            return false;
        }

        if ($strict === true) {
            // Check that there are not two capital letters next to each other.
            $length          = strlen($string);
            $lastCharWasCaps = $classFormat;

            for ($i = 1; $i < $length; $i++) {
                $ascii = ord($string{$i});
                if ($ascii >= 48 && $ascii <= 57) {
                    // The character is a number, so it cant be a capital.
                    $isCaps = false;
                } else {
                    if (strtoupper($string{$i}) === $string{$i}) {
                        $isCaps = true;
                    } else {
                        $isCaps = false;
                    }
                }

                if ($isCaps === true && $lastCharWasCaps === true) {
                    return false;
                }

                $lastCharWasCaps = $isCaps;
            }
        }//end if

        return true;

    }//end isCamelCaps()


    /**
     * Returns true if the specified string is in the underscore caps format.
     *
     * @param string $string The string to verify.
     *
     * @return boolean
     */
    public static function isUnderscoreName($string)
    {
        // If there are space in the name, it can't be valid.
        if (strpos($string, ' ') !== false) {
            return false;
        }

        $validName = true;
        $nameBits  = explode('_', $string);

        if (preg_match('|^[A-Z]|', $string) === 0) {
            // Name does not begin with a capital letter.
            $validName = false;
        } else {
            foreach ($nameBits as $bit) {
                if ($bit === '') {
                    continue;
                }

                if ($bit{0} !== strtoupper($bit{0})) {
                    $validName = false;
                    break;
                }
            }
        }

        return $validName;

    }//end isUnderscoreName()


    /**
     * Returns a valid variable type for param/var tag.
     *
     * If type is not one of the standard type, it must be a custom type.
     * Returns the correct type name suggestion if type name is invalid.
     *
     * @param string $varType The variable type to process.
     *
     * @return string
     */
    public static function suggestType($varType)
    {
        if ($varType === '') {
            return '';
        }

        if (in_array($varType, self::$allowedTypes) === true) {
            return $varType;
        } else {
            $lowerVarType = strtolower($varType);
            switch ($lowerVarType) {
            case 'bool':
                return 'boolean';
            case 'double':
            case 'real':
                return 'float';
            case 'int':
                return 'integer';
            case 'array()':
                return 'array';
            }//end switch

            if (strpos($lowerVarType, 'array(') !== false) {
                // Valid array declaration:
                // array, array(type), array(type1 => type2).
                $matches = array();
                $pattern = '/^array\(\s*([^\s^=^>]*)(\s*=>\s*(.*))?\s*\)/i';
                if (preg_match($pattern, $varType, $matches) !== 0) {
                    $type1 = '';
                    if (isset($matches[1]) === true) {
                        $type1 = $matches[1];
                    }

                    $type2 = '';
                    if (isset($matches[3]) === true) {
                        $type2 = $matches[3];
                    }

                    $type1 = self::suggestType($type1);
                    $type2 = self::suggestType($type2);
                    if ($type2 !== '') {
                        $type2 = ' => '.$type2;
                    }

                    return "array($type1$type2)";
                } else {
                    return 'array';
                }//end if
            } else if (in_array($lowerVarType, self::$allowedTypes) === true) {
                // A valid type, but not lower cased.
                return $lowerVarType;
            } else {
                // Must be a custom type name.
                return $varType;
            }//end if
        }//end if

    }//end suggestType()


    /**
     * Prepares token content for output to screen.
     *
     * Replaces invisible characters so they are visible. On non-Windows
     * OSes it will also colour the invisible characters.
     *
     * @param string $content The content to prepare.
     *
     * @return string
     */
    public static function prepareForOutput($content)
    {
        if (strtoupper(substr(PHP_OS, 0, 3)) === 'WIN') {
            $content = str_replace("\r", '\r', $content);
            $content = str_replace("\n", '\n', $content);
            $content = str_replace("\t", '\t', $content);
        } else {
            $content = str_replace("\r", "\033[30;1m\\r\033[0m", $content);
            $content = str_replace("\n", "\033[30;1m\\n\033[0m", $content);
            $content = str_replace("\t", "\033[30;1m\\t\033[0m", $content);
            $content = str_replace(' ', "\033[30;1m·\033[0m", $content);
        }

        return $content;

    }//end prepareForOutput()


    /**
     * Get a list paths where standards are installed.
     *
     * @return array
     */
    public static function getInstalledStandardPaths()
    {
        $installedPaths = array(dirname(__FILE__).DIRECTORY_SEPARATOR.'CodeSniffer'.DIRECTORY_SEPARATOR.'Standards');
        $configPaths    = PHP_CodeSniffer::getConfigData('installed_paths');
        if ($configPaths !== null) {
            $installedPaths = array_merge($installedPaths, explode(',', $configPaths));
        }

        $resolvedInstalledPaths = array();
        foreach ($installedPaths as $installedPath) {
            if (substr($installedPath, 0, 1) === '.') {
                $installedPath = dirname(__FILE__).DIRECTORY_SEPARATOR.$installedPath;
            }

            $resolvedInstalledPaths[] = $installedPath;
        }

        return $resolvedInstalledPaths;

    }//end getInstalledStandardPaths()


    /**
     * Get a list of all coding standards installed.
     *
     * Coding standards are directories located in the
     * CodeSniffer/Standards directory. Valid coding standards
     * include a Sniffs subdirectory.
     *
     * @param boolean $includeGeneric If true, the special "Generic"
     *                                coding standard will be included
     *                                if installed.
     * @param string  $standardsDir   A specific directory to look for standards
     *                                in. If not specified, PHP_CodeSniffer will
     *                                look in its default locations.
     *
     * @return array
     * @see    isInstalledStandard()
     */
    public static function getInstalledStandards(
        $includeGeneric=false,
        $standardsDir=''
    ) {
        $installedStandards = array();

        if ($standardsDir === '') {
            $installedPaths = self::getInstalledStandardPaths();
        } else {
            $installedPaths = array($standardsDir);
        }

        foreach ($installedPaths as $standardsDir) {
            $di = new DirectoryIterator($standardsDir);
            foreach ($di as $file) {
                if ($file->isDir() === true && $file->isDot() === false) {
                    $filename = $file->getFilename();

                    // Ignore the special "Generic" standard.
                    if ($includeGeneric === false && $filename === 'Generic') {
                        continue;
                    }

                    // Valid coding standard dirs include a ruleset.
                    $csFile = $file->getPathname().'/ruleset.xml';
                    if (is_file($csFile) === true) {
                        $installedStandards[] = $filename;
                    }
                }
            }
        }//end foreach

        return $installedStandards;

    }//end getInstalledStandards()


    /**
     * Determine if a standard is installed.
     *
     * Coding standards are directories located in the
     * CodeSniffer/Standards directory. Valid coding standards
     * include a ruleset.xml file.
     *
     * @param string $standard The name of the coding standard.
     *
     * @return boolean
     * @see    getInstalledStandards()
     */
    public static function isInstalledStandard($standard)
    {
        $path = self::getInstalledStandardPath($standard);
        if ($path !== null && strpos($path, 'ruleset.xml') !== false) {
            return true;
        } else {
            // This could be a custom standard, installed outside our
            // standards directory.
            $standard = self::realPath($standard);

            // Might be an actual ruleset file itself.
            // If it has an XML extension, let's at least try it.
            if (is_file($standard) === true
                && substr(strtolower($standard), -4) === '.xml'
            ) {
                return true;
            }

            // If it is a directory with a ruleset.xml file in it,
            // it is a standard.
            $ruleset = rtrim($standard, ' /\\').DIRECTORY_SEPARATOR.'ruleset.xml';
            if (is_file($ruleset) === true) {
                return true;
            }
        }//end if

        return false;

    }//end isInstalledStandard()


    /**
     * Return the path of an installed coding standard.
     *
     * Coding standards are directories located in the
     * CodeSniffer/Standards directory. Valid coding standards
     * include a ruleset.xml file.
     *
     * @param string $standard The name of the coding standard.
     *
     * @return string|null
     */
    public static function getInstalledStandardPath($standard)
    {
        $installedPaths = self::getInstalledStandardPaths();
        foreach ($installedPaths as $installedPath) {
            $standardPath = $installedPath.DIRECTORY_SEPARATOR.$standard;
            $path         = self::realpath($standardPath.DIRECTORY_SEPARATOR.'ruleset.xml');
            if (is_file($path) === true) {
                return $path;
            } else if (self::isPharFile($standardPath) === true) {
                $path = self::realpath($standardPath);
                if ($path !== false) {
                    return $path;
                }
            }
        }

        return null;

    }//end getInstalledStandardPath()


    /**
     * Get a single config value.
     *
     * Config data is stored in the data dir, in a file called
     * CodeSniffer.conf. It is a simple PHP array.
     *
     * @param string $key The name of the config value.
     *
     * @return string|null
     * @see    setConfigData()
     * @see    getAllConfigData()
     */
    public static function getConfigData($key)
    {
        $phpCodeSnifferConfig = self::getAllConfigData();

        if ($phpCodeSnifferConfig === null) {
            return null;
        }

        if (isset($phpCodeSnifferConfig[$key]) === false) {
            return null;
        }

        return $phpCodeSnifferConfig[$key];

    }//end getConfigData()


    /**
     * Set a single config value.
     *
     * Config data is stored in the data dir, in a file called
     * CodeSniffer.conf. It is a simple PHP array.
     *
     * @param string      $key   The name of the config value.
     * @param string|null $value The value to set. If null, the config
     *                           entry is deleted, reverting it to the
     *                           default value.
     * @param boolean     $temp  Set this config data temporarily for this
     *                           script run. This will not write the config
     *                           data to the config file.
     *
     * @return boolean
     * @see    getConfigData()
     * @throws PHP_CodeSniffer_Exception If the config file can not be written.
     */
    public static function setConfigData($key, $value, $temp=false)
    {
        if ($temp === false) {
            $configFile = dirname(__FILE__).'/CodeSniffer.conf';
            if (is_file($configFile) === false
                && strpos('@data_dir@', '@data_dir') === false
            ) {
                // If data_dir was replaced, this is a PEAR install and we can
                // use the PEAR data dir to store the conf file.
                $configFile = '@data_dir@/PHP_CodeSniffer/CodeSniffer.conf';
            }

            if (is_file($configFile) === true
                && is_writable($configFile) === false
            ) {
                $error = 'Config file '.$configFile.' is not writable';
                throw new PHP_CodeSniffer_Exception($error);
            }
        }

        $phpCodeSnifferConfig = self::getAllConfigData();

        if ($value === null) {
            if (isset($phpCodeSnifferConfig[$key]) === true) {
                unset($phpCodeSnifferConfig[$key]);
            }
        } else {
            $phpCodeSnifferConfig[$key] = $value;
        }

        if ($temp === false) {
            $output  = '<'.'?php'."\n".' $phpCodeSnifferConfig = ';
            $output .= var_export($phpCodeSnifferConfig, true);
            $output .= "\n?".'>';

            if (file_put_contents($configFile, $output) === false) {
                return false;
            }
        }

        $GLOBALS['PHP_CODESNIFFER_CONFIG_DATA'] = $phpCodeSnifferConfig;

        return true;

    }//end setConfigData()


    /**
     * Get all config data in an array.
     *
     * @return array<string, string>
     * @see    getConfigData()
     */
    public static function getAllConfigData()
    {
        if (isset($GLOBALS['PHP_CODESNIFFER_CONFIG_DATA']) === true) {
            return $GLOBALS['PHP_CODESNIFFER_CONFIG_DATA'];
        }

        $configFile = dirname(__FILE__).'/CodeSniffer.conf';
        if (is_file($configFile) === false) {
            $configFile = '@data_dir@/PHP_CodeSniffer/CodeSniffer.conf';
        }

        if (is_file($configFile) === false) {
            $GLOBALS['PHP_CODESNIFFER_CONFIG_DATA'] = array();
            return array();
        }

        include $configFile;
        $GLOBALS['PHP_CODESNIFFER_CONFIG_DATA'] = $phpCodeSnifferConfig;
        return $GLOBALS['PHP_CODESNIFFER_CONFIG_DATA'];

    }//end getAllConfigData()


    /**
     * Return TRUE, if the path is a phar file.
     *
     * @param string $path The path to use.
     *
     * @return mixed
     */
    public static function isPharFile($path)
    {
        if (strpos($path, 'phar://') === 0) {
            return true;
        }

        return false;

    }//end isPharFile()


    /**
     * CodeSniffer alternative for realpath.
     *
     * Allows for phar support.
     *
     * @param string $path The path to use.
     *
     * @return mixed
     */
    public static function realpath($path)
    {
        // Support the path replacement of ~ with the user's home directory.
        if (substr($path, 0, 2) === '~/') {
            $homeDir = getenv('HOME');
            if ($homeDir !== false) {
                $path = $homeDir.substr($path, 1);
            }
        }

        // No extra work needed if this is not a phar file.
        if (self::isPharFile($path) === false) {
            return realpath($path);
        }

        // Before trying to break down the file path,
        // check if it exists first because it will mostly not
        // change after running the below code.
        if (file_exists($path) === true) {
            return $path;
        }

        $phar  = Phar::running(false);
        $extra = str_replace('phar://'.$phar, '', $path);
        $path  = realpath($phar);
        if ($path === false) {
            return false;
        }

        $path = 'phar://'.$path.$extra;
        if (file_exists($path) === true) {
            return $path;
        }

        return false;

    }//end realpath()


    /**
     * CodeSniffer alternative for chdir().
     *
     * Allows for phar support.
     *
     * @param string $path The path to use.
     *
     * @return void
     */
    public static function chdir($path)
    {
        if (self::isPharFile($path) === true) {
            $phar = Phar::running(false);
            chdir(dirname($phar));
        } else {
            chdir($path);
        }

    }//end chdir()


}//end class
