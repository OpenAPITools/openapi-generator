<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * A TestRunner for the Command Line Interface (CLI)
 * PHP SAPI Module.
 *
 * @package    PHPUnit
 * @subpackage TextUI
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.0.0
 */
class PHPUnit_TextUI_Command
{
    /**
     * @var array
     */
    protected $arguments = array(
      'listGroups'              => false,
      'loader'                  => null,
      'useDefaultConfiguration' => true
    );

    /**
     * @var array
     */
    protected $options = array();

    /**
     * @var array
     */
    protected $longOptions = array(
      'colors==' => null,
      'bootstrap=' => null,
      'columns=' => null,
      'configuration=' => null,
      'coverage-clover=' => null,
      'coverage-crap4j=' => null,
      'coverage-html=' => null,
      'coverage-php=' => null,
      'coverage-text==' => null,
      'coverage-xml=' => null,
      'debug' => null,
      'exclude-group=' => null,
      'filter=' => null,
      'testsuite=' => null,
      'group=' => null,
      'help' => null,
      'include-path=' => null,
      'list-groups' => null,
      'loader=' => null,
      'log-json=' => null,
      'log-junit=' => null,
      'log-tap=' => null,
      'process-isolation' => null,
      'repeat=' => null,
      'stderr' => null,
      'stop-on-error' => null,
      'stop-on-failure' => null,
      'stop-on-incomplete' => null,
      'stop-on-risky' => null,
      'stop-on-skipped' => null,
      'report-useless-tests' => null,
      'strict-coverage' => null,
      'disallow-test-output' => null,
      'enforce-time-limit' => null,
      'disallow-todo-tests' => null,
      'strict-global-state' => null,
      'strict' => null,
      'tap' => null,
      'testdox' => null,
      'testdox-html=' => null,
      'testdox-text=' => null,
      'test-suffix=' => null,
      'no-configuration' => null,
      'no-globals-backup' => null,
      'printer=' => null,
      'static-backup' => null,
      'verbose' => null,
      'version' => null
    );

    /**
     * @var boolean
     */
    private $versionStringPrinted = false;

    /**
     * @param boolean $exit
     */
    public static function main($exit = true)
    {
        $command = new static;

        return $command->run($_SERVER['argv'], $exit);
    }

    /**
     * @param  array $argv
     * @param  boolean $exit
     * @return integer
     */
    public function run(array $argv, $exit = true)
    {
        $this->handleArguments($argv);

        $runner = $this->createRunner();

        if (is_object($this->arguments['test']) &&
            $this->arguments['test'] instanceof PHPUnit_Framework_Test) {
            $suite = $this->arguments['test'];
        } else {
            $suite = $runner->getTest(
                $this->arguments['test'],
                $this->arguments['testFile'],
                $this->arguments['testSuffixes']
            );
        }

        if ($this->arguments['listGroups']) {
            $this->printVersionString();

            print "Available test group(s):\n";

            $groups = $suite->getGroups();
            sort($groups);

            foreach ($groups as $group) {
                print " - $group\n";
            }

            if ($exit) {
                exit(PHPUnit_TextUI_TestRunner::SUCCESS_EXIT);
            } else {
                return PHPUnit_TextUI_TestRunner::SUCCESS_EXIT;
            }
        }

        unset($this->arguments['test']);
        unset($this->arguments['testFile']);

        try {
            $result = $runner->doRun($suite, $this->arguments);
        } catch (PHPUnit_Framework_Exception $e) {
            print $e->getMessage() . "\n";
        }

        $ret = PHPUnit_TextUI_TestRunner::FAILURE_EXIT;

        if (isset($result) && $result->wasSuccessful()) {
            $ret = PHPUnit_TextUI_TestRunner::SUCCESS_EXIT;
        } elseif (!isset($result) || $result->errorCount() > 0) {
            $ret = PHPUnit_TextUI_TestRunner::EXCEPTION_EXIT;
        }

        if ($exit) {
            exit($ret);
        } else {
            return $ret;
        }
    }

    /**
     * Create a TestRunner, override in subclasses.
     *
     * @return PHPUnit_TextUI_TestRunner
     * @since  Method available since Release 3.6.0
     */
    protected function createRunner()
    {
        return new PHPUnit_TextUI_TestRunner($this->arguments['loader']);
    }

    /**
     * Handles the command-line arguments.
     *
     * A child class of PHPUnit_TextUI_Command can hook into the argument
     * parsing by adding the switch(es) to the $longOptions array and point to a
     * callback method that handles the switch(es) in the child class like this
     *
     * <code>
     * <?php
     * class MyCommand extends PHPUnit_TextUI_Command
     * {
     *     public function __construct()
     *     {
     *         // my-switch won't accept a value, it's an on/off
     *         $this->longOptions['my-switch'] = 'myHandler';
     *         // my-secondswitch will accept a value - note the equals sign
     *         $this->longOptions['my-secondswitch='] = 'myOtherHandler';
     *     }
     *
     *     // --my-switch  -> myHandler()
     *     protected function myHandler()
     *     {
     *     }
     *
     *     // --my-secondswitch foo -> myOtherHandler('foo')
     *     protected function myOtherHandler ($value)
     *     {
     *     }
     *
     *     // You will also need this - the static keyword in the
     *     // PHPUnit_TextUI_Command will mean that it'll be
     *     // PHPUnit_TextUI_Command that gets instantiated,
     *     // not MyCommand
     *     public static function main($exit = true)
     *     {
     *         $command = new static;
     *
     *         return $command->run($_SERVER['argv'], $exit);
     *     }
     *
     * }
     * </code>
     *
     * @param array $argv
     */
    protected function handleArguments(array $argv)
    {
        if (defined('__PHPUNIT_PHAR__')) {
            $this->longOptions['selfupdate']  = null;
            $this->longOptions['self-update'] = null;
        }

        try {
            $this->options = PHPUnit_Util_Getopt::getopt(
                $argv,
                'd:c:hv',
                array_keys($this->longOptions)
            );
        } catch (PHPUnit_Framework_Exception $e) {
            $this->showError($e->getMessage());
        }

        foreach ($this->options[0] as $option) {
            switch ($option[0]) {
                case '--colors': {
                    $this->arguments['colors'] = $option[1] ?: PHPUnit_TextUI_ResultPrinter::COLOR_AUTO;
                    }
                break;

                case '--bootstrap': {
                    $this->arguments['bootstrap'] = $option[1];
                    }
                break;

                case '--columns': {
                    if (is_numeric($option[1])) {
                        $this->arguments['columns'] = (int) $option[1];
                    } elseif ($option[1] == 'max') {
                        $this->arguments['columns'] = 'max';
                    }
                }
                break;

                case 'c':
                case '--configuration': {
                    $this->arguments['configuration'] = $option[1];
                    }
                break;

                case '--coverage-clover': {
                    $this->arguments['coverageClover'] = $option[1];
                    }
                break;

                case '--coverage-crap4j': {
                    $this->arguments['coverageCrap4J'] = $option[1];
                    }
                break;

                case '--coverage-html': {
                    $this->arguments['coverageHtml'] = $option[1];
                    }
                break;

                case '--coverage-php': {
                    $this->arguments['coveragePHP'] = $option[1];
                    }
                break;

                case '--coverage-text': {
                    if ($option[1] === null) {
                        $option[1] = 'php://stdout';
                    }

                    $this->arguments['coverageText'] = $option[1];
                    $this->arguments['coverageTextShowUncoveredFiles'] = false;
                    $this->arguments['coverageTextShowOnlySummary'] = false;
                    }
                break;

                case '--coverage-xml': {
                    $this->arguments['coverageXml'] = $option[1];
                    }
                break;

                case 'd': {
                    $ini = explode('=', $option[1]);

                    if (isset($ini[0])) {
                        if (isset($ini[1])) {
                            ini_set($ini[0], $ini[1]);
                        } else {
                            ini_set($ini[0], true);
                        }
                    }
                    }
                break;

                case '--debug': {
                    $this->arguments['debug'] = true;
                    }
                break;

                case 'h':
                case '--help': {
                    $this->showHelp();
                    exit(PHPUnit_TextUI_TestRunner::SUCCESS_EXIT);
                    }
                break;

                case '--filter': {
                    $this->arguments['filter'] = $option[1];
                    }
                break;

                case '--testsuite': {
                    $this->arguments['testsuite'] = $option[1];
                    }
                break;

                case '--group': {
                    $this->arguments['groups'] = explode(',', $option[1]);
                    }
                break;

                case '--exclude-group': {
                    $this->arguments['excludeGroups'] = explode(
                        ',', $option[1]
                    );
                    }
                break;

                case '--test-suffix': {
                    $this->arguments['testSuffixes'] = explode(
                        ',', $option[1]
                    );
                    }
                break;

                case '--include-path': {
                    $includePath = $option[1];
                    }
                break;

                case '--list-groups': {
                    $this->arguments['listGroups'] = true;
                    }
                break;

                case '--printer': {
                    $this->arguments['printer'] = $option[1];
                    }
                break;

                case '--loader': {
                    $this->arguments['loader'] = $option[1];
                    }
                break;

                case '--log-json': {
                    $this->arguments['jsonLogfile'] = $option[1];
                    }
                break;

                case '--log-junit': {
                    $this->arguments['junitLogfile'] = $option[1];
                    }
                break;

                case '--log-tap': {
                    $this->arguments['tapLogfile'] = $option[1];
                    }
                break;

                case '--process-isolation': {
                    $this->arguments['processIsolation'] = true;
                    }
                break;

                case '--repeat': {
                    $this->arguments['repeat'] = (int) $option[1];
                    }
                break;

                case '--stderr': {
                    $this->arguments['stderr'] = true;
                    }
                break;

                case '--stop-on-error': {
                    $this->arguments['stopOnError'] = true;
                    }
                break;

                case '--stop-on-failure': {
                    $this->arguments['stopOnFailure'] = true;
                    }
                break;

                case '--stop-on-incomplete': {
                    $this->arguments['stopOnIncomplete'] = true;
                    }
                break;

                case '--stop-on-risky': {
                    $this->arguments['stopOnRisky'] = true;
                    }
                break;

                case '--stop-on-skipped': {
                    $this->arguments['stopOnSkipped'] = true;
                    }
                break;

                case '--tap': {
                    $this->arguments['printer'] = 'PHPUnit_Util_Log_TAP';
                    }
                break;

                case '--testdox': {
                    $this->arguments['printer'] = 'PHPUnit_Util_TestDox_ResultPrinter_Text';
                    }
                break;

                case '--testdox-html': {
                    $this->arguments['testdoxHTMLFile'] = $option[1];
                    }
                break;

                case '--testdox-text': {
                    $this->arguments['testdoxTextFile'] = $option[1];
                    }
                break;

                case '--no-configuration': {
                    $this->arguments['useDefaultConfiguration'] = false;
                    }
                break;

                case '--no-globals-backup': {
                    $this->arguments['backupGlobals'] = false;
                    }
                break;

                case '--static-backup': {
                    $this->arguments['backupStaticAttributes'] = true;
                    }
                break;

                case 'v':
                case '--verbose': {
                    $this->arguments['verbose'] = true;
                    }
                break;

                case '--version': {
                    $this->printVersionString();
                    exit(PHPUnit_TextUI_TestRunner::SUCCESS_EXIT);
                    }
                break;

                case '--report-useless-tests': {
                    $this->arguments['reportUselessTests'] = true;
                    }
                break;

                case '--strict-coverage': {
                    $this->arguments['strictCoverage'] = true;
                    }
                break;

                case '--strict-global-state': {
                    $this->arguments['disallowChangesToGlobalState'] = true;
                }
                break;

                case '--disallow-test-output': {
                    $this->arguments['disallowTestOutput'] = true;
                    }
                break;

                case '--enforce-time-limit': {
                    $this->arguments['enforceTimeLimit'] = true;
                    }
                break;

                case '--disallow-todo-tests': {
                    $this->arguments['disallowTodoAnnotatedTests'] = true;
                    }
                break;

                case '--strict': {
                    $this->arguments['reportUselessTests']         = true;
                    $this->arguments['strictCoverage']             = true;
                    $this->arguments['disallowTestOutput']         = true;
                    $this->arguments['enforceTimeLimit']           = true;
                    $this->arguments['disallowTodoAnnotatedTests'] = true;
                    $this->arguments['deprecatedStrictModeOption'] = true;
                    }
                break;

                case '--selfupdate':
                case '--self-update': {
                    $this->handleSelfUpdate();
                    }
                break;

                default: {
                    $optionName = str_replace('--', '', $option[0]);

                    if (isset($this->longOptions[$optionName])) {
                        $handler = $this->longOptions[$optionName];
                    } elseif (isset($this->longOptions[$optionName . '='])) {
                        $handler = $this->longOptions[$optionName . '='];
                    }

                    if (isset($handler) && is_callable(array($this, $handler))) {
                        $this->$handler($option[1]);
                    }
                    }
            }
        }

        $this->handleCustomTestSuite();

        if (!isset($this->arguments['test'])) {
            if (isset($this->options[1][0])) {
                $this->arguments['test'] = $this->options[1][0];
            }

            if (isset($this->options[1][1])) {
                $this->arguments['testFile'] = realpath($this->options[1][1]);
            } else {
                $this->arguments['testFile'] = '';
            }

            if (isset($this->arguments['test']) &&
                is_file($this->arguments['test']) &&
                substr($this->arguments['test'], -5, 5) != '.phpt') {
                $this->arguments['testFile'] = realpath($this->arguments['test']);
                $this->arguments['test']     = substr($this->arguments['test'], 0, strrpos($this->arguments['test'], '.'));
            }
        }

        if (!isset($this->arguments['testSuffixes'])) {
            $this->arguments['testSuffixes'] = array('Test.php', '.phpt');
        }

        if (isset($includePath)) {
            ini_set(
                'include_path',
                $includePath . PATH_SEPARATOR . ini_get('include_path')
            );
        }

        if ($this->arguments['loader'] !== null) {
            $this->arguments['loader'] = $this->handleLoader($this->arguments['loader']);
        }

        if (isset($this->arguments['configuration']) &&
            is_dir($this->arguments['configuration'])) {
            $configurationFile = $this->arguments['configuration'] .
                                 '/phpunit.xml';

            if (file_exists($configurationFile)) {
                $this->arguments['configuration'] = realpath(
                    $configurationFile
                );
            } elseif (file_exists($configurationFile . '.dist')) {
                $this->arguments['configuration'] = realpath(
                    $configurationFile . '.dist'
                );
            }
        } elseif (!isset($this->arguments['configuration']) &&
                 $this->arguments['useDefaultConfiguration']) {
            if (file_exists('phpunit.xml')) {
                $this->arguments['configuration'] = realpath('phpunit.xml');
            } elseif (file_exists('phpunit.xml.dist')) {
                $this->arguments['configuration'] = realpath(
                    'phpunit.xml.dist'
                );
            }
        }

        if (isset($this->arguments['configuration'])) {
            try {
                $configuration = PHPUnit_Util_Configuration::getInstance(
                    $this->arguments['configuration']
                );
            } catch (Exception $e) {
                print $e->getMessage() . "\n";
                exit(PHPUnit_TextUI_TestRunner::FAILURE_EXIT);
            }

            $phpunit = $configuration->getPHPUnitConfiguration();

            $configuration->handlePHPConfiguration();

            /**
             * Issue #1216
             */
            if (isset($this->arguments['bootstrap'])) {
                $this->handleBootstrap($this->arguments['bootstrap']);
            } elseif (isset($phpunit['bootstrap'])) {
                $this->handleBootstrap($phpunit['bootstrap']);
            }

            /**
             * Issue #657
             */
            if (isset($phpunit['stderr']) && ! isset($this->arguments['stderr'])) {
                $this->arguments['stderr'] = $phpunit['stderr'];
            }

            if (isset($phpunit['printerClass'])) {
                if (isset($phpunit['printerFile'])) {
                    $file = $phpunit['printerFile'];
                } else {
                    $file = '';
                }

                $this->arguments['printer'] = $this->handlePrinter(
                    $phpunit['printerClass'], $file
                );
            }

            if (isset($phpunit['testSuiteLoaderClass'])) {
                if (isset($phpunit['testSuiteLoaderFile'])) {
                    $file = $phpunit['testSuiteLoaderFile'];
                } else {
                    $file = '';
                }

                $this->arguments['loader'] = $this->handleLoader(
                    $phpunit['testSuiteLoaderClass'], $file
                );
            }

            $browsers = $configuration->getSeleniumBrowserConfiguration();

            if (!empty($browsers) &&
                class_exists('PHPUnit_Extensions_SeleniumTestCase')) {
                PHPUnit_Extensions_SeleniumTestCase::$browsers = $browsers;
            }

            if (!isset($this->arguments['test'])) {
                $testSuite = $configuration->getTestSuiteConfiguration(isset($this->arguments['testsuite']) ? $this->arguments['testsuite'] : null);

                if ($testSuite !== null) {
                    $this->arguments['test'] = $testSuite;
                }
            }
        } elseif (isset($this->arguments['bootstrap'])) {
            $this->handleBootstrap($this->arguments['bootstrap']);
        }

        if (isset($this->arguments['printer']) &&
            is_string($this->arguments['printer'])) {
            $this->arguments['printer'] = $this->handlePrinter($this->arguments['printer']);
        }

        if (isset($this->arguments['test']) && is_string($this->arguments['test']) && substr($this->arguments['test'], -5, 5) == '.phpt') {
            $test = new PHPUnit_Extensions_PhptTestCase($this->arguments['test']);

            $this->arguments['test'] = new PHPUnit_Framework_TestSuite;
            $this->arguments['test']->addTest($test);
        }

        if (!isset($this->arguments['test']) ||
            (isset($this->arguments['testDatabaseLogRevision']) && !isset($this->arguments['testDatabaseDSN']))) {
            $this->showHelp();
            exit(PHPUnit_TextUI_TestRunner::EXCEPTION_EXIT);
        }
    }

    /**
     * Handles the loading of the PHPUnit_Runner_TestSuiteLoader implementation.
     *
     * @param  string                         $loaderClass
     * @param  string                         $loaderFile
     * @return PHPUnit_Runner_TestSuiteLoader
     */
    protected function handleLoader($loaderClass, $loaderFile = '')
    {
        if (!class_exists($loaderClass, false)) {
            if ($loaderFile == '') {
                $loaderFile = PHPUnit_Util_Filesystem::classNameToFilename(
                    $loaderClass
                );
            }

            $loaderFile = stream_resolve_include_path($loaderFile);

            if ($loaderFile) {
                require $loaderFile;
            }
        }

        if (class_exists($loaderClass, false)) {
            $class = new ReflectionClass($loaderClass);

            if ($class->implementsInterface('PHPUnit_Runner_TestSuiteLoader') &&
                $class->isInstantiable()) {
                return $class->newInstance();
            }
        }

        if ($loaderClass == 'PHPUnit_Runner_StandardTestSuiteLoader') {
            return;
        }

        $this->showError(
            sprintf(
                'Could not use "%s" as loader.',
                $loaderClass
            )
        );
    }

    /**
     * Handles the loading of the PHPUnit_Util_Printer implementation.
     *
     * @param  string               $printerClass
     * @param  string               $printerFile
     * @return PHPUnit_Util_Printer
     */
    protected function handlePrinter($printerClass, $printerFile = '')
    {
        if (!class_exists($printerClass, false)) {
            if ($printerFile == '') {
                $printerFile = PHPUnit_Util_Filesystem::classNameToFilename(
                    $printerClass
                );
            }

            $printerFile = stream_resolve_include_path($printerFile);

            if ($printerFile) {
                require $printerFile;
            }
        }

        if (class_exists($printerClass)) {
            $class = new ReflectionClass($printerClass);

            if ($class->implementsInterface('PHPUnit_Framework_TestListener') &&
                $class->isSubclassOf('PHPUnit_Util_Printer') &&
                $class->isInstantiable()) {
                if ($class->isSubclassOf('PHPUnit_TextUI_ResultPrinter')) {
                    return $printerClass;
                }

                $outputStream = isset($this->arguments['stderr']) ? 'php://stderr' : null;

                return $class->newInstance($outputStream);
            }
        }

        $this->showError(
            sprintf(
                'Could not use "%s" as printer.',
                $printerClass
            )
        );
    }

    /**
     * Loads a bootstrap file.
     *
     * @param string $filename
     */
    protected function handleBootstrap($filename)
    {
        try {
            PHPUnit_Util_Fileloader::checkAndLoad($filename);
        } catch (PHPUnit_Framework_Exception $e) {
            $this->showError($e->getMessage());
        }
    }

    /**
     * @since Method available since Release 4.0.0
     */
    protected function handleSelfUpdate()
    {
        $this->printVersionString();

        if (!extension_loaded('openssl')) {
            print "The OpenSSL extension is not loaded.\n";
            exit(PHPUnit_TextUI_TestRunner::EXCEPTION_EXIT);
        }

        $remoteFilename = sprintf(
            'https://phar.phpunit.de/phpunit%s.phar',
            PHPUnit_Runner_Version::getReleaseChannel()
        );

        $localFilename = realpath($_SERVER['argv'][0]);
        $tempFilename  = basename($localFilename, '.phar') . '-temp.phar';

        // Workaround for https://bugs.php.net/bug.php?id=65538
        $caFile = dirname($tempFilename) . '/ca.pem';
        copy(__PHPUNIT_PHAR_ROOT__ . '/ca.pem', $caFile);

        print 'Updating the PHPUnit PHAR ... ';

        $options = array(
            'ssl' => array(
                'allow_self_signed' => false,
                'cafile' => $caFile,
                'verify_peer' => true
            )
        );

        if (PHP_VERSION_ID < 50600) {
            $options['ssl']['CN_match']        = 'phar.phpunit.de';
            $options['ssl']['SNI_server_name'] = 'phar.phpunit.de';
        }

        file_put_contents(
            $tempFilename,
            file_get_contents(
                $remoteFilename,
                false,
                stream_context_create($options)
            )
        );

        chmod($tempFilename, 0777 & ~umask());

        try {
            $phar = new Phar($tempFilename);
            unset($phar);
            rename($tempFilename, $localFilename);
            unlink($caFile);
        } catch (Exception $e) {
            unlink($caFile);
            unlink($tempFilename);
            print " done\n\n" . $e->getMessage() . "\n";
            exit(2);
        }

        print " done\n";
        exit(0);
    }

    /**
     * Show the help message.
     */
    protected function showHelp()
    {
        $this->printVersionString();

        print <<<EOT
Usage: phpunit [options] UnitTest [UnitTest.php]
       phpunit [options] <directory>

Code Coverage Options:

  --coverage-clover <file>  Generate code coverage report in Clover XML format.
  --coverage-crap4j <file>  Generate code coverage report in Crap4J XML format.
  --coverage-html <dir>     Generate code coverage report in HTML format.
  --coverage-php <file>     Export PHP_CodeCoverage object to file.
  --coverage-text=<file>    Generate code coverage report in text format.
                            Default: Standard output.
  --coverage-xml <dir>      Generate code coverage report in PHPUnit XML format.

Logging Options:

  --log-junit <file>        Log test execution in JUnit XML format to file.
  --log-tap <file>          Log test execution in TAP format to file.
  --log-json <file>         Log test execution in JSON format.
  --testdox-html <file>     Write agile documentation in HTML format to file.
  --testdox-text <file>     Write agile documentation in Text format to file.

Test Selection Options:

  --filter <pattern>        Filter which tests to run.
  --testsuite <pattern>     Filter which testsuite to run.
  --group ...               Only runs tests from the specified group(s).
  --exclude-group ...       Exclude tests from the specified group(s).
  --list-groups             List available test groups.
  --test-suffix ...         Only search for test in files with specified
                            suffix(es). Default: Test.php,.phpt

Test Execution Options:

  --report-useless-tests    Be strict about tests that do not test anything.
  --strict-coverage         Be strict about unintentionally covered code.
  --strict-global-state     Be strict about changes to global state
  --disallow-test-output    Be strict about output during tests.
  --enforce-time-limit      Enforce time limit based on test size.
  --disallow-todo-tests     Disallow @todo-annotated tests.

  --process-isolation       Run each test in a separate PHP process.
  --no-globals-backup       Do not backup and restore \$GLOBALS for each test.
  --static-backup           Backup and restore static attributes for each test.

  --colors=<flag>           Use colors in output ("never", "auto" or "always").
  --columns <n>             Number of columns to use for progress output.
  --columns max             Use maximum number of columns for progress output.
  --stderr                  Write to STDERR instead of STDOUT.
  --stop-on-error           Stop execution upon first error.
  --stop-on-failure         Stop execution upon first error or failure.
  --stop-on-risky           Stop execution upon first risky test.
  --stop-on-skipped         Stop execution upon first skipped test.
  --stop-on-incomplete      Stop execution upon first incomplete test.
  -v|--verbose              Output more verbose information.
  --debug                   Display debugging information during test execution.

  --loader <loader>         TestSuiteLoader implementation to use.
  --repeat <times>          Runs the test(s) repeatedly.
  --tap                     Report test execution progress in TAP format.
  --testdox                 Report test execution progress in TestDox format.
  --printer <printer>       TestListener implementation to use.

Configuration Options:

  --bootstrap <file>        A "bootstrap" PHP file that is run before the tests.
  -c|--configuration <file> Read configuration from XML file.
  --no-configuration        Ignore default configuration file (phpunit.xml).
  --include-path <path(s)>  Prepend PHP's include_path with given path(s).
  -d key[=value]            Sets a php.ini value.

Miscellaneous Options:

  -h|--help                 Prints this usage information.
  --version                 Prints the version and exits.

EOT;

        if (defined('__PHPUNIT_PHAR__')) {
            print "\n  --self-update             Update PHPUnit to the latest version.\n";
        }
    }

    /**
     * Custom callback for test suite discovery.
     */
    protected function handleCustomTestSuite()
    {
    }

    private function printVersionString()
    {
        if ($this->versionStringPrinted) {
            return;
        }

        print PHPUnit_Runner_Version::getVersionString() . "\n\n";

        $this->versionStringPrinted = true;
    }

    /**
     */
    private function showError($message)
    {
        $this->printVersionString();

        print $message . "\n";

        exit(PHPUnit_TextUI_TestRunner::FAILURE_EXIT);
    }
}
