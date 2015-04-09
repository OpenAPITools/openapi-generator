<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

use SebastianBergmann\Environment\Runtime;

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
 * @since      Class available since Release 2.0.0
 */
class PHPUnit_TextUI_TestRunner extends PHPUnit_Runner_BaseTestRunner
{
    const SUCCESS_EXIT   = 0;
    const FAILURE_EXIT   = 1;
    const EXCEPTION_EXIT = 2;

    /**
     * @var PHP_CodeCoverage_Filter
     */
    protected $codeCoverageFilter;

    /**
     * @var PHPUnit_Runner_TestSuiteLoader
     */
    protected $loader = null;

    /**
     * @var PHPUnit_TextUI_ResultPrinter
     */
    protected $printer = null;

    /**
     * @var boolean
     */
    protected static $versionStringPrinted = false;

    /**
     * @var array
     */
    private $missingExtensions = array();

    /**
     * @var boolean
     */
    private $canCollectCodeCoverage;

    /**
     * @param PHPUnit_Runner_TestSuiteLoader $loader
     * @param PHP_CodeCoverage_Filter        $filter
     * @since Method available since Release 3.4.0
     */
    public function __construct(PHPUnit_Runner_TestSuiteLoader $loader = null, PHP_CodeCoverage_Filter $filter = null)
    {
        if ($filter === null) {
            $filter = new PHP_CodeCoverage_Filter;
        }

        $this->codeCoverageFilter = $filter;
        $this->loader             = $loader;

        $runtime = new Runtime;
        $this->canCollectCodeCoverage = $runtime->canCollectCodeCoverage();
    }

    /**
     * @param  PHPUnit_Framework_Test|ReflectionClass $test
     * @param  array                               $arguments
     * @return PHPUnit_Framework_TestResult
     * @throws PHPUnit_Framework_Exception
     */
    public static function run($test, array $arguments = array())
    {
        if ($test instanceof ReflectionClass) {
            $test = new PHPUnit_Framework_TestSuite($test);
        }

        if ($test instanceof PHPUnit_Framework_Test) {
            $aTestRunner = new PHPUnit_TextUI_TestRunner;

            return $aTestRunner->doRun(
                $test,
                $arguments
            );
        } else {
            throw new PHPUnit_Framework_Exception(
                'No test case or test suite found.'
            );
        }
    }

    /**
     * @return PHPUnit_Framework_TestResult
     */
    protected function createTestResult()
    {
        return new PHPUnit_Framework_TestResult;
    }

    private function processSuiteFilters(PHPUnit_Framework_TestSuite $suite, array $arguments)
    {
        if (!$arguments['filter'] &&
            empty($arguments['groups']) &&
            empty($arguments['excludeGroups'])) {
            return;
        }

        $filterFactory = new PHPUnit_Runner_Filter_Factory();

        if (!empty($arguments['excludeGroups'])) {
            $filterFactory->addFilter(
                new ReflectionClass('PHPUnit_Runner_Filter_Group_Exclude'),
                $arguments['excludeGroups']
            );
        }

        if (!empty($arguments['groups'])) {
            $filterFactory->addFilter(
                new ReflectionClass('PHPUnit_Runner_Filter_Group_Include'),
                $arguments['groups']
            );
        }

        if ($arguments['filter']) {
            $filterFactory->addFilter(
                new ReflectionClass('PHPUnit_Runner_Filter_Test'),
                $arguments['filter']
            );
        }
        $suite->injectFilter($filterFactory);
    }

    /**
     * @param  PHPUnit_Framework_Test       $suite
     * @param  array                        $arguments
     * @return PHPUnit_Framework_TestResult
     */
    public function doRun(PHPUnit_Framework_Test $suite, array $arguments = array())
    {
        $this->handleConfiguration($arguments);

        $this->processSuiteFilters($suite, $arguments);

        if (isset($arguments['bootstrap'])) {
            $GLOBALS['__PHPUNIT_BOOTSTRAP'] = $arguments['bootstrap'];
        }

        if ($arguments['backupGlobals'] === false) {
            $suite->setBackupGlobals(false);
        }

        if ($arguments['backupStaticAttributes'] === true) {
            $suite->setBackupStaticAttributes(true);
        }

        if ($arguments['disallowChangesToGlobalState'] === true) {
            $suite->setDisallowChangesToGlobalState(true);
        }

        if (is_integer($arguments['repeat'])) {
            $test = new PHPUnit_Extensions_RepeatedTest(
                $suite,
                $arguments['repeat'],
                $arguments['processIsolation']
            );

            $suite = new PHPUnit_Framework_TestSuite();
            $suite->addTest($test);
        }

        $result = $this->createTestResult();

        if (!$arguments['convertErrorsToExceptions']) {
            $result->convertErrorsToExceptions(false);
        }

        if (!$arguments['convertNoticesToExceptions']) {
            PHPUnit_Framework_Error_Notice::$enabled = false;
        }

        if (!$arguments['convertWarningsToExceptions']) {
            PHPUnit_Framework_Error_Warning::$enabled = false;
        }

        if ($arguments['stopOnError']) {
            $result->stopOnError(true);
        }

        if ($arguments['stopOnFailure']) {
            $result->stopOnFailure(true);
        }

        if ($arguments['stopOnIncomplete']) {
            $result->stopOnIncomplete(true);
        }

        if ($arguments['stopOnRisky']) {
            $result->stopOnRisky(true);
        }

        if ($arguments['stopOnSkipped']) {
            $result->stopOnSkipped(true);
        }

        if ($this->printer === null) {
            if (isset($arguments['printer']) &&
                $arguments['printer'] instanceof PHPUnit_Util_Printer) {
                $this->printer = $arguments['printer'];
            } else {
                $printerClass = 'PHPUnit_TextUI_ResultPrinter';

                if (isset($arguments['printer']) &&
                    is_string($arguments['printer']) &&
                    class_exists($arguments['printer'], false)) {
                    $class = new ReflectionClass($arguments['printer']);

                    if ($class->isSubclassOf('PHPUnit_TextUI_ResultPrinter')) {
                        $printerClass = $arguments['printer'];
                    }
                }

                $this->printer = new $printerClass(
                  isset($arguments['stderr']) ? 'php://stderr' : null,
                  $arguments['verbose'],
                  $arguments['colors'],
                  $arguments['debug'],
                  $arguments['columns']
                );
            }
        }

        if (!$this->printer instanceof PHPUnit_Util_Log_TAP) {
            $this->printer->write(
                PHPUnit_Runner_Version::getVersionString() . "\n\n"
            );

            self::$versionStringPrinted = true;

            if (isset($arguments['configuration'])) {
                $this->printer->write(
                    sprintf(
                        "Configuration read from %s\n\n",
                        $arguments['configuration']->getFilename()
                    )
                );
            }

            if (isset($arguments['deprecatedStrictModeOption'])) {
                print "Deprecated option \"--strict\" used\n\n";
            }

            if (isset($arguments['deprecatedStrictModeSetting'])) {
                print "Deprecated configuration setting \"strict\" used\n\n";
            }
        }

        foreach ($arguments['listeners'] as $listener) {
            $result->addListener($listener);
        }

        $result->addListener($this->printer);

        if (isset($arguments['testdoxHTMLFile'])) {
            $result->addListener(
                new PHPUnit_Util_TestDox_ResultPrinter_HTML(
                    $arguments['testdoxHTMLFile']
                )
            );
        }

        if (isset($arguments['testdoxTextFile'])) {
            $result->addListener(
                new PHPUnit_Util_TestDox_ResultPrinter_Text(
                    $arguments['testdoxTextFile']
                )
            );
        }

        $codeCoverageReports = 0;

        if (isset($arguments['coverageClover'])) {
            $codeCoverageReports++;
        }

        if (isset($arguments['coverageCrap4J'])) {
            $codeCoverageReports++;
        }

        if (isset($arguments['coverageHtml'])) {
            $codeCoverageReports++;
        }

        if (isset($arguments['coveragePHP'])) {
            $codeCoverageReports++;
        }

        if (isset($arguments['coverageText'])) {
            $codeCoverageReports++;
        }

        if (isset($arguments['coverageXml'])) {
            $codeCoverageReports++;
        }

        if ($codeCoverageReports > 0 && (!extension_loaded('tokenizer') || !$this->canCollectCodeCoverage)) {
            if (!extension_loaded('tokenizer')) {
                $this->showExtensionNotLoadedMessage(
                    'tokenizer',
                    'No code coverage will be generated.'
                );
            } elseif (!extension_loaded('Xdebug')) {
                $this->showExtensionNotLoadedMessage(
                    'Xdebug',
                    'No code coverage will be generated.'
                );
            }

            $codeCoverageReports = 0;
        }

        if ($codeCoverageReports > 0) {
            $codeCoverage = new PHP_CodeCoverage(
                null,
                $this->codeCoverageFilter
            );

            $codeCoverage->setAddUncoveredFilesFromWhitelist(
                $arguments['addUncoveredFilesFromWhitelist']
            );

            $codeCoverage->setCheckForUnintentionallyCoveredCode(
                $arguments['strictCoverage']
            );

            $codeCoverage->setProcessUncoveredFilesFromWhitelist(
                $arguments['processUncoveredFilesFromWhitelist']
            );

            if (isset($arguments['forceCoversAnnotation'])) {
                $codeCoverage->setForceCoversAnnotation(
                    $arguments['forceCoversAnnotation']
                );
            }

            if (isset($arguments['mapTestClassNameToCoveredClassName'])) {
                $codeCoverage->setMapTestClassNameToCoveredClassName(
                    $arguments['mapTestClassNameToCoveredClassName']
                );
            }

            $result->setCodeCoverage($codeCoverage);
        }

        if ($codeCoverageReports > 1) {
            if (isset($arguments['cacheTokens'])) {
                $codeCoverage->setCacheTokens($arguments['cacheTokens']);
            }
        }

        if (isset($arguments['jsonLogfile'])) {
            $result->addListener(
                new PHPUnit_Util_Log_JSON($arguments['jsonLogfile'])
            );
        }

        if (isset($arguments['tapLogfile'])) {
            $result->addListener(
                new PHPUnit_Util_Log_TAP($arguments['tapLogfile'])
            );
        }

        if (isset($arguments['junitLogfile'])) {
            $result->addListener(
                new PHPUnit_Util_Log_JUnit(
                    $arguments['junitLogfile'],
                    $arguments['logIncompleteSkipped']
                )
            );
        }

        $result->beStrictAboutTestsThatDoNotTestAnything($arguments['reportUselessTests']);
        $result->beStrictAboutOutputDuringTests($arguments['disallowTestOutput']);
        $result->beStrictAboutTodoAnnotatedTests($arguments['disallowTodoAnnotatedTests']);
        $result->beStrictAboutTestSize($arguments['enforceTimeLimit']);
        $result->setTimeoutForSmallTests($arguments['timeoutForSmallTests']);
        $result->setTimeoutForMediumTests($arguments['timeoutForMediumTests']);
        $result->setTimeoutForLargeTests($arguments['timeoutForLargeTests']);

        if ($suite instanceof PHPUnit_Framework_TestSuite) {
            $suite->setRunTestInSeparateProcess($arguments['processIsolation']);
        }

        $suite->run($result);

        unset($suite);
        $result->flushListeners();

        if ($this->printer instanceof PHPUnit_TextUI_ResultPrinter) {
            $this->printer->printResult($result);
        }

        if (isset($codeCoverage)) {
            if (isset($arguments['coverageClover'])) {
                $this->printer->write(
                    "\nGenerating code coverage report in Clover XML format ..."
                );

                $writer = new PHP_CodeCoverage_Report_Clover;
                $writer->process($codeCoverage, $arguments['coverageClover']);

                $this->printer->write(" done\n");
                unset($writer);
            }

            if (isset($arguments['coverageCrap4J'])) {
                $this->printer->write(
                    "\nGenerating Crap4J report XML file ..."
                );

                $writer = new PHP_CodeCoverage_Report_Crap4j;
                $writer->process($codeCoverage, $arguments['coverageCrap4J']);

                $this->printer->write(" done\n");
                unset($writer);
            }

            if (isset($arguments['coverageHtml'])) {
                $this->printer->write(
                    "\nGenerating code coverage report in HTML format ..."
                );

                $writer = new PHP_CodeCoverage_Report_HTML(
                    $arguments['reportLowUpperBound'],
                    $arguments['reportHighLowerBound'],
                    sprintf(
                        ' and <a href="http://phpunit.de/">PHPUnit %s</a>',
                        PHPUnit_Runner_Version::id()
                    )
                );

                $writer->process($codeCoverage, $arguments['coverageHtml']);

                $this->printer->write(" done\n");
                unset($writer);
            }

            if (isset($arguments['coveragePHP'])) {
                $this->printer->write(
                    "\nGenerating code coverage report in PHP format ..."
                );

                $writer = new PHP_CodeCoverage_Report_PHP;
                $writer->process($codeCoverage, $arguments['coveragePHP']);

                $this->printer->write(" done\n");
                unset($writer);
            }

            if (isset($arguments['coverageText'])) {
                if ($arguments['coverageText'] == 'php://stdout') {
                    $outputStream = $this->printer;
                    $colors       = $arguments['colors'];
                } else {
                    $outputStream = new PHPUnit_Util_Printer($arguments['coverageText']);
                    $colors       = false;
                }

                $processor = new PHP_CodeCoverage_Report_Text(
                    $arguments['reportLowUpperBound'],
                    $arguments['reportHighLowerBound'],
                    $arguments['coverageTextShowUncoveredFiles'],
                    $arguments['coverageTextShowOnlySummary']
                );

                $outputStream->write(
                    $processor->process($codeCoverage, $colors)
                );
            }

            if (isset($arguments['coverageXml'])) {
                $this->printer->write(
                    "\nGenerating code coverage report in PHPUnit XML format ..."
                );

                $writer = new PHP_CodeCoverage_Report_XML;
                $writer->process($codeCoverage, $arguments['coverageXml']);

                $this->printer->write(" done\n");
                unset($writer);
            }
        }

        return $result;
    }

    /**
     * @param PHPUnit_TextUI_ResultPrinter $resultPrinter
     */
    public function setPrinter(PHPUnit_TextUI_ResultPrinter $resultPrinter)
    {
        $this->printer = $resultPrinter;
    }

    /**
     * Override to define how to handle a failed loading of
     * a test suite.
     *
     * @param string $message
     */
    protected function runFailed($message)
    {
        $this->write($message . PHP_EOL);
        exit(self::FAILURE_EXIT);
    }

    /**
     * @param string $buffer
     * @since  Method available since Release 3.1.0
     */
    protected function write($buffer)
    {
        if (PHP_SAPI != 'cli') {
            $buffer = htmlspecialchars($buffer);
        }

        if ($this->printer !== null) {
            $this->printer->write($buffer);
        } else {
            print $buffer;
        }
    }

    /**
     * Returns the loader to be used.
     *
     * @return PHPUnit_Runner_TestSuiteLoader
     * @since  Method available since Release 2.2.0
     */
    public function getLoader()
    {
        if ($this->loader === null) {
            $this->loader = new PHPUnit_Runner_StandardTestSuiteLoader;
        }

        return $this->loader;
    }

    /**
     * @param array $arguments
     * @since  Method available since Release 3.2.1
     */
    protected function handleConfiguration(array &$arguments)
    {
        if (isset($arguments['configuration']) &&
            !$arguments['configuration'] instanceof PHPUnit_Util_Configuration) {
            $arguments['configuration'] = PHPUnit_Util_Configuration::getInstance(
                $arguments['configuration']
            );
        }

        $arguments['debug']     = isset($arguments['debug'])     ? $arguments['debug']     : false;
        $arguments['filter']    = isset($arguments['filter'])    ? $arguments['filter']    : false;
        $arguments['listeners'] = isset($arguments['listeners']) ? $arguments['listeners'] : array();

        if (isset($arguments['configuration'])) {
            $arguments['configuration']->handlePHPConfiguration();

            $phpunitConfiguration = $arguments['configuration']->getPHPUnitConfiguration();

            if (isset($phpunitConfiguration['deprecatedStrictModeSetting'])) {
                $arguments['deprecatedStrictModeSetting'] = true;
            }

            if (isset($phpunitConfiguration['backupGlobals']) &&
                !isset($arguments['backupGlobals'])) {
                $arguments['backupGlobals'] = $phpunitConfiguration['backupGlobals'];
            }

            if (isset($phpunitConfiguration['backupStaticAttributes']) &&
                !isset($arguments['backupStaticAttributes'])) {
                $arguments['backupStaticAttributes'] = $phpunitConfiguration['backupStaticAttributes'];
            }

            if (isset($phpunitConfiguration['disallowChangesToGlobalState']) &&
                !isset($arguments['disallowChangesToGlobalState'])) {
                $arguments['disallowChangesToGlobalState'] = $phpunitConfiguration['disallowChangesToGlobalState'];
            }

            if (isset($phpunitConfiguration['bootstrap']) &&
                !isset($arguments['bootstrap'])) {
                $arguments['bootstrap'] = $phpunitConfiguration['bootstrap'];
            }

            if (isset($phpunitConfiguration['cacheTokens']) &&
                !isset($arguments['cacheTokens'])) {
                $arguments['cacheTokens'] = $phpunitConfiguration['cacheTokens'];
            }

            if (isset($phpunitConfiguration['colors']) &&
                !isset($arguments['colors'])) {
                $arguments['colors'] = $phpunitConfiguration['colors'];
            }

            if (isset($phpunitConfiguration['convertErrorsToExceptions']) &&
                !isset($arguments['convertErrorsToExceptions'])) {
                $arguments['convertErrorsToExceptions'] = $phpunitConfiguration['convertErrorsToExceptions'];
            }

            if (isset($phpunitConfiguration['convertNoticesToExceptions']) &&
                !isset($arguments['convertNoticesToExceptions'])) {
                $arguments['convertNoticesToExceptions'] = $phpunitConfiguration['convertNoticesToExceptions'];
            }

            if (isset($phpunitConfiguration['convertWarningsToExceptions']) &&
                !isset($arguments['convertWarningsToExceptions'])) {
                $arguments['convertWarningsToExceptions'] = $phpunitConfiguration['convertWarningsToExceptions'];
            }

            if (isset($phpunitConfiguration['processIsolation']) &&
                !isset($arguments['processIsolation'])) {
                $arguments['processIsolation'] = $phpunitConfiguration['processIsolation'];
            }

            if (isset($phpunitConfiguration['stopOnFailure']) &&
                !isset($arguments['stopOnFailure'])) {
                $arguments['stopOnFailure'] = $phpunitConfiguration['stopOnFailure'];
            }

            if (isset($phpunitConfiguration['timeoutForSmallTests']) &&
                !isset($arguments['timeoutForSmallTests'])) {
                $arguments['timeoutForSmallTests'] = $phpunitConfiguration['timeoutForSmallTests'];
            }

            if (isset($phpunitConfiguration['timeoutForMediumTests']) &&
                !isset($arguments['timeoutForMediumTests'])) {
                $arguments['timeoutForMediumTests'] = $phpunitConfiguration['timeoutForMediumTests'];
            }

            if (isset($phpunitConfiguration['timeoutForLargeTests']) &&
                !isset($arguments['timeoutForLargeTests'])) {
                $arguments['timeoutForLargeTests'] = $phpunitConfiguration['timeoutForLargeTests'];
            }

            if (isset($phpunitConfiguration['reportUselessTests']) &&
                !isset($arguments['reportUselessTests'])) {
                $arguments['reportUselessTests'] = $phpunitConfiguration['reportUselessTests'];
            }

            if (isset($phpunitConfiguration['strictCoverage']) &&
                !isset($arguments['strictCoverage'])) {
                $arguments['strictCoverage'] = $phpunitConfiguration['strictCoverage'];
            }

            if (isset($phpunitConfiguration['disallowTestOutput']) &&
                !isset($arguments['disallowTestOutput'])) {
                $arguments['disallowTestOutput'] = $phpunitConfiguration['disallowTestOutput'];
            }

            if (isset($phpunitConfiguration['enforceTimeLimit']) &&
                !isset($arguments['enforceTimeLimit'])) {
                $arguments['enforceTimeLimit'] = $phpunitConfiguration['enforceTimeLimit'];
            }

            if (isset($phpunitConfiguration['disallowTodoAnnotatedTests']) &&
                !isset($arguments['disallowTodoAnnotatedTests'])) {
                $arguments['disallowTodoAnnotatedTests'] = $phpunitConfiguration['disallowTodoAnnotatedTests'];
            }

            if (isset($phpunitConfiguration['verbose']) &&
                !isset($arguments['verbose'])) {
                $arguments['verbose'] = $phpunitConfiguration['verbose'];
            }

            if (isset($phpunitConfiguration['forceCoversAnnotation']) &&
                !isset($arguments['forceCoversAnnotation'])) {
                $arguments['forceCoversAnnotation'] = $phpunitConfiguration['forceCoversAnnotation'];
            }

            if (isset($phpunitConfiguration['mapTestClassNameToCoveredClassName']) &&
                !isset($arguments['mapTestClassNameToCoveredClassName'])) {
                $arguments['mapTestClassNameToCoveredClassName'] = $phpunitConfiguration['mapTestClassNameToCoveredClassName'];
            }

            $groupCliArgs = array();
            if (!empty($arguments['groups'])) {
                $groupCliArgs = $arguments['groups'];
            }

            $groupConfiguration = $arguments['configuration']->getGroupConfiguration();

            if (!empty($groupConfiguration['include']) &&
                !isset($arguments['groups'])) {
                $arguments['groups'] = $groupConfiguration['include'];
            }

            if (!empty($groupConfiguration['exclude']) &&
                !isset($arguments['excludeGroups'])) {
                $arguments['excludeGroups'] = array_diff($groupConfiguration['exclude'], $groupCliArgs);
            }

            foreach ($arguments['configuration']->getListenerConfiguration() as $listener) {
                if (!class_exists($listener['class'], false) &&
                    $listener['file'] !== '') {
                    require_once $listener['file'];
                }

                if (class_exists($listener['class'])) {
                    if (count($listener['arguments']) == 0) {
                        $listener = new $listener['class'];
                    } else {
                        $listenerClass = new ReflectionClass(
                            $listener['class']
                        );
                        $listener      = $listenerClass->newInstanceArgs(
                            $listener['arguments']
                        );
                    }

                    if ($listener instanceof PHPUnit_Framework_TestListener) {
                        $arguments['listeners'][] = $listener;
                    }
                }
            }

            $loggingConfiguration = $arguments['configuration']->getLoggingConfiguration();

            if (isset($loggingConfiguration['coverage-clover']) &&
                !isset($arguments['coverageClover'])) {
                $arguments['coverageClover'] = $loggingConfiguration['coverage-clover'];
            }

            if (isset($loggingConfiguration['coverage-crap4j']) &&
                !isset($arguments['coverageCrap4J'])) {
                $arguments['coverageCrap4J'] = $loggingConfiguration['coverage-crap4j'];
            }

            if (isset($loggingConfiguration['coverage-html']) &&
                !isset($arguments['coverageHtml'])) {
                if (isset($loggingConfiguration['lowUpperBound']) &&
                    !isset($arguments['reportLowUpperBound'])) {
                    $arguments['reportLowUpperBound'] = $loggingConfiguration['lowUpperBound'];
                }

                if (isset($loggingConfiguration['highLowerBound']) &&
                    !isset($arguments['reportHighLowerBound'])) {
                    $arguments['reportHighLowerBound'] = $loggingConfiguration['highLowerBound'];
                }

                $arguments['coverageHtml'] = $loggingConfiguration['coverage-html'];
            }

            if (isset($loggingConfiguration['coverage-php']) &&
                !isset($arguments['coveragePHP'])) {
                $arguments['coveragePHP'] = $loggingConfiguration['coverage-php'];
            }

            if (isset($loggingConfiguration['coverage-text']) &&
                !isset($arguments['coverageText'])) {
                $arguments['coverageText'] = $loggingConfiguration['coverage-text'];
                if (isset($loggingConfiguration['coverageTextShowUncoveredFiles'])) {
                    $arguments['coverageTextShowUncoveredFiles'] = $loggingConfiguration['coverageTextShowUncoveredFiles'];
                } else {
                    $arguments['coverageTextShowUncoveredFiles'] = false;
                }
                if (isset($loggingConfiguration['coverageTextShowOnlySummary'])) {
                    $arguments['coverageTextShowOnlySummary'] = $loggingConfiguration['coverageTextShowOnlySummary'];
                } else {
                    $arguments['coverageTextShowOnlySummary'] = false;
                }
            }

            if (isset($loggingConfiguration['coverage-xml']) &&
                !isset($arguments['coverageXml'])) {
                $arguments['coverageXml'] = $loggingConfiguration['coverage-xml'];
            }

            if (isset($loggingConfiguration['json']) &&
                !isset($arguments['jsonLogfile'])) {
                $arguments['jsonLogfile'] = $loggingConfiguration['json'];
            }

            if (isset($loggingConfiguration['plain'])) {
                $arguments['listeners'][] = new PHPUnit_TextUI_ResultPrinter(
                    $loggingConfiguration['plain'],
                    true
                );
            }

            if (isset($loggingConfiguration['tap']) &&
                !isset($arguments['tapLogfile'])) {
                $arguments['tapLogfile'] = $loggingConfiguration['tap'];
            }

            if (isset($loggingConfiguration['junit']) &&
                !isset($arguments['junitLogfile'])) {
                $arguments['junitLogfile'] = $loggingConfiguration['junit'];

                if (isset($loggingConfiguration['logIncompleteSkipped']) &&
                    !isset($arguments['logIncompleteSkipped'])) {
                    $arguments['logIncompleteSkipped'] = $loggingConfiguration['logIncompleteSkipped'];
                }
            }

            if (isset($loggingConfiguration['testdox-html']) &&
                !isset($arguments['testdoxHTMLFile'])) {
                $arguments['testdoxHTMLFile'] = $loggingConfiguration['testdox-html'];
            }

            if (isset($loggingConfiguration['testdox-text']) &&
                !isset($arguments['testdoxTextFile'])) {
                $arguments['testdoxTextFile'] = $loggingConfiguration['testdox-text'];
            }

            if ((isset($arguments['coverageClover']) ||
                isset($arguments['coverageCrap4J']) ||
                isset($arguments['coverageHtml']) ||
                isset($arguments['coveragePHP']) ||
                isset($arguments['coverageText']) ||
                isset($arguments['coverageXml'])) &&
                $this->canCollectCodeCoverage) {
                $filterConfiguration = $arguments['configuration']->getFilterConfiguration();
                $arguments['addUncoveredFilesFromWhitelist'] = $filterConfiguration['whitelist']['addUncoveredFilesFromWhitelist'];
                $arguments['processUncoveredFilesFromWhitelist'] = $filterConfiguration['whitelist']['processUncoveredFilesFromWhitelist'];

                if (empty($filterConfiguration['whitelist']['include']['directory']) &&
                    empty($filterConfiguration['whitelist']['include']['file'])) {
                    if (defined('__PHPUNIT_PHAR__')) {
                        $this->codeCoverageFilter->addFileToBlacklist(__PHPUNIT_PHAR__);
                    }

                    $blacklist = new PHPUnit_Util_Blacklist;

                    foreach ($blacklist->getBlacklistedDirectories() as $directory) {
                        $this->codeCoverageFilter->addDirectoryToBlacklist($directory);
                    }

                    foreach ($filterConfiguration['blacklist']['include']['directory'] as $dir) {
                        $this->codeCoverageFilter->addDirectoryToBlacklist(
                            $dir['path'],
                            $dir['suffix'],
                            $dir['prefix'],
                            $dir['group']
                        );
                    }

                    foreach ($filterConfiguration['blacklist']['include']['file'] as $file) {
                        $this->codeCoverageFilter->addFileToBlacklist($file);
                    }

                    foreach ($filterConfiguration['blacklist']['exclude']['directory'] as $dir) {
                        $this->codeCoverageFilter->removeDirectoryFromBlacklist(
                            $dir['path'],
                            $dir['suffix'],
                            $dir['prefix'],
                            $dir['group']
                        );
                    }

                    foreach ($filterConfiguration['blacklist']['exclude']['file'] as $file) {
                        $this->codeCoverageFilter->removeFileFromBlacklist($file);
                    }
                }

                foreach ($filterConfiguration['whitelist']['include']['directory'] as $dir) {
                    $this->codeCoverageFilter->addDirectoryToWhitelist(
                        $dir['path'],
                        $dir['suffix'],
                        $dir['prefix']
                    );
                }

                foreach ($filterConfiguration['whitelist']['include']['file'] as $file) {
                    $this->codeCoverageFilter->addFileToWhitelist($file);
                }

                foreach ($filterConfiguration['whitelist']['exclude']['directory'] as $dir) {
                    $this->codeCoverageFilter->removeDirectoryFromWhitelist(
                        $dir['path'],
                        $dir['suffix'],
                        $dir['prefix']
                    );
                }

                foreach ($filterConfiguration['whitelist']['exclude']['file'] as $file) {
                    $this->codeCoverageFilter->removeFileFromWhitelist($file);
                }
            }
        }

        $arguments['addUncoveredFilesFromWhitelist']     = isset($arguments['addUncoveredFilesFromWhitelist'])     ? $arguments['addUncoveredFilesFromWhitelist']     : true;
        $arguments['processUncoveredFilesFromWhitelist'] = isset($arguments['processUncoveredFilesFromWhitelist']) ? $arguments['processUncoveredFilesFromWhitelist'] : false;
        $arguments['backupGlobals']                      = isset($arguments['backupGlobals'])                      ? $arguments['backupGlobals']                      : null;
        $arguments['backupStaticAttributes']             = isset($arguments['backupStaticAttributes'])             ? $arguments['backupStaticAttributes']             : null;
        $arguments['disallowChangesToGlobalState']       = isset($arguments['disallowChangesToGlobalState'])       ? $arguments['disallowChangesToGlobalState']       : null;
        $arguments['cacheTokens']                        = isset($arguments['cacheTokens'])                        ? $arguments['cacheTokens']                        : false;
        $arguments['columns']                            = isset($arguments['columns'])                            ? $arguments['columns']                            : 80;
        $arguments['colors']                             = isset($arguments['colors'])                             ? $arguments['colors']                             : PHPUnit_TextUI_ResultPrinter::COLOR_DEFAULT;
        $arguments['convertErrorsToExceptions']          = isset($arguments['convertErrorsToExceptions'])          ? $arguments['convertErrorsToExceptions']          : true;
        $arguments['convertNoticesToExceptions']         = isset($arguments['convertNoticesToExceptions'])         ? $arguments['convertNoticesToExceptions']         : true;
        $arguments['convertWarningsToExceptions']        = isset($arguments['convertWarningsToExceptions'])        ? $arguments['convertWarningsToExceptions']        : true;
        $arguments['excludeGroups']                      = isset($arguments['excludeGroups'])                      ? $arguments['excludeGroups']                      : array();
        $arguments['groups']                             = isset($arguments['groups'])                             ? $arguments['groups']                             : array();
        $arguments['logIncompleteSkipped']               = isset($arguments['logIncompleteSkipped'])               ? $arguments['logIncompleteSkipped']               : false;
        $arguments['processIsolation']                   = isset($arguments['processIsolation'])                   ? $arguments['processIsolation']                   : false;
        $arguments['repeat']                             = isset($arguments['repeat'])                             ? $arguments['repeat']                             : false;
        $arguments['reportHighLowerBound']               = isset($arguments['reportHighLowerBound'])               ? $arguments['reportHighLowerBound']               : 90;
        $arguments['reportLowUpperBound']                = isset($arguments['reportLowUpperBound'])                ? $arguments['reportLowUpperBound']                : 50;
        $arguments['stopOnError']                        = isset($arguments['stopOnError'])                        ? $arguments['stopOnError']                        : false;
        $arguments['stopOnFailure']                      = isset($arguments['stopOnFailure'])                      ? $arguments['stopOnFailure']                      : false;
        $arguments['stopOnIncomplete']                   = isset($arguments['stopOnIncomplete'])                   ? $arguments['stopOnIncomplete']                   : false;
        $arguments['stopOnRisky']                        = isset($arguments['stopOnRisky'])                        ? $arguments['stopOnRisky']                        : false;
        $arguments['stopOnSkipped']                      = isset($arguments['stopOnSkipped'])                      ? $arguments['stopOnSkipped']                      : false;
        $arguments['timeoutForSmallTests']               = isset($arguments['timeoutForSmallTests'])               ? $arguments['timeoutForSmallTests']               : 1;
        $arguments['timeoutForMediumTests']              = isset($arguments['timeoutForMediumTests'])              ? $arguments['timeoutForMediumTests']              : 10;
        $arguments['timeoutForLargeTests']               = isset($arguments['timeoutForLargeTests'])               ? $arguments['timeoutForLargeTests']               : 60;
        $arguments['reportUselessTests']                 = isset($arguments['reportUselessTests'])                 ? $arguments['reportUselessTests']                 : false;
        $arguments['strictCoverage']                     = isset($arguments['strictCoverage'])                     ? $arguments['strictCoverage']                     : false;
        $arguments['disallowTestOutput']                 = isset($arguments['disallowTestOutput'])                 ? $arguments['disallowTestOutput']                 : false;
        $arguments['enforceTimeLimit']                   = isset($arguments['enforceTimeLimit'])                   ? $arguments['enforceTimeLimit']                   : false;
        $arguments['disallowTodoAnnotatedTests']         = isset($arguments['disallowTodoAnnotatedTests'])         ? $arguments['disallowTodoAnnotatedTests']         : false;
        $arguments['verbose']                            = isset($arguments['verbose'])                            ? $arguments['verbose']                            : false;
    }

    /**
     * @param $extension
     * @param string $message
     * @since Method available since Release 4.0.0
     */
    private function showExtensionNotLoadedMessage($extension, $message = '')
    {
        if (isset($this->missingExtensions[$extension])) {
            return;
        }

        if (!empty($message)) {
            $message = ' ' . $message;
        }

        $this->showMessage(
            'The ' . $extension . ' extension is not loaded.' . $message . "\n"
        );

        $this->missingExtensions[$extension] = true;
    }

    /**
     * Shows a message.
     *
     * @param string  $message
     * @param boolean $exit
     * @since Method available since Release 4.0.0
     */
    private function showMessage($message, $exit = false)
    {
        $this->write($message . "\n");

        if ($exit) {
            exit(self::EXCEPTION_EXIT);
        }
    }
}
