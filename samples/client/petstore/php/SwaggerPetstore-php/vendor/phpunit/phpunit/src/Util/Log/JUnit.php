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
 * A TestListener that generates a logfile of the test execution in XML markup.
 *
 * The XML markup used is the same as the one that is used by the JUnit Ant task.
 *
 * @package    PHPUnit
 * @subpackage Util_Log
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 2.1.0
 */
class PHPUnit_Util_Log_JUnit extends PHPUnit_Util_Printer implements PHPUnit_Framework_TestListener
{
    /**
     * @var    DOMDocument
     */
    protected $document;

    /**
     * @var    DOMElement
     */
    protected $root;

    /**
     * @var    boolean
     */
    protected $logIncompleteSkipped = false;

    /**
     * @var    boolean
     */
    protected $writeDocument = true;

    /**
     * @var    DOMElement[]
     */
    protected $testSuites = array();

    /**
     * @var    integer[]
     */
    protected $testSuiteTests = array(0);

    /**
     * @var    integer[]
     */
    protected $testSuiteAssertions = array(0);

    /**
     * @var    integer[]
     */
    protected $testSuiteErrors = array(0);

    /**
     * @var    integer[]
     */
    protected $testSuiteFailures = array(0);

    /**
     * @var    integer[]
     */
    protected $testSuiteTimes = array(0);

    /**
     * @var    integer
     */
    protected $testSuiteLevel = 0;

    /**
     * @var    DOMElement
     */
    protected $currentTestCase = null;

    /**
     * @var    boolean
     */
    protected $attachCurrentTestCase = true;

    /**
     * Constructor.
     *
     * @param mixed   $out
     * @param boolean $logIncompleteSkipped
     */
    public function __construct($out = null, $logIncompleteSkipped = false)
    {
        $this->document = new DOMDocument('1.0', 'UTF-8');
        $this->document->formatOutput = true;

        $this->root = $this->document->createElement('testsuites');
        $this->document->appendChild($this->root);

        parent::__construct($out);

        $this->logIncompleteSkipped = $logIncompleteSkipped;
    }

    /**
     * Flush buffer and close output.
     *
     */
    public function flush()
    {
        if ($this->writeDocument === true) {
            $this->write($this->getXML());
        }

        parent::flush();
    }

    /**
     * An error occurred.
     *
     * @param PHPUnit_Framework_Test $test
     * @param Exception              $e
     * @param float                  $time
     */
    public function addError(PHPUnit_Framework_Test $test, Exception $e, $time)
    {
        if ($this->currentTestCase !== null) {
            if ($test instanceof PHPUnit_Framework_SelfDescribing) {
                $buffer = $test->toString() . "\n";
            } else {
                $buffer = '';
            }

            $buffer .= PHPUnit_Framework_TestFailure::exceptionToString($e) .
                       "\n" .
                       PHPUnit_Util_Filter::getFilteredStacktrace($e);

            $error = $this->document->createElement(
                'error',
                PHPUnit_Util_XML::prepareString($buffer)
            );

            $error->setAttribute('type', get_class($e));

            $this->currentTestCase->appendChild($error);

            $this->testSuiteErrors[$this->testSuiteLevel]++;
        }
    }

    /**
     * A failure occurred.
     *
     * @param PHPUnit_Framework_Test                 $test
     * @param PHPUnit_Framework_AssertionFailedError $e
     * @param float                                  $time
     */
    public function addFailure(PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e, $time)
    {
        if ($this->currentTestCase !== null) {
            if (!$test instanceof PHPUnit_Framework_Warning) {
                if ($test instanceof PHPUnit_Framework_SelfDescribing) {
                    $buffer = $test->toString() . "\n";
                } else {
                    $buffer = '';
                }

                $buffer .= PHPUnit_Framework_TestFailure::exceptionToString($e) .
                           "\n" .
                           PHPUnit_Util_Filter::getFilteredStacktrace($e);

                $failure = $this->document->createElement(
                    'failure',
                    PHPUnit_Util_XML::prepareString($buffer)
                );

                $failure->setAttribute('type', get_class($e));

                $this->currentTestCase->appendChild($failure);

                $this->testSuiteFailures[$this->testSuiteLevel]++;
            }
        }
    }

    /**
     * Incomplete test.
     *
     * @param PHPUnit_Framework_Test $test
     * @param Exception              $e
     * @param float                  $time
     */
    public function addIncompleteTest(PHPUnit_Framework_Test $test, Exception $e, $time)
    {
        if ($this->logIncompleteSkipped && $this->currentTestCase !== null) {
            $error = $this->document->createElement(
                'error',
                PHPUnit_Util_XML::prepareString(
                    "Incomplete Test\n" .
                    PHPUnit_Util_Filter::getFilteredStacktrace($e)
                )
            );

            $error->setAttribute('type', get_class($e));

            $this->currentTestCase->appendChild($error);

            $this->testSuiteErrors[$this->testSuiteLevel]++;
        } else {
            $this->attachCurrentTestCase = false;
        }
    }

    /**
     * Risky test.
     *
     * @param PHPUnit_Framework_Test $test
     * @param Exception              $e
     * @param float                  $time
     * @since  Method available since Release 4.0.0
     */
    public function addRiskyTest(PHPUnit_Framework_Test $test, Exception $e, $time)
    {
        if ($this->logIncompleteSkipped && $this->currentTestCase !== null) {
            $error = $this->document->createElement(
                'error',
                PHPUnit_Util_XML::prepareString(
                    "Risky Test\n" .
                    PHPUnit_Util_Filter::getFilteredStacktrace($e)
                )
            );

            $error->setAttribute('type', get_class($e));

            $this->currentTestCase->appendChild($error);

            $this->testSuiteErrors[$this->testSuiteLevel]++;
        } else {
            $this->attachCurrentTestCase = false;
        }
    }

    /**
     * Skipped test.
     *
     * @param PHPUnit_Framework_Test $test
     * @param Exception              $e
     * @param float                  $time
     * @since  Method available since Release 3.0.0
     */
    public function addSkippedTest(PHPUnit_Framework_Test $test, Exception $e, $time)
    {
        if ($this->logIncompleteSkipped && $this->currentTestCase !== null) {
            $error = $this->document->createElement(
                'error',
                PHPUnit_Util_XML::prepareString(
                    "Skipped Test\n" .
                    PHPUnit_Util_Filter::getFilteredStacktrace($e)
                )
            );

            $error->setAttribute('type', get_class($e));

            $this->currentTestCase->appendChild($error);

            $this->testSuiteErrors[$this->testSuiteLevel]++;
        } else {
            $this->attachCurrentTestCase = false;
        }
    }

    /**
     * A testsuite started.
     *
     * @param PHPUnit_Framework_TestSuite $suite
     * @since  Method available since Release 2.2.0
     */
    public function startTestSuite(PHPUnit_Framework_TestSuite $suite)
    {
        $testSuite = $this->document->createElement('testsuite');
        $testSuite->setAttribute('name', $suite->getName());

        if (class_exists($suite->getName(), false)) {
            try {
                $class = new ReflectionClass($suite->getName());

                $testSuite->setAttribute('file', $class->getFileName());
            } catch (ReflectionException $e) {
            }
        }

        if ($this->testSuiteLevel > 0) {
            $this->testSuites[$this->testSuiteLevel]->appendChild($testSuite);
        } else {
            $this->root->appendChild($testSuite);
        }

        $this->testSuiteLevel++;
        $this->testSuites[$this->testSuiteLevel]          = $testSuite;
        $this->testSuiteTests[$this->testSuiteLevel]      = 0;
        $this->testSuiteAssertions[$this->testSuiteLevel] = 0;
        $this->testSuiteErrors[$this->testSuiteLevel]     = 0;
        $this->testSuiteFailures[$this->testSuiteLevel]   = 0;
        $this->testSuiteTimes[$this->testSuiteLevel]      = 0;
    }

    /**
     * A testsuite ended.
     *
     * @param PHPUnit_Framework_TestSuite $suite
     * @since  Method available since Release 2.2.0
     */
    public function endTestSuite(PHPUnit_Framework_TestSuite $suite)
    {
        $this->testSuites[$this->testSuiteLevel]->setAttribute(
            'tests',
            $this->testSuiteTests[$this->testSuiteLevel]
        );

        $this->testSuites[$this->testSuiteLevel]->setAttribute(
            'assertions',
            $this->testSuiteAssertions[$this->testSuiteLevel]
        );

        $this->testSuites[$this->testSuiteLevel]->setAttribute(
            'failures',
            $this->testSuiteFailures[$this->testSuiteLevel]
        );

        $this->testSuites[$this->testSuiteLevel]->setAttribute(
            'errors',
            $this->testSuiteErrors[$this->testSuiteLevel]
        );

        $this->testSuites[$this->testSuiteLevel]->setAttribute(
            'time',
            sprintf('%F', $this->testSuiteTimes[$this->testSuiteLevel])
        );

        if ($this->testSuiteLevel > 1) {
            $this->testSuiteTests[$this->testSuiteLevel - 1]      += $this->testSuiteTests[$this->testSuiteLevel];
            $this->testSuiteAssertions[$this->testSuiteLevel - 1] += $this->testSuiteAssertions[$this->testSuiteLevel];
            $this->testSuiteErrors[$this->testSuiteLevel - 1]     += $this->testSuiteErrors[$this->testSuiteLevel];
            $this->testSuiteFailures[$this->testSuiteLevel - 1]   += $this->testSuiteFailures[$this->testSuiteLevel];
            $this->testSuiteTimes[$this->testSuiteLevel - 1]      += $this->testSuiteTimes[$this->testSuiteLevel];
        }

        $this->testSuiteLevel--;
    }

    /**
     * A test started.
     *
     * @param PHPUnit_Framework_Test $test
     */
    public function startTest(PHPUnit_Framework_Test $test)
    {
        if (!$test instanceof PHPUnit_Framework_Warning) {
            $testCase = $this->document->createElement('testcase');
            $testCase->setAttribute('name', $test->getName());

            if ($test instanceof PHPUnit_Framework_TestCase) {
                $class      = new ReflectionClass($test);
                $methodName = $test->getName();

                if ($class->hasMethod($methodName)) {
                    $method = $class->getMethod($test->getName());

                    $testCase->setAttribute('class', $class->getName());
                    $testCase->setAttribute('file', $class->getFileName());
                    $testCase->setAttribute('line', $method->getStartLine());
                }
            }

            $this->currentTestCase = $testCase;
        }
    }

    /**
     * A test ended.
     *
     * @param PHPUnit_Framework_Test $test
     * @param float                  $time
     */
    public function endTest(PHPUnit_Framework_Test $test, $time)
    {
        if (!$test instanceof PHPUnit_Framework_Warning) {
            if ($this->attachCurrentTestCase) {
                if ($test instanceof PHPUnit_Framework_TestCase) {
                    $numAssertions = $test->getNumAssertions();
                    $this->testSuiteAssertions[$this->testSuiteLevel] += $numAssertions;

                    $this->currentTestCase->setAttribute(
                        'assertions',
                        $numAssertions
                    );
                }

                $this->currentTestCase->setAttribute(
                    'time',
                    sprintf('%F', $time)
                );

                $this->testSuites[$this->testSuiteLevel]->appendChild(
                    $this->currentTestCase
                );

                $this->testSuiteTests[$this->testSuiteLevel]++;
                $this->testSuiteTimes[$this->testSuiteLevel] += $time;

                if (method_exists($test, 'hasOutput') && $test->hasOutput()) {
                    $systemOut = $this->document->createElement('system-out');
                    $systemOut->appendChild(
                        $this->document->createTextNode($test->getActualOutput())
                    );
                    $this->currentTestCase->appendChild($systemOut);
                }
            }
        }

        $this->attachCurrentTestCase = true;
        $this->currentTestCase       = null;
    }

    /**
     * Returns the XML as a string.
     *
     * @return string
     * @since  Method available since Release 2.2.0
     */
    public function getXML()
    {
        return $this->document->saveXML();
    }

    /**
     * Enables or disables the writing of the document
     * in flush().
     *
     * This is a "hack" needed for the integration of
     * PHPUnit with Phing.
     *
     * @return string
     * @since  Method available since Release 2.2.0
     */
    public function setWriteDocument($flag)
    {
        if (is_bool($flag)) {
            $this->writeDocument = $flag;
        }
    }
}
