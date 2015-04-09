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
 * Runner for PHPT test cases.
 *
 * @package    PHPUnit
 * @subpackage Extensions_PhptTestCase
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.1.4
 */
class PHPUnit_Extensions_PhptTestCase implements PHPUnit_Framework_Test, PHPUnit_Framework_SelfDescribing
{
    /**
     * @var string
     */
    private $filename;

    /**
     * @var array
     */
    private $settings = array(
        'allow_url_fopen=1',
        'auto_append_file=',
        'auto_prepend_file=',
        'disable_functions=',
        'display_errors=1',
        'docref_root=',
        'docref_ext=.html',
        'error_append_string=',
        'error_prepend_string=',
        'error_reporting=-1',
        'html_errors=0',
        'log_errors=0',
        'magic_quotes_runtime=0',
        'output_handler=',
        'open_basedir=',
        'output_buffering=Off',
        'report_memleaks=0',
        'report_zend_debug=0',
        'safe_mode=0',
        'track_errors=1',
        'xdebug.default_enable=0'
    );

    /**
     * Constructs a test case with the given filename.
     *
     * @param  string                      $filename
     * @throws PHPUnit_Framework_Exception
     */
    public function __construct($filename)
    {
        if (!is_string($filename)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(1, 'string');
        }

        if (!is_file($filename)) {
            throw new PHPUnit_Framework_Exception(
                sprintf(
                    'File "%s" does not exist.',
                    $filename
                )
            );
        }

        $this->filename = $filename;
    }

    /**
     * Counts the number of test cases executed by run(TestResult result).
     *
     * @return integer
     */
    public function count()
    {
        return 1;
    }

    /**
     * Runs a test and collects its result in a TestResult instance.
     *
     * @param  PHPUnit_Framework_TestResult $result
     * @return PHPUnit_Framework_TestResult
     */
    public function run(PHPUnit_Framework_TestResult $result = null)
    {
        $sections = $this->parse();
        $code     = $this->render($sections['FILE']);

        if ($result === null) {
            $result = new PHPUnit_Framework_TestResult;
        }

        $php  = PHPUnit_Util_PHP::factory();
        $skip = false;
        $time = 0;

        $result->startTest($this);

        if (isset($sections['SKIPIF'])) {
            $jobResult = $php->runJob($sections['SKIPIF'], $this->settings);

            if (!strncasecmp('skip', ltrim($jobResult['stdout']), 4)) {
                if (preg_match('/^\s*skip\s*(.+)\s*/i', $jobResult['stdout'], $message)) {
                    $message = substr($message[1], 2);
                } else {
                    $message = '';
                }

                $result->addFailure($this, new PHPUnit_Framework_SkippedTestError($message), 0);

                $skip = true;
            }
        }

        if (!$skip) {
            PHP_Timer::start();
            $jobResult = $php->runJob($code, $this->settings);
            $time = PHP_Timer::stop();

            if (isset($sections['EXPECT'])) {
                $assertion = 'assertEquals';
                $expected  = $sections['EXPECT'];
            } else {
                $assertion = 'assertStringMatchesFormat';
                $expected  = $sections['EXPECTF'];
            }

            $output = preg_replace('/\r\n/', "\n", trim($jobResult['stdout']));
            $expected = preg_replace('/\r\n/', "\n", trim($expected));

            try {
                PHPUnit_Framework_Assert::$assertion($expected, $output);
            } catch (PHPUnit_Framework_AssertionFailedError $e) {
                $result->addFailure($this, $e, $time);
            } catch (Exception $e) {
                $result->addError($this, $e, $time);
            }
        }

        $result->endTest($this, $time);

        return $result;
    }

    /**
     * Returns the name of the test case.
     *
     * @return string
     */
    public function getName()
    {
        return $this->toString();
    }

    /**
     * Returns a string representation of the test case.
     *
     * @return string
     */
    public function toString()
    {
        return $this->filename;
    }

    /**
     * @return array
     * @throws PHPUnit_Framework_Exception
     */
    private function parse()
    {
        $sections = array();
        $section  = '';

        foreach (file($this->filename) as $line) {
            if (preg_match('/^--([_A-Z]+)--/', $line, $result)) {
                $section            = $result[1];
                $sections[$section] = '';
                continue;
            } elseif (empty($section)) {
                throw new PHPUnit_Framework_Exception('Invalid PHPT file');
            }

            $sections[$section] .= $line;
        }

        if (!isset($sections['FILE']) ||
            (!isset($sections['EXPECT']) && !isset($sections['EXPECTF']))) {
            throw new PHPUnit_Framework_Exception('Invalid PHPT file');
        }

        return $sections;
    }

    /**
     * @param  string $code
     * @return string
     */
    private function render($code)
    {
        return str_replace(
            array(
            '__DIR__',
            '__FILE__'
            ),
            array(
            "'" . dirname($this->filename) . "'",
            "'" . $this->filename . "'"
            ),
            $code
        );
    }
}
