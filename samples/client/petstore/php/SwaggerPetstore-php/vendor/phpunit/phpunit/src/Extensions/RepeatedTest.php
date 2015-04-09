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
 * A Decorator that runs a test repeatedly.
 *
 * @package    PHPUnit
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 2.0.0
 */
class PHPUnit_Extensions_RepeatedTest extends PHPUnit_Extensions_TestDecorator
{
    /**
     * @var mixed
     */
    protected $filter = false;

    /**
     * @var array
     */
    protected $groups = array();

    /**
     * @var array
     */
    protected $excludeGroups = array();

    /**
     * @var boolean
     */
    protected $processIsolation = false;

    /**
     * @var integer
     */
    protected $timesRepeat = 1;

    /**
     * @param  PHPUnit_Framework_Test      $test
     * @param  integer                     $timesRepeat
     * @param  boolean                     $processIsolation
     * @throws PHPUnit_Framework_Exception
     */
    public function __construct(PHPUnit_Framework_Test $test, $timesRepeat = 1, $processIsolation = false)
    {
        parent::__construct($test);

        if (is_integer($timesRepeat) &&
            $timesRepeat >= 0) {
            $this->timesRepeat = $timesRepeat;
        } else {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(
                2,
                'positive integer'
            );
        }

        $this->processIsolation = $processIsolation;
    }

    /**
     * Counts the number of test cases that
     * will be run by this test.
     *
     * @return integer
     */
    public function count()
    {
        return $this->timesRepeat * count($this->test);
    }

    /**
     * Runs the decorated test and collects the
     * result in a TestResult.
     *
     * @param  PHPUnit_Framework_TestResult $result
     * @return PHPUnit_Framework_TestResult
     * @throws PHPUnit_Framework_Exception
     */
    public function run(PHPUnit_Framework_TestResult $result = null)
    {
        if ($result === null) {
            $result = $this->createResult();
        }

        //@codingStandardsIgnoreStart
        for ($i = 0; $i < $this->timesRepeat && !$result->shouldStop(); $i++) {
            //@codingStandardsIgnoreEnd
            if ($this->test instanceof PHPUnit_Framework_TestSuite) {
                $this->test->setRunTestInSeparateProcess($this->processIsolation);
            }
            $this->test->run($result);
        }

        return $result;
    }
}
