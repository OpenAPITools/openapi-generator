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
 *
 *
 * @package    PHPUnit
 * @author     Giorgio Sironi <info@giorgiosironi.com>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 4.0.0
 */
class Framework_BaseTestListenerTest extends PHPUnit_Framework_TestCase
{
    /**
     * @var PHPUnit_Framework_TestResult
     */
    private $result;

    /**
     * @covers PHPUnit_Framework_TestResult
     */
    public function testEndEventsAreCounted()
    {
        $this->result = new PHPUnit_Framework_TestResult;
        $listener = new BaseTestListenerSample();
        $this->result->addListener($listener);
        $test = new Success;
        $test->run($this->result);

        $this->assertEquals(1, $listener->endCount);
    }
}
