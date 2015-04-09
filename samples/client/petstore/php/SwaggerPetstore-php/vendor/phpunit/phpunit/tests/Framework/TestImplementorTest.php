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
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 2.0.0
 */
class Framework_TestImplementorTest extends PHPUnit_Framework_TestCase
{
    /**
     * @covers PHPUnit_Framework_TestCase
     */
    public function testSuccessfulRun()
    {
        $result = new PHPUnit_Framework_TestResult;

        $test = new DoubleTestCase(new Success);
        $test->run($result);

        $this->assertEquals(count($test), count($result));
        $this->assertEquals(0, $result->errorCount());
        $this->assertEquals(0, $result->failureCount());
    }
}
