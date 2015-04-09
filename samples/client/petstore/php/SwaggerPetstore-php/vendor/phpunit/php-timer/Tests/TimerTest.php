<?php
/**
 * PHP_Timer
 *
 * Copyright (c) 2010-2013, Sebastian Bergmann <sebastian@phpunit.de>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *
 *   * Neither the name of Sebastian Bergmann nor the names of his
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * @package    PHP
 * @subpackage Timer
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  2010 Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/php-timer
 * @since      File available since Release 1.0.0
 */

require_once dirname(dirname(__FILE__)) . '/PHP/Timer.php';

/**
 * Tests for PHP_Timer.
 *
 * @package    PHP
 * @subpackage Timer
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  2010-2013 Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/php-timer
 * @since      Class available since Release 1.0.0
 */
class PHP_TimerTest extends PHPUnit_Framework_TestCase
{
    /**
     * @covers PHP_Timer::start
     * @covers PHP_Timer::stop
     */
    public function testStartStop()
    {
        $this->assertInternalType('float', PHP_Timer::stop());
    }

    /**
     * @covers       PHP_Timer::secondsToTimeString
     * @dataProvider secondsProvider
     */
    public function testSecondsToTimeString($string, $seconds)
    {
        $this->assertEquals(
          $string, PHP_Timer::secondsToTimeString($seconds)
        );
    }

    /**
     * @covers PHP_Timer::timeSinceStartOfRequest
     */
    public function testTimeSinceStartOfRequest()
    {
        $this->assertStringMatchesFormat(
          '%f %s', PHP_Timer::timeSinceStartOfRequest()
        );
    }


    /**
     * @covers PHP_Timer::resourceUsage
     */
    public function testResourceUsage()
    {
        $this->assertStringMatchesFormat(
          'Time: %s, Memory: %s', PHP_Timer::resourceUsage()
        );
    }

    public function secondsProvider()
    {
        return array(
          array('0 ms', 0),
          array('1 ms', .001),
          array('10 ms', .01),
          array('100 ms', .1),
          array('999 ms', .999),
          array('1 second', .9999),
          array('1 second', 1),
          array('2 seconds', 2),
          array('59.9 seconds', 59.9),
          array('59.99 seconds', 59.99),
          array('59.99 seconds', 59.999),
          array('1 minute', 59.9999),
          array('59 seconds', 59.001),
          array('59.01 seconds', 59.01),
          array('1 minute', 60),
          array('1.01 minutes', 61),
          array('2 minutes', 120),
          array('2.01 minutes', 121),
          array('59.99 minutes', 3599.9),
          array('59.99 minutes', 3599.99),
          array('59.99 minutes', 3599.999),
          array('1 hour', 3599.9999),
          array('59.98 minutes', 3599.001),
          array('59.98 minutes', 3599.01),
          array('1 hour', 3600),
          array('1 hour', 3601),
          array('1 hour', 3601.9),
          array('1 hour', 3601.99),
          array('1 hour', 3601.999),
          array('1 hour', 3601.9999),
          array('1.01 hours', 3659.9999),
          array('1.01 hours', 3659.001),
          array('1.01 hours', 3659.01),
          array('2 hours', 7199.9999),
        );
    }
}
