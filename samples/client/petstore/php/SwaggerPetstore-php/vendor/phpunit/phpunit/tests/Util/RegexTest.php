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
 * @author     Jeff Welch <whatthejeff@gmail.com>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 4.2.0
 */
class Util_RegexTest extends PHPUnit_Framework_TestCase
{
    public function validRegexpProvider()
    {
        return array(
          array('#valid regexp#', 'valid regexp', 1),
          array(';val.*xp;', 'valid regexp', 1),
          array('/val.*xp/i', 'VALID REGEXP', 1),
          array('/a val.*p/','valid regexp', 0),
        );
    }

    public function invalidRegexpProvider()
    {
        return array(
          array('valid regexp', 'valid regexp'),
          array(';val.*xp', 'valid regexp'),
          array('val.*xp/i', 'VALID REGEXP'),
        );
    }

    /**
     * @dataProvider validRegexpProvider
     * @covers       PHPUnit_Util_Regex::pregMatchSafe
     */
    public function testValidRegex($pattern, $subject, $return)
    {
        $this->assertEquals($return, PHPUnit_Util_Regex::pregMatchSafe($pattern, $subject));
    }

    /**
     * @dataProvider invalidRegexpProvider
     * @covers       PHPUnit_Util_Regex::pregMatchSafe
     */
    public function testInvalidRegex($pattern, $subject)
    {
        $this->assertFalse(PHPUnit_Util_Regex::pregMatchSafe($pattern, $subject));
    }
}
