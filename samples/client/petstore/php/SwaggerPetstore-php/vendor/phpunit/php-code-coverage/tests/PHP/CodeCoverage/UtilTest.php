<?php
/*
 * This file is part of the PHP_CodeCoverage package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Tests for the PHP_CodeCoverage_Util class.
 *
 * @category   PHP
 * @package    CodeCoverage
 * @subpackage Tests
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/php-code-coverage
 * @since      Class available since Release 1.0.0
 */
class PHP_CodeCoverage_UtilTest extends PHPUnit_Framework_TestCase
{
    /**
     * @covers PHP_CodeCoverage_Util::percent
     */
    public function testPercent()
    {
        $this->assertEquals(100, PHP_CodeCoverage_Util::percent(100, 0));
        $this->assertEquals(100, PHP_CodeCoverage_Util::percent(100, 100));
        $this->assertEquals(
          '100.00%', PHP_CodeCoverage_Util::percent(100, 100, true)
        );
    }
}
