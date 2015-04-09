<?php
/*
 * This file is part of the PHP_TokenStream package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Tests for the PHP_Token_REQUIRE_ONCE, PHP_Token_REQUIRE
 * PHP_Token_INCLUDE_ONCE and PHP_Token_INCLUDE_ONCE classes.
 *
 * @package    PHP_TokenStream
 * @subpackage Tests
 * @author     Laurent Laville <pear@laurent-laville.org>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/php-token-stream/
 * @since      Class available since Release 1.0.2
 */
class PHP_Token_IncludeTest extends PHPUnit_Framework_TestCase
{
    protected $ts;

    protected function setUp()
    {
        $this->ts = new PHP_Token_Stream(TEST_FILES_PATH . 'source3.php');
    }

    /**
     * @covers PHP_Token_Includes::getName
     * @covers PHP_Token_Includes::getType
     */
    public function testGetIncludes()
    {
        $this->assertSame(
          array('test4.php', 'test3.php', 'test2.php', 'test1.php'),
          $this->ts->getIncludes()
        );
    }

    /**
     * @covers PHP_Token_Includes::getName
     * @covers PHP_Token_Includes::getType
     */
    public function testGetIncludesCategorized()
    {
        $this->assertSame(
          array(
            'require_once' => array('test4.php'),
            'require'      => array('test3.php'),
            'include_once' => array('test2.php'),
            'include'      => array('test1.php')
          ),
          $this->ts->getIncludes(TRUE)
        );
    }

    /**
     * @covers PHP_Token_Includes::getName
     * @covers PHP_Token_Includes::getType
     */
    public function testGetIncludesCategory()
    {
        $this->assertSame(
          array('test4.php'),
          $this->ts->getIncludes(TRUE, 'require_once')
        );
    }
}
