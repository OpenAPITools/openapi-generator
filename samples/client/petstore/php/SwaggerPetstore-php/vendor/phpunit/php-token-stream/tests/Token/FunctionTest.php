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
 * Tests for the PHP_Token_FUNCTION class.
 *
 * @package    PHP_TokenStream
 * @subpackage Tests
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/php-token-stream/
 * @since      Class available since Release 1.0.0
 */
class PHP_Token_FunctionTest extends PHPUnit_Framework_TestCase
{
    protected $functions;

    protected function setUp()
    {
        $ts = new PHP_Token_Stream(TEST_FILES_PATH . 'source.php');

        foreach ($ts as $token) {
            if ($token instanceof PHP_Token_FUNCTION) {
                $this->functions[] = $token;
            }
        }
    }

    /**
     * @covers PHP_Token_FUNCTION::getArguments
     */
    public function testGetArguments()
    {
        $this->assertEquals(array(), $this->functions[0]->getArguments());

        $this->assertEquals(
          array('$baz' => 'Baz'), $this->functions[1]->getArguments()
        );

        $this->assertEquals(
          array('$foobar' => 'Foobar'), $this->functions[2]->getArguments()
        );

        $this->assertEquals(
          array('$barfoo' => 'Barfoo'), $this->functions[3]->getArguments()
        );

        $this->assertEquals(array(), $this->functions[4]->getArguments());

        $this->assertEquals(array('$x' => null, '$y' => null), $this->functions[5]->getArguments());
    }

    /**
     * @covers PHP_Token_FUNCTION::getName
     */
    public function testGetName()
    {
        $this->assertEquals('foo', $this->functions[0]->getName());
        $this->assertEquals('bar', $this->functions[1]->getName());
        $this->assertEquals('foobar', $this->functions[2]->getName());
        $this->assertEquals('barfoo', $this->functions[3]->getName());
        $this->assertEquals('baz', $this->functions[4]->getName());
    }

    /**
     * @covers PHP_Token::getLine
     */
    public function testGetLine()
    {
        $this->assertEquals(5, $this->functions[0]->getLine());
        $this->assertEquals(10, $this->functions[1]->getLine());
        $this->assertEquals(17, $this->functions[2]->getLine());
        $this->assertEquals(21, $this->functions[3]->getLine());
        $this->assertEquals(29, $this->functions[4]->getLine());
    }

    /**
     * @covers PHP_TokenWithScope::getEndLine
     */
    public function testGetEndLine()
    {
        $this->assertEquals(5, $this->functions[0]->getEndLine());
        $this->assertEquals(12, $this->functions[1]->getEndLine());
        $this->assertEquals(19, $this->functions[2]->getEndLine());
        $this->assertEquals(23, $this->functions[3]->getEndLine());
        $this->assertEquals(31, $this->functions[4]->getEndLine());
    }

    /**
     * @covers PHP_Token_FUNCTION::getDocblock
     */
    public function testGetDocblock()
    {
        $this->assertNull($this->functions[0]->getDocblock());

        $this->assertEquals(
          "/**\n     * @param Baz \$baz\n     */",
          $this->functions[1]->getDocblock()
        );

        $this->assertEquals(
          "/**\n     * @param Foobar \$foobar\n     */",
          $this->functions[2]->getDocblock()
        );

        $this->assertNull($this->functions[3]->getDocblock());
        $this->assertNull($this->functions[4]->getDocblock());
    }

    public function testSignature()
    {
        $ts = new PHP_Token_Stream(TEST_FILES_PATH . 'source5.php');
        $f  = $ts->getFunctions();
        $c  = $ts->getClasses();
        $i  = $ts->getInterfaces();

        $this->assertEquals(
          'foo($a, array $b, array $c = array())',
          $f['foo']['signature']
        );

        $this->assertEquals(
          'm($a, array $b, array $c = array())',
          $c['c']['methods']['m']['signature']
        );

        $this->assertEquals(
          'm($a, array $b, array $c = array())',
          $c['a']['methods']['m']['signature']
        );

        $this->assertEquals(
          'm($a, array $b, array $c = array())',
          $i['i']['methods']['m']['signature']
        );
    }
}
