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
 * @since      Class available since Release 2.1.0
 */
class Util_TestDox_NamePrettifierTest extends PHPUnit_Framework_TestCase
{
    protected $namePrettifier;

    protected function setUp()
    {
        $this->namePrettifier = new PHPUnit_Util_TestDox_NamePrettifier;
    }

    /**
     * @covers PHPUnit_Util_TestDox_NamePrettifier::prettifyTestClass
     */
    public function testTitleHasSensibleDefaults()
    {
        $this->assertEquals('Foo', $this->namePrettifier->prettifyTestClass('FooTest'));
        $this->assertEquals('Foo', $this->namePrettifier->prettifyTestClass('TestFoo'));
        $this->assertEquals('Foo', $this->namePrettifier->prettifyTestClass('TestFooTest'));
        $this->assertEquals('Foo', $this->namePrettifier->prettifyTestClass('Test\FooTest'));
    }

    /**
     * @covers PHPUnit_Util_TestDox_NamePrettifier::prettifyTestClass
     */
    public function testCaterForUserDefinedSuffix()
    {
        $this->namePrettifier->setSuffix('TestCase');
        $this->namePrettifier->setPrefix(null);

        $this->assertEquals('Foo', $this->namePrettifier->prettifyTestClass('FooTestCase'));
        $this->assertEquals('TestFoo', $this->namePrettifier->prettifyTestClass('TestFoo'));
        $this->assertEquals('FooTest', $this->namePrettifier->prettifyTestClass('FooTest'));
    }

    /**
     * @covers PHPUnit_Util_TestDox_NamePrettifier::prettifyTestClass
     */
    public function testCaterForUserDefinedPrefix()
    {
        $this->namePrettifier->setSuffix(null);
        $this->namePrettifier->setPrefix('XXX');

        $this->assertEquals('Foo', $this->namePrettifier->prettifyTestClass('XXXFoo'));
        $this->assertEquals('TestXXX', $this->namePrettifier->prettifyTestClass('TestXXX'));
        $this->assertEquals('XXX', $this->namePrettifier->prettifyTestClass('XXXXXX'));
    }

    /**
     * @covers PHPUnit_Util_TestDox_NamePrettifier::prettifyTestMethod
     */
    public function testTestNameIsConvertedToASentence()
    {
        $this->assertEquals('This is a test', $this->namePrettifier->prettifyTestMethod('testThisIsATest'));
        $this->assertEquals('This is a test', $this->namePrettifier->prettifyTestMethod('testThisIsATest2'));
        $this->assertEquals('this is a test', $this->namePrettifier->prettifyTestMethod('this_is_a_test'));
        $this->assertEquals('Foo for bar is 0', $this->namePrettifier->prettifyTestMethod('testFooForBarIs0'));
        $this->assertEquals('Foo for baz is 1', $this->namePrettifier->prettifyTestMethod('testFooForBazIs1'));
    }

    /**
     * @covers PHPUnit_Util_TestDox_NamePrettifier::prettifyTestMethod
     * @ticket 224
     */
    public function testTestNameIsNotGroupedWhenNotInSequence()
    {
        $this->assertEquals('Sets redirect header on 301', $this->namePrettifier->prettifyTestMethod('testSetsRedirectHeaderOn301'));
        $this->assertEquals('Sets redirect header on 302', $this->namePrettifier->prettifyTestMethod('testSetsRedirectHeaderOn302'));
    }
}
