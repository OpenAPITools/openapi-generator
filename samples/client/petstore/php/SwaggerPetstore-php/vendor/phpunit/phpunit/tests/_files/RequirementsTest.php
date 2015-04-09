<?php
class RequirementsTest extends PHPUnit_Framework_TestCase
{
    public function testOne()
    {
    }

    /**
     * @requires PHPUnit 1.0
     */
    public function testTwo()
    {
    }

    /**
     * @requires PHP 2.0
     */
    public function testThree()
    {
    }

    /**
     * @requires PHPUnit 2.0
     * @requires PHP 1.0
     */
    public function testFour()
    {
    }

    /**
     * @requires PHP 5.4.0RC6
     */
    public function testFive()
    {
    }

    /**
     * @requires PHP 5.4.0-alpha1
     */
    public function testSix()
    {
    }

    /**
     * @requires PHP 5.4.0beta2
     */
    public function testSeven()
    {
    }

    /**
     * @requires PHP 5.4-dev
     */
    public function testEight()
    {
    }


    /**
     * @requires function testFunc
     */
    public function testNine()
    {
    }

    /**
     * @requires extension testExt
     */
    public function testTen()
    {
    }

    /**
     * @requires OS Linux
     */
    public function testEleven()
    {
    }

    /**
     * @requires PHP 99-dev
     * @requires PHPUnit 9-dev
     * @requires OS DOESNOTEXIST
     * @requires function testFuncOne
     * @requires function testFuncTwo
     * @requires extension testExtOne
     * @requires extension testExtTwo
     */
    public function testAllPossibleRequirements()
    {
    }

    /**
     * @requires function array_merge
     */
    public function testExistingFunction()
    {
    }

    /**
     * @requires function ReflectionMethod::setAccessible
     */
    public function testExistingMethod()
    {
    }

    /**
     * @requires extension spl
     */
    public function testExistingExtension()
    {
    }

    /**
     * @requires OS .*
     */
    public function testExistingOs()
    {
    }

    /**
     * @requires PHPUnit 1111111
     */
    public function testAlwaysSkip()
    {
    }

    /**
     * @requires PHP 9999999
     */
    public function testAlwaysSkip2()
    {
    }

    /**
     * @requires OS DOESNOTEXIST
     */
    public function testAlwaysSkip3()
    {
    }

    /**
     * @requires	  extension	  spl
     * @requires	  OS	  .*
     */
    public function testSpace()
    {
    }
}
