<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

if (!defined('TEST_FILES_PATH')) {
    define(
        'TEST_FILES_PATH',
        dirname(__DIR__) . DIRECTORY_SEPARATOR .
        '_files' . DIRECTORY_SEPARATOR
    );
}

/**
 *
 *
 * @package    PHPUnit
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.3.6
 */
class Util_TestTest extends PHPUnit_Framework_TestCase
{
    /**
     * @covers PHPUnit_Util_Test::getExpectedException
     * @todo   Split up in separate tests
     */
    public function testGetExpectedException()
    {
        $this->assertArraySubset(
          array('class' => 'FooBarBaz', 'code' => null, 'message' => ''),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testOne')
        );

        $this->assertArraySubset(
          array('class' => 'Foo_Bar_Baz', 'code' => null, 'message' => ''),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testTwo')
        );

        $this->assertArraySubset(
          array('class' => 'Foo\Bar\Baz', 'code' => null, 'message' => ''),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testThree')
        );

        $this->assertArraySubset(
          array('class' => 'ほげ', 'code' => null, 'message' => ''),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testFour')
        );

        $this->assertArraySubset(
          array('class' => 'Class', 'code' => 1234, 'message' => 'Message'),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testFive')
        );

        $this->assertArraySubset(
          array('class' => 'Class', 'code' => 1234, 'message' => 'Message'),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testSix')
        );

        $this->assertArraySubset(
          array('class' => 'Class', 'code' => 'ExceptionCode', 'message' => 'Message'),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testSeven')
        );

        $this->assertArraySubset(
          array('class' => 'Class', 'code' => 0, 'message' => 'Message'),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testEight')
        );

        $this->assertArraySubset(
          array('class' => 'Class', 'code' => ExceptionTest::ERROR_CODE, 'message' => ExceptionTest::ERROR_MESSAGE),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testNine')
        );

        $this->assertArraySubset(
          array('class' => 'Class', 'code' => null, 'message' => ''),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testSingleLine')
        );

        $this->assertArraySubset(
          array('class' => 'Class', 'code' => My\Space\ExceptionNamespaceTest::ERROR_CODE, 'message' => My\Space\ExceptionNamespaceTest::ERROR_MESSAGE),
          PHPUnit_Util_Test::getExpectedException('My\Space\ExceptionNamespaceTest', 'testConstants')
        );

        // Ensure the Class::CONST expression is only evaluated when the constant really exists
        $this->assertArraySubset(
          array('class' => 'Class', 'code' => 'ExceptionTest::UNKNOWN_CODE_CONSTANT', 'message' => 'ExceptionTest::UNKNOWN_MESSAGE_CONSTANT'),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testUnknownConstants')
        );

        $this->assertArraySubset(
          array('class' => 'Class', 'code' => 'My\Space\ExceptionNamespaceTest::UNKNOWN_CODE_CONSTANT', 'message' => 'My\Space\ExceptionNamespaceTest::UNKNOWN_MESSAGE_CONSTANT'),
          PHPUnit_Util_Test::getExpectedException('My\Space\ExceptionNamespaceTest', 'testUnknownConstants')
        );
    }

    /**
     * @covers PHPUnit_Util_Test::getExpectedException
     */
    public function testGetExpectedRegExp()
    {
        $this->assertArraySubset(
          array('message_regex' => '#regex#'),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testWithRegexMessage')
        );

        $this->assertArraySubset(
          array('message_regex' => '#regex#'),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testWithRegexMessageFromClassConstant')
        );

        $this->assertArraySubset(
          array('message_regex' => 'ExceptionTest::UNKNOWN_MESSAGE_REGEX_CONSTANT'),
          PHPUnit_Util_Test::getExpectedException('ExceptionTest', 'testWithUnknowRegexMessageFromClassConstant')
        );
    }

    /**
     * @covers       PHPUnit_Util_Test::getRequirements
     * @dataProvider requirementsProvider
     */
    public function testGetRequirements($test, $result)
    {
        $this->assertEquals(
            $result,
            PHPUnit_Util_Test::getRequirements('RequirementsTest', $test)
        );
    }

    public function requirementsProvider()
    {
        return array(
            array('testOne',    array()),
            array('testTwo',    array('PHPUnit' => '1.0')),
            array('testThree',  array('PHP' => '2.0')),
            array('testFour',   array('PHPUnit'=>'2.0', 'PHP' => '1.0')),
            array('testFive',   array('PHP' => '5.4.0RC6')),
            array('testSix',    array('PHP' => '5.4.0-alpha1')),
            array('testSeven',  array('PHP' => '5.4.0beta2')),
            array('testEight',  array('PHP' => '5.4-dev')),
            array('testNine',   array('functions' => array('testFunc'))),
            array('testTen',    array('extensions' => array('testExt'))),
            array('testEleven', array('OS' => '/Linux/i')),
            array(
              'testSpace',
              array(
                'extensions' => array('spl'),
                'OS' => '/.*/i'
              )
            ),
            array(
              'testAllPossibleRequirements',
              array(
                'PHP' => '99-dev',
                'PHPUnit' => '9-dev',
                'OS' => '/DOESNOTEXIST/i',
                'functions' => array(
                  'testFuncOne',
                  'testFuncTwo',
                ),
                'extensions' => array(
                  'testExtOne',
                  'testExtTwo',
                )
              )
            )
        );
    }

    /**
     * @covers PHPUnit_Util_Test::getRequirements
     */
    public function testGetRequirementsMergesClassAndMethodDocBlocks()
    {
        $expectedAnnotations = array(
            'PHP' => '5.4',
            'PHPUnit' => '3.7',
            'OS' => '/WINNT/i',
            'functions' => array(
              'testFuncClass',
              'testFuncMethod',
            ),
            'extensions' => array(
              'testExtClass',
              'testExtMethod',
            )
        );

        $this->assertEquals(
            $expectedAnnotations,
            PHPUnit_Util_Test::getRequirements('RequirementsClassDocBlockTest', 'testMethod')
        );
    }

    /**
     * @covers       PHPUnit_Util_Test::getMissingRequirements
     * @dataProvider missingRequirementsProvider
     */
    public function testGetMissingRequirements($test, $result)
    {
        $this->assertEquals(
            $result,
            PHPUnit_Util_Test::getMissingRequirements('RequirementsTest', $test)
        );
    }

    public function missingRequirementsProvider()
    {
        return array(
            array('testOne',            array()),
            array('testNine',           array('Function testFunc is required.')),
            array('testTen',            array('Extension testExt is required.')),
            array('testAlwaysSkip',     array('PHPUnit 1111111 (or later) is required.')),
            array('testAlwaysSkip2',    array('PHP 9999999 (or later) is required.')),
            array('testAlwaysSkip3',    array('Operating system matching /DOESNOTEXIST/i is required.')),
            array('testAllPossibleRequirements', array(
              'PHP 99-dev (or later) is required.',
              'PHPUnit 9-dev (or later) is required.',
              'Operating system matching /DOESNOTEXIST/i is required.',
              'Function testFuncOne is required.',
              'Function testFuncTwo is required.',
              'Extension testExtOne is required.',
              'Extension testExtTwo is required.',
            )),
        );
    }

    /**
     * @coversNothing
     * @todo   This test does not really test functionality of PHPUnit_Util_Test
     */
    public function testGetProvidedDataRegEx()
    {
        $result = preg_match(PHPUnit_Util_Test::REGEX_DATA_PROVIDER, '@dataProvider method', $matches);
        $this->assertEquals(1, $result);
        $this->assertEquals('method', $matches[1]);

        $result = preg_match(PHPUnit_Util_Test::REGEX_DATA_PROVIDER, '@dataProvider class::method', $matches);
        $this->assertEquals(1, $result);
        $this->assertEquals('class::method', $matches[1]);

        $result = preg_match(PHPUnit_Util_Test::REGEX_DATA_PROVIDER, '@dataProvider namespace\class::method', $matches);
        $this->assertEquals(1, $result);
        $this->assertEquals('namespace\class::method', $matches[1]);

        $result = preg_match(PHPUnit_Util_Test::REGEX_DATA_PROVIDER, '@dataProvider namespace\namespace\class::method', $matches);
        $this->assertEquals(1, $result);
        $this->assertEquals('namespace\namespace\class::method', $matches[1]);

        $result = preg_match(PHPUnit_Util_Test::REGEX_DATA_PROVIDER, '@dataProvider メソッド', $matches);
        $this->assertEquals(1, $result);
        $this->assertEquals('メソッド', $matches[1]);
    }

    /**
     * @covers PHPUnit_Util_Test::getDependencies
     * @todo   Not sure what this test tests (name is misleading at least)
     */
    public function testParseAnnotation()
    {
        $this->assertEquals(
            array('Foo', 'ほげ'),
            PHPUnit_Util_Test::getDependencies(get_class($this), 'methodForTestParseAnnotation')
        );
    }

    /**
     * @depends Foo
     * @depends ほげ
     * @todo    Remove fixture from test class
     */
    public function methodForTestParseAnnotation()
    {
    }

    /**
     * @covers PHPUnit_Util_Test::getDependencies
     */
    public function testParseAnnotationThatIsOnlyOneLine()
    {
        $this->assertEquals(
            array('Bar'),
            PHPUnit_Util_Test::getDependencies(get_class($this), 'methodForTestParseAnnotationThatIsOnlyOneLine')
        );
    }

    /** @depends Bar */
    public function methodForTestParseAnnotationThatIsOnlyOneLine()
    {
        // TODO Remove fixture from test class
    }

    /**
     * @covers       PHPUnit_Util_Test::getLinesToBeCovered
     * @covers       PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     * @covers       PHPUnit_Util_Test::resolveElementToReflectionObjects
     * @dataProvider getLinesToBeCoveredProvider
     */
    public function testGetLinesToBeCovered($test, $lines)
    {
        if (strpos($test, 'Namespace') === 0) {
            $expected = array(
              TEST_FILES_PATH . 'NamespaceCoveredClass.php' => $lines
            );
        } elseif ($test === 'CoverageNoneTest') {
            $expected = array();
        } elseif ($test === 'CoverageNothingTest') {
            $expected = false;
        } elseif ($test === 'CoverageFunctionTest') {
            $expected = array(
              TEST_FILES_PATH . 'CoveredFunction.php' => $lines
            );
        } else {
            $expected = array(TEST_FILES_PATH . 'CoveredClass.php' => $lines);
        }

        $this->assertEquals(
            $expected,
            PHPUnit_Util_Test::getLinesToBeCovered(
                $test, 'testSomething'
            )
        );
    }

    /**
     * @covers            PHPUnit_Util_Test::getLinesToBeCovered
     * @covers            PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     * @covers            PHPUnit_Util_Test::resolveElementToReflectionObjects
     * @expectedException PHPUnit_Framework_CodeCoverageException
     */
    public function testGetLinesToBeCovered2()
    {
        PHPUnit_Util_Test::getLinesToBeCovered(
            'NotExistingCoveredElementTest', 'testOne'
        );
    }

    /**
     * @covers            PHPUnit_Util_Test::getLinesToBeCovered
     * @covers            PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     * @covers            PHPUnit_Util_Test::resolveElementToReflectionObjects
     * @expectedException PHPUnit_Framework_CodeCoverageException
     */
    public function testGetLinesToBeCovered3()
    {
        PHPUnit_Util_Test::getLinesToBeCovered(
            'NotExistingCoveredElementTest', 'testTwo'
        );
    }

    /**
     * @covers            PHPUnit_Util_Test::getLinesToBeCovered
     * @covers            PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     * @covers            PHPUnit_Util_Test::resolveElementToReflectionObjects
     * @expectedException PHPUnit_Framework_CodeCoverageException
     */
    public function testGetLinesToBeCovered4()
    {
        PHPUnit_Util_Test::getLinesToBeCovered(
            'NotExistingCoveredElementTest', 'testThree'
        );
    }

    /**
     * @covers PHPUnit_Util_Test::getLinesToBeCovered
     * @covers PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     */
    public function testGetLinesToBeCoveredSkipsNonExistentMethods()
    {
        $this->assertSame(
            array(),
            PHPUnit_Util_Test::getLinesToBeCovered(
                'NotExistingCoveredElementTest',
                'methodDoesNotExist'
            )
        );
    }

    /**
     * @covers            PHPUnit_Util_Test::getLinesToBeCovered
     * @covers            PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     * @expectedException PHPUnit_Framework_CodeCoverageException
     */
    public function testTwoCoversDefaultClassAnnoationsAreNotAllowed()
    {
        PHPUnit_Util_Test::getLinesToBeCovered(
            'CoverageTwoDefaultClassAnnotations',
            'testSomething'
        );
    }

    /**
     * @covers PHPUnit_Util_Test::getLinesToBeCovered
     * @covers PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     */
    public function testFunctionParenthesesAreAllowed()
    {
        $this->assertSame(
            array(TEST_FILES_PATH . 'CoveredFunction.php' => range(2, 4)),
            PHPUnit_Util_Test::getLinesToBeCovered(
                'CoverageFunctionParenthesesTest',
                'testSomething'
            )
        );
    }

    /**
     * @covers PHPUnit_Util_Test::getLinesToBeCovered
     * @covers PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     */
    public function testFunctionParenthesesAreAllowedWithWhitespace()
    {
        $this->assertSame(
            array(TEST_FILES_PATH . 'CoveredFunction.php' => range(2, 4)),
            PHPUnit_Util_Test::getLinesToBeCovered(
                'CoverageFunctionParenthesesWhitespaceTest',
                'testSomething'
            )
        );
    }

    /**
     * @covers PHPUnit_Util_Test::getLinesToBeCovered
     * @covers PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     */
    public function testMethodParenthesesAreAllowed()
    {
        $this->assertSame(
            array(TEST_FILES_PATH . 'CoveredClass.php' => range(31, 35)),
            PHPUnit_Util_Test::getLinesToBeCovered(
                'CoverageMethodParenthesesTest',
                'testSomething'
            )
        );
    }

    /**
     * @covers PHPUnit_Util_Test::getLinesToBeCovered
     * @covers PHPUnit_Util_Test::getLinesToBeCoveredOrUsed
     */
    public function testMethodParenthesesAreAllowedWithWhitespace()
    {
        $this->assertSame(
            array(TEST_FILES_PATH . 'CoveredClass.php' => range(31, 35)),
            PHPUnit_Util_Test::getLinesToBeCovered(
                'CoverageMethodParenthesesWhitespaceTest',
                'testSomething'
            )
        );
    }

    public function getLinesToBeCoveredProvider()
    {
        return array(
          array(
            'CoverageNoneTest',
            array()
          ),
          array(
            'CoverageClassExtendedTest',
            array_merge(range(19, 36), range(2, 17))
          ),
          array(
            'CoverageClassTest',
            range(19, 36)
          ),
          array(
            'CoverageMethodTest',
            range(31, 35)
          ),
          array(
            'CoverageMethodOneLineAnnotationTest',
            range(31, 35)
          ),
          array(
            'CoverageNotPrivateTest',
            array_merge(range(25, 29), range(31, 35))
          ),
          array(
            'CoverageNotProtectedTest',
            array_merge(range(21, 23), range(31, 35))
          ),
          array(
            'CoverageNotPublicTest',
            array_merge(range(21, 23), range(25, 29))
          ),
          array(
            'CoveragePrivateTest',
            range(21, 23)
          ),
          array(
            'CoverageProtectedTest',
            range(25, 29)
          ),
          array(
            'CoveragePublicTest',
            range(31, 35)
          ),
          array(
            'CoverageFunctionTest',
            range(2, 4)
          ),
          array(
            'NamespaceCoverageClassExtendedTest',
            array_merge(range(21, 38), range(4, 19))
          ),
          array(
            'NamespaceCoverageClassTest',
            range(21, 38)
          ),
          array(
            'NamespaceCoverageMethodTest',
            range(33, 37)
          ),
          array(
            'NamespaceCoverageNotPrivateTest',
            array_merge(range(27, 31), range(33, 37))
          ),
          array(
            'NamespaceCoverageNotProtectedTest',
            array_merge(range(23, 25), range(33, 37))
          ),
          array(
            'NamespaceCoverageNotPublicTest',
            array_merge(range(23, 25), range(27, 31))
          ),
          array(
            'NamespaceCoveragePrivateTest',
            range(23, 25)
          ),
          array(
            'NamespaceCoverageProtectedTest',
            range(27, 31)
          ),
          array(
            'NamespaceCoveragePublicTest',
            range(33, 37)
          ),
          array(
            'NamespaceCoverageCoversClassTest',
            array_merge(range(23, 25), range(27, 31), range(33, 37), range(6, 8), range(10, 13), range(15, 18))
          ),
          array(
            'NamespaceCoverageCoversClassPublicTest',
            range(33, 37)
          ),
          array(
            'CoverageNothingTest',
            false
          )
        );
    }
}
