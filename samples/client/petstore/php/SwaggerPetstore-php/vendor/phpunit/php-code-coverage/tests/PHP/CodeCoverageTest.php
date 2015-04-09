<?php
/*
 * This file is part of the PHP_CodeCoverage package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

if (!defined('TEST_FILES_PATH')) {
    define(
      'TEST_FILES_PATH',
      dirname(dirname(__FILE__)) . DIRECTORY_SEPARATOR .
      '_files' . DIRECTORY_SEPARATOR
    );
}

require_once TEST_FILES_PATH . '../TestCase.php';
require_once TEST_FILES_PATH . 'BankAccount.php';
require_once TEST_FILES_PATH . 'BankAccountTest.php';

/**
 * Tests for the PHP_CodeCoverage class.
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
class PHP_CodeCoverageTest extends PHP_CodeCoverage_TestCase
{
    private $coverage;

    protected function setUp()
    {
        $this->coverage = new PHP_CodeCoverage;
    }

    /**
     * @covers PHP_CodeCoverage::__construct
     * @covers PHP_CodeCoverage::filter
     */
    public function testConstructor()
    {
        $this->assertAttributeInstanceOf(
          'PHP_CodeCoverage_Driver_Xdebug', 'driver', $this->coverage
        );

        $this->assertAttributeInstanceOf(
          'PHP_CodeCoverage_Filter', 'filter', $this->coverage
        );
    }

    /**
     * @covers PHP_CodeCoverage::__construct
     * @covers PHP_CodeCoverage::filter
     */
    public function testConstructor2()
    {
        $filter   = new PHP_CodeCoverage_Filter;
        $coverage = new PHP_CodeCoverage(null, $filter);

        $this->assertAttributeInstanceOf(
          'PHP_CodeCoverage_Driver_Xdebug', 'driver', $coverage
        );

        $this->assertSame($filter, $coverage->filter());
    }

    /**
     * @covers            PHP_CodeCoverage::start
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testStartThrowsExceptionForInvalidArgument()
    {
        $this->coverage->start(null, array(), null);
    }

    /**
     * @covers            PHP_CodeCoverage::stop
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testStopThrowsExceptionForInvalidArgument()
    {
        $this->coverage->stop(null);
    }

    /**
     * @covers            PHP_CodeCoverage::stop
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testStopThrowsExceptionForInvalidArgument2()
    {
        $this->coverage->stop(true, null);
    }

    /**
     * @covers            PHP_CodeCoverage::append
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testAppendThrowsExceptionForInvalidArgument()
    {
        $this->coverage->append(array(), null);
    }

    /**
     * @covers            PHP_CodeCoverage::setCacheTokens
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testSetCacheTokensThrowsExceptionForInvalidArgument()
    {
        $this->coverage->setCacheTokens(null);
    }

    /**
     * @covers PHP_CodeCoverage::setCacheTokens
     */
    public function testSetCacheTokens()
    {
        $this->coverage->setCacheTokens(true);
        $this->assertAttributeEquals(true, 'cacheTokens', $this->coverage);
    }

    /**
     * @covers            PHP_CodeCoverage::setCheckForUnintentionallyCoveredCode
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testSetCheckForUnintentionallyCoveredCodeThrowsExceptionForInvalidArgument()
    {
        $this->coverage->setCheckForUnintentionallyCoveredCode(null);
    }

    /**
     * @covers PHP_CodeCoverage::setCheckForUnintentionallyCoveredCode
     */
    public function testSetCheckForUnintentionallyCoveredCode()
    {
        $this->coverage->setCheckForUnintentionallyCoveredCode(true);
        $this->assertAttributeEquals(
          true, 'checkForUnintentionallyCoveredCode', $this->coverage
        );
    }

    /**
     * @covers            PHP_CodeCoverage::setForceCoversAnnotation
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testSetForceCoversAnnotationThrowsExceptionForInvalidArgument()
    {
        $this->coverage->setForceCoversAnnotation(null);
    }

    /**
     * @covers PHP_CodeCoverage::setForceCoversAnnotation
     */
    public function testSetForceCoversAnnotation()
    {
        $this->coverage->setForceCoversAnnotation(true);
        $this->assertAttributeEquals(
          true, 'forceCoversAnnotation', $this->coverage
        );
    }

    /**
     * @covers            PHP_CodeCoverage::setAddUncoveredFilesFromWhitelist
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testSetAddUncoveredFilesFromWhitelistThrowsExceptionForInvalidArgument()
    {
        $this->coverage->setAddUncoveredFilesFromWhitelist(null);
    }

    /**
     * @covers PHP_CodeCoverage::setAddUncoveredFilesFromWhitelist
     */
    public function testSetAddUncoveredFilesFromWhitelist()
    {
        $this->coverage->setAddUncoveredFilesFromWhitelist(true);
        $this->assertAttributeEquals(
          true, 'addUncoveredFilesFromWhitelist', $this->coverage
        );
    }

    /**
     * @covers            PHP_CodeCoverage::setProcessUncoveredFilesFromWhitelist
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testSetProcessUncoveredFilesFromWhitelistThrowsExceptionForInvalidArgument()
    {
        $this->coverage->setProcessUncoveredFilesFromWhitelist(null);
    }

    /**
     * @covers PHP_CodeCoverage::setProcessUncoveredFilesFromWhitelist
     */
    public function testSetProcessUncoveredFilesFromWhitelist()
    {
        $this->coverage->setProcessUncoveredFilesFromWhitelist(true);
        $this->assertAttributeEquals(
          true, 'processUncoveredFilesFromWhitelist', $this->coverage
        );
    }

    /**
     * @covers PHP_CodeCoverage::setMapTestClassNameToCoveredClassName
     */
    public function testSetMapTestClassNameToCoveredClassName()
    {
        $this->coverage->setMapTestClassNameToCoveredClassName(true);
        $this->assertAttributeEquals(
          true, 'mapTestClassNameToCoveredClassName', $this->coverage
        );
    }

    /**
     * @covers            PHP_CodeCoverage::setMapTestClassNameToCoveredClassName
     * @expectedException PHP_CodeCoverage_Exception
     */
    public function testSetMapTestClassNameToCoveredClassNameThrowsExceptionForInvalidArgument()
    {
        $this->coverage->setMapTestClassNameToCoveredClassName(null);
    }

    /**
     * @covers PHP_CodeCoverage::clear
     */
    public function testClear()
    {
        $this->coverage->clear();

        $this->assertAttributeEquals(null, 'currentId', $this->coverage);
        $this->assertAttributeEquals(array(), 'data', $this->coverage);
        $this->assertAttributeEquals(array(), 'tests', $this->coverage);
    }

    /**
     * @covers PHP_CodeCoverage::start
     * @covers PHP_CodeCoverage::stop
     * @covers PHP_CodeCoverage::append
     * @covers PHP_CodeCoverage::applyListsFilter
     * @covers PHP_CodeCoverage::initializeFilesThatAreSeenTheFirstTime
     * @covers PHP_CodeCoverage::applyCoversAnnotationFilter
     * @covers PHP_CodeCoverage::getTests
     */
    public function testCollect()
    {
        $coverage = $this->getCoverageForBankAccount();

        $this->assertEquals(
          $this->getExpectedDataArrayForBankAccount(), $coverage->getData()
        );

        $this->assertEquals(
          array(
            'BankAccountTest::testBalanceIsInitiallyZero' => null,
            'BankAccountTest::testBalanceCannotBecomeNegative' => null,
            'BankAccountTest::testBalanceCannotBecomeNegative2' => null,
            'BankAccountTest::testDepositWithdrawMoney' => null
          ),
          $coverage->getTests()
        );
    }

    /**
     * @covers PHP_CodeCoverage::getData
     * @covers PHP_CodeCoverage::merge
     */
    public function testMerge()
    {
        $coverage = $this->getCoverageForBankAccountForFirstTwoTests();
        $coverage->merge($this->getCoverageForBankAccountForLastTwoTests());

        $this->assertEquals(
          $this->getExpectedDataArrayForBankAccount(), $coverage->getData()
        );
    }

    /**
     * @covers PHP_CodeCoverage::getData
     * @covers PHP_CodeCoverage::merge
     */
    public function testMerge2()
    {
        $coverage = new PHP_CodeCoverage(
          $this->getMock('PHP_CodeCoverage_Driver_Xdebug'),
          new PHP_CodeCoverage_Filter
        );

        $coverage->merge($this->getCoverageForBankAccount());

        $this->assertEquals(
          $this->getExpectedDataArrayForBankAccount(), $coverage->getData()
        );
    }

    /**
     * @covers PHP_CodeCoverage::getLinesToBeIgnored
     */
    public function testGetLinesToBeIgnored()
    {
        $this->assertEquals(
          array(
             1,
             3,
             4,
             5,
             7,
             8,
             9,
            10,
            11,
            12,
            13,
            14,
            15,
            16,
            17,
            18,
            19,
            20,
            21,
            22,
            23,
            24,
            25,
            26,
            27,
            28,
            30,
            32,
            33,
            34,
            35,
            36,
            37,
            38
          ),
          $this->getLinesToBeIgnored()->invoke(
            $this->coverage,
            TEST_FILES_PATH . 'source_with_ignore.php'
          )
        );
    }

    /**
     * @covers PHP_CodeCoverage::getLinesToBeIgnored
     */
    public function testGetLinesToBeIgnored2()
    {
        $this->assertEquals(
          array(1, 5),
          $this->getLinesToBeIgnored()->invoke(
            $this->coverage,
            TEST_FILES_PATH . 'source_without_ignore.php'
          )
        );
    }

    /**
     * @covers PHP_CodeCoverage::getLinesToBeIgnored
     */
    public function testGetLinesToBeIgnored3()
    {
        $this->assertEquals(
          array(
            1,
            2,
            3,
            4,
            5,
            8,
            11,
            15,
            16,
            19,
            20
          ),
          $this->getLinesToBeIgnored()->invoke(
            $this->coverage,
            TEST_FILES_PATH . 'source_with_class_and_anonymous_function.php'
          )
        );
    }

    /**
     * @covers PHP_CodeCoverage::getLinesToBeIgnored
     */
    public function testGetLinesToBeIgnoredOneLineAnnotations()
    {
        $this->assertEquals(
          array(
            1,
            2,
            3,
            4,
            5,
            6,
            7,
            8,
            9,
            10,
            11,
            12,
            13,
            14,
            15,
            16,
            18,
            20,
            21,
            23,
            24,
            25,
            27,
            28,
            29,
            30,
            31,
            32,
            33,
            34,
            37
          ),
          $this->getLinesToBeIgnored()->invoke(
            $this->coverage,
            TEST_FILES_PATH . 'source_with_oneline_annotations.php'
          )
        );
    }

    /**
     * @return ReflectionMethod
     */
    private function getLinesToBeIgnored()
    {
        $getLinesToBeIgnored = new ReflectionMethod(
            'PHP_CodeCoverage', 'getLinesToBeIgnored'
        );

        $getLinesToBeIgnored->setAccessible(true);

        return $getLinesToBeIgnored;
    }
}
