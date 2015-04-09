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
 * Abstract base class for test case classes.
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
abstract class PHP_CodeCoverage_TestCase extends PHPUnit_Framework_TestCase
{
    protected function getXdebugDataForBankAccount()
    {
        return array(
          array(
            TEST_FILES_PATH . 'BankAccount.php' => array(
               8 =>  1,
               9 => -2,
              13 => -1,
              14 => -1,
              15 => -1,
              16 => -1,
              18 => -1,
              22 => -1,
              24 => -1,
              25 => -2,
              29 => -1,
              31 => -1,
              32 => -2
            )
          ),
          array(
            TEST_FILES_PATH . 'BankAccount.php' => array(
               8 => 1,
              13 => 1,
              16 => 1,
              29 => 1,
            )
          ),
          array(
            TEST_FILES_PATH . 'BankAccount.php' => array(
               8 => 1,
              13 => 1,
              16 => 1,
              22 => 1,
            )
          ),
          array(
            TEST_FILES_PATH . 'BankAccount.php' => array(
               8 => 1,
              13 => 1,
              14 => 1,
              15 => 1,
              18 => 1,
              22 => 1,
              24 => 1,
              29 => 1,
              31 => 1,
            )
          )
        );
    }

    protected function getCoverageForBankAccount()
    {
        $data = $this->getXdebugDataForBankAccount();

        $stub = $this->getMock('PHP_CodeCoverage_Driver_Xdebug');
        $stub->expects($this->any())
             ->method('stop')
             ->will($this->onConsecutiveCalls(
               $data[0], $data[1], $data[2], $data[3]
             ));

        $coverage = new PHP_CodeCoverage($stub, new PHP_CodeCoverage_Filter);

        $coverage->start(
          new BankAccountTest('testBalanceIsInitiallyZero'), true
        );

        $coverage->stop(
          true,
          array(TEST_FILES_PATH . 'BankAccount.php' => range(6, 9))
        );

        $coverage->start(
          new BankAccountTest('testBalanceCannotBecomeNegative')
        );

        $coverage->stop(
          true,
          array(TEST_FILES_PATH . 'BankAccount.php' => range(27, 32))
        );

        $coverage->start(
          new BankAccountTest('testBalanceCannotBecomeNegative2')
        );

        $coverage->stop(
          true,
          array(TEST_FILES_PATH . 'BankAccount.php' => range(20, 25))
        );

        $coverage->start(
          new BankAccountTest('testDepositWithdrawMoney')
        );

        $coverage->stop(
          true,
          array(
            TEST_FILES_PATH . 'BankAccount.php' => array_merge(
              range(6, 9), range(20, 25), range(27, 32)
            )
          )
        );

        return $coverage;
    }

    protected function getCoverageForBankAccountForFirstTwoTests()
    {
        $data = $this->getXdebugDataForBankAccount();

        $stub = $this->getMock('PHP_CodeCoverage_Driver_Xdebug');
        $stub->expects($this->any())
             ->method('stop')
             ->will($this->onConsecutiveCalls(
               $data[0], $data[1]
             ));

        $coverage = new PHP_CodeCoverage($stub, new PHP_CodeCoverage_Filter);

        $coverage->start(
          new BankAccountTest('testBalanceIsInitiallyZero'), true
        );

        $coverage->stop(
          true,
          array(TEST_FILES_PATH . 'BankAccount.php' => range(6, 9))
        );

        $coverage->start(
          new BankAccountTest('testBalanceCannotBecomeNegative')
        );

        $coverage->stop(
          true,
          array(TEST_FILES_PATH . 'BankAccount.php' => range(27, 32))
        );

        return $coverage;
    }

    protected function getCoverageForBankAccountForLastTwoTests()
    {
        $data = $this->getXdebugDataForBankAccount();

        $stub = $this->getMock('PHP_CodeCoverage_Driver_Xdebug');
        $stub->expects($this->any())
             ->method('stop')
             ->will($this->onConsecutiveCalls(
               $data[2], $data[3]
             ));

        $coverage = new PHP_CodeCoverage($stub, new PHP_CodeCoverage_Filter);

        $coverage->start(
          new BankAccountTest('testBalanceCannotBecomeNegative2')
        );

        $coverage->stop(
          true,
          array(TEST_FILES_PATH . 'BankAccount.php' => range(20, 25))
        );

        $coverage->start(
          new BankAccountTest('testDepositWithdrawMoney')
        );

        $coverage->stop(
          true,
          array(
            TEST_FILES_PATH . 'BankAccount.php' => array_merge(
              range(6, 9), range(20, 25), range(27, 32)
            )
          )
        );

        return $coverage;
    }

    protected function getExpectedDataArrayForBankAccount()
    {
        return array(
          TEST_FILES_PATH . 'BankAccount.php' => array(
            8 => array(
              0 => 'BankAccountTest::testBalanceIsInitiallyZero',
              1 => 'BankAccountTest::testDepositWithdrawMoney'
            ),
            9 => null,
            13 => array(),
            14 => array(),
            15 => array(),
            16 => array(),
            18 => array(),
            22 => array(
              0 => 'BankAccountTest::testBalanceCannotBecomeNegative2',
              1 => 'BankAccountTest::testDepositWithdrawMoney'
            ),
            24 => array(
              0 => 'BankAccountTest::testDepositWithdrawMoney',
            ),
            25 => null,
            29 => array(
              0 => 'BankAccountTest::testBalanceCannotBecomeNegative',
              1 => 'BankAccountTest::testDepositWithdrawMoney'
            ),
            31 => array(
              0 => 'BankAccountTest::testDepositWithdrawMoney'
            ),
            32 => null
          )
        );
    }

    protected function getCoverageForFileWithIgnoredLines()
    {
        $coverage = new PHP_CodeCoverage(
          $this->setUpXdebugStubForFileWithIgnoredLines(),
          new PHP_CodeCoverage_Filter
        );

        $coverage->start('FileWithIgnoredLines', true);
        $coverage->stop();

        return $coverage;
    }

    protected function setUpXdebugStubForFileWithIgnoredLines()
    {
        $stub = $this->getMock('PHP_CodeCoverage_Driver_Xdebug');
        $stub->expects($this->any())
             ->method('stop')
             ->will($this->returnValue(
               array(
                 TEST_FILES_PATH . 'source_with_ignore.php' => array(
                   2 => 1,
                   4 => -1,
                   6 => -1,
                   7 => 1
                 )
               )
            ));

        return $stub;
    }

    protected function getCoverageForClassWithAnonymousFunction()
    {
        $coverage = new PHP_CodeCoverage(
          $this->setUpXdebugStubForClassWithAnonymousFunction(),
          new PHP_CodeCoverage_Filter
        );

        $coverage->start('ClassWithAnonymousFunction', true);
        $coverage->stop();

        return $coverage;
    }

    protected function setUpXdebugStubForClassWithAnonymousFunction()
    {
        $stub = $this->getMock('PHP_CodeCoverage_Driver_Xdebug');
        $stub->expects($this->any())
             ->method('stop')
             ->will($this->returnValue(
               array(
                 TEST_FILES_PATH . 'source_with_class_and_anonymous_function.php' => array(
                    7  => 1,
                    9  => 1,
                    10 => -1,
                    11 => 1,
                    12 => 1,
                    13 => 1,
                    14 => 1,
                    17 => 1,
                    18 => 1
                 )
               )
            ));

        return $stub;
    }
}
