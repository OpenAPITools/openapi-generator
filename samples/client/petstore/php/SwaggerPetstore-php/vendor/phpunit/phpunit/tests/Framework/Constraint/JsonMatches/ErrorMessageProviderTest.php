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
 * @package    PHPUnit
 * @author     Bastian Feder <php@bastian-feder.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause
 * @link       http://www.phpunit.de/
 * @since      File available since Release 3.7.0
 */
class Framework_Constraint_JsonMatches_ErrorMessageProviderTest extends PHPUnit_Framework_TestCase
{
    /**
     * @dataProvider translateTypeToPrefixDataprovider
     * @covers PHPUnit_Framework_Constraint_JsonMatches_ErrorMessageProvider::translateTypeToPrefix
     */
    public function testTranslateTypeToPrefix($expected, $type)
    {
        $this->assertEquals(
            $expected,
            PHPUnit_Framework_Constraint_JsonMatches_ErrorMessageProvider::translateTypeToPrefix($type)
        );
    }

    /**
     * @dataProvider determineJsonErrorDataprovider
     * @covers PHPUnit_Framework_Constraint_JsonMatches_ErrorMessageProvider::determineJsonError
     */
    public function testDetermineJsonError($expected, $error, $prefix)
    {
        $this->assertEquals(
            $expected,
            PHPUnit_Framework_Constraint_JsonMatches_ErrorMessageProvider::determineJsonError(
                $error,
                $prefix
            )
        );
    }

    public static function determineJsonErrorDataprovider()
    {
        return array(
            'JSON_ERROR_NONE'  => array(
                null, 'json_error_none', ''
            ),
            'JSON_ERROR_DEPTH' => array(
                'Maximum stack depth exceeded', JSON_ERROR_DEPTH, ''
            ),
            'prefixed JSON_ERROR_DEPTH' => array(
                'TUX: Maximum stack depth exceeded', JSON_ERROR_DEPTH, 'TUX: '
            ),
            'JSON_ERROR_STATE_MISMatch' => array(
                'Underflow or the modes mismatch', JSON_ERROR_STATE_MISMATCH, ''
            ),
            'JSON_ERROR_CTRL_CHAR' => array(
                'Unexpected control character found', JSON_ERROR_CTRL_CHAR, ''
            ),
            'JSON_ERROR_SYNTAX' => array(
                'Syntax error, malformed JSON', JSON_ERROR_SYNTAX, ''
            ),
            'JSON_ERROR_UTF8`' => array(
                'Malformed UTF-8 characters, possibly incorrectly encoded',
                JSON_ERROR_UTF8,
                ''
            ),
            'Invalid error indicator' => array(
                'Unknown error', 55, ''
            ),
        );
    }

    public static function translateTypeToPrefixDataprovider()
    {
        return array(
            'expected' => array('Expected value JSON decode error - ', 'expected'),
            'actual' => array('Actual value JSON decode error - ', 'actual'),
            'default' => array('', ''),
        );
    }
}
