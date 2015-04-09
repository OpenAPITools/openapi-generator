<?php

/*
 * This file is part of the Symfony package.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Symfony\Component\Yaml\Tests;

use Symfony\Component\Yaml\Exception\ParseException;

class ParseExceptionTest extends \PHPUnit_Framework_TestCase
{
    public function testGetMessage()
    {
        $exception = new ParseException('Error message', 42, 'foo: bar', '/var/www/app/config.yml');
        if (PHP_VERSION_ID >= 50400) {
            $message = 'Error message in "/var/www/app/config.yml" at line 42 (near "foo: bar")';
        } else {
            $message = 'Error message in "\\/var\\/www\\/app\\/config.yml" at line 42 (near "foo: bar")';
        }

        $this->assertEquals($message, $exception->getMessage());
    }

    public function testGetMessageWithUnicodeInFilename()
    {
        $exception = new ParseException('Error message', 42, 'foo: bar', 'äöü.yml');
        if (PHP_VERSION_ID >= 50400) {
            $message = 'Error message in "äöü.yml" at line 42 (near "foo: bar")';
        } else {
            $message = 'Error message in "\u00e4\u00f6\u00fc.yml" at line 42 (near "foo: bar")';
        }

        $this->assertEquals($message, $exception->getMessage());
    }
}
