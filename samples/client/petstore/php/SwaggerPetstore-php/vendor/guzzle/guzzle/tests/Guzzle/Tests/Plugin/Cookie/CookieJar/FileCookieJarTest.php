<?php

namespace Guzzle\Tests\Plugin\Cookie\CookieJar;

use Guzzle\Plugin\Cookie\Cookie;
use Guzzle\Plugin\Cookie\CookieJar\FileCookieJar;

/**
 * @covers Guzzle\Plugin\Cookie\CookieJar\FileCookieJar
 */
class FileCookieJarTest extends \Guzzle\Tests\GuzzleTestCase
{
    private $file;

    public function setUp()
    {
        $this->file = tempnam('/tmp', 'file-cookies');
    }

    public function testLoadsFromFileFile()
    {
        $jar = new FileCookieJar($this->file);
        $this->assertEquals(array(), $jar->all());
        unlink($this->file);
    }

    public function testPersistsToFileFile()
    {
        $jar = new FileCookieJar($this->file);
        $jar->add(new Cookie(array(
            'name'    => 'foo',
            'value'   => 'bar',
            'domain'  => 'foo.com',
            'expires' => time() + 1000
        )));
        $jar->add(new Cookie(array(
            'name'    => 'baz',
            'value'   => 'bar',
            'domain'  => 'foo.com',
            'expires' => time() + 1000
        )));
        $jar->add(new Cookie(array(
            'name'    => 'boo',
            'value'   => 'bar',
            'domain'  => 'foo.com',
        )));

        $this->assertEquals(3, count($jar));
        unset($jar);

        // Make sure it wrote to the file
        $contents = file_get_contents($this->file);
        $this->assertNotEmpty($contents);

        // Load the cookieJar from the file
        $jar = new FileCookieJar($this->file);

        // Weeds out temporary and session cookies
        $this->assertEquals(2, count($jar));
        unset($jar);
        unlink($this->file);
    }
}
