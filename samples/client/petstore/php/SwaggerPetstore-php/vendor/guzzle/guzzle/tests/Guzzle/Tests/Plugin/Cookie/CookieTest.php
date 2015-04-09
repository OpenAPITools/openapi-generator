<?php

namespace Guzzle\Tests\Plugin\Cookie;

use Guzzle\Plugin\Cookie\Cookie;

/**
 * @covers Guzzle\Plugin\Cookie\Cookie
 */
class CookieTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testInitializesDefaultValues()
    {
        $cookie = new Cookie();
        $this->assertEquals('/', $cookie->getPath());
        $this->assertEquals(array(), $cookie->getPorts());
    }

    public function testConvertsDateTimeMaxAgeToUnixTimestamp()
    {
        $cookie = new Cookie(array(
           'expires' => 'November 20, 1984'
        ));
        $this->assertTrue(is_numeric($cookie->getExpires()));
    }

    public function testAddsExpiresBasedOnMaxAge()
    {
        $t = time();
        $cookie = new Cookie(array(
            'max_age' => 100
        ));
        $this->assertEquals($t + 100, $cookie->getExpires());
    }

    public function testHoldsValues()
    {
        $t = time();
        $data = array(
            'name'        => 'foo',
            'value'       => 'baz',
            'path'        => '/bar',
            'domain'      => 'baz.com',
            'expires'     => $t,
            'max_age'     => 100,
            'comment'     => 'Hi',
            'comment_url' => 'foo.com',
            'port'        => array(1, 2),
            'version'     => 2,
            'secure'      => true,
            'discard'     => true,
            'http_only'   => true,
            'data'        => array(
                'foo' => 'baz',
                'bar' => 'bam'
            )
        );

        $cookie = new Cookie($data);
        $this->assertEquals($data, $cookie->toArray());

        $this->assertEquals('foo', $cookie->getName());
        $this->assertEquals('baz', $cookie->getValue());
        $this->assertEquals('baz.com', $cookie->getDomain());
        $this->assertEquals('/bar', $cookie->getPath());
        $this->assertEquals($t, $cookie->getExpires());
        $this->assertEquals(100, $cookie->getMaxAge());
        $this->assertEquals('Hi', $cookie->getComment());
        $this->assertEquals('foo.com', $cookie->getCommentUrl());
        $this->assertEquals(array(1, 2), $cookie->getPorts());
        $this->assertEquals(2, $cookie->getVersion());
        $this->assertTrue($cookie->getSecure());
        $this->assertTrue($cookie->getDiscard());
        $this->assertTrue($cookie->getHttpOnly());
        $this->assertEquals('baz', $cookie->getAttribute('foo'));
        $this->assertEquals('bam', $cookie->getAttribute('bar'));
        $this->assertEquals(array(
            'foo' => 'baz',
            'bar' => 'bam'
        ), $cookie->getAttributes());

        $cookie->setName('a')
            ->setValue('b')
            ->setPath('c')
            ->setDomain('bar.com')
            ->setExpires(10)
            ->setMaxAge(200)
            ->setComment('e')
            ->setCommentUrl('f')
            ->setPorts(array(80))
            ->setVersion(3)
            ->setSecure(false)
            ->setHttpOnly(false)
            ->setDiscard(false)
            ->setAttribute('snoop', 'dog');

        $this->assertEquals('a', $cookie->getName());
        $this->assertEquals('b', $cookie->getValue());
        $this->assertEquals('c', $cookie->getPath());
        $this->assertEquals('bar.com', $cookie->getDomain());
        $this->assertEquals(10, $cookie->getExpires());
        $this->assertEquals(200, $cookie->getMaxAge());
        $this->assertEquals('e', $cookie->getComment());
        $this->assertEquals('f', $cookie->getCommentUrl());
        $this->assertEquals(array(80), $cookie->getPorts());
        $this->assertEquals(3, $cookie->getVersion());
        $this->assertFalse($cookie->getSecure());
        $this->assertFalse($cookie->getDiscard());
        $this->assertFalse($cookie->getHttpOnly());
        $this->assertEquals('dog', $cookie->getAttribute('snoop'));
    }

    public function testDeterminesIfExpired()
    {
        $c = new Cookie();
        $c->setExpires(10);
        $this->assertTrue($c->isExpired());
        $c->setExpires(time() + 10000);
        $this->assertFalse($c->isExpired());
    }

    public function testMatchesPorts()
    {
        $cookie = new Cookie();
        // Always matches when nothing is set
        $this->assertTrue($cookie->matchesPort(2));

        $cookie->setPorts(array(1, 2));
        $this->assertTrue($cookie->matchesPort(2));
        $this->assertFalse($cookie->matchesPort(100));
    }

    public function testMatchesDomain()
    {
        $cookie = new Cookie();
        $this->assertTrue($cookie->matchesDomain('baz.com'));

        $cookie->setDomain('baz.com');
        $this->assertTrue($cookie->matchesDomain('baz.com'));
        $this->assertFalse($cookie->matchesDomain('bar.com'));

        $cookie->setDomain('.baz.com');
        $this->assertTrue($cookie->matchesDomain('.baz.com'));
        $this->assertTrue($cookie->matchesDomain('foo.baz.com'));
        $this->assertFalse($cookie->matchesDomain('baz.bar.com'));
        $this->assertTrue($cookie->matchesDomain('baz.com'));

        $cookie->setDomain('.127.0.0.1');
        $this->assertTrue($cookie->matchesDomain('127.0.0.1'));

        $cookie->setDomain('127.0.0.1');
        $this->assertTrue($cookie->matchesDomain('127.0.0.1'));

        $cookie->setDomain('.com.');
        $this->assertFalse($cookie->matchesDomain('baz.com'));

        $cookie->setDomain('.local');
        $this->assertTrue($cookie->matchesDomain('example.local'));
    }

    public function testMatchesPath()
    {
        $cookie = new Cookie();
        $this->assertTrue($cookie->matchesPath('/foo'));

        $cookie->setPath('/foo');

        // o  The cookie-path and the request-path are identical.
        $this->assertTrue($cookie->matchesPath('/foo'));
        $this->assertFalse($cookie->matchesPath('/bar'));

        // o  The cookie-path is a prefix of the request-path, and the first
        // character of the request-path that is not included in the cookie-
        // path is a %x2F ("/") character.
        $this->assertTrue($cookie->matchesPath('/foo/bar'));
        $this->assertFalse($cookie->matchesPath('/fooBar'));

        // o  The cookie-path is a prefix of the request-path, and the last
        // character of the cookie-path is %x2F ("/").
        $cookie->setPath('/foo/');
        $this->assertTrue($cookie->matchesPath('/foo/bar'));
        $this->assertFalse($cookie->matchesPath('/fooBaz'));
        $this->assertFalse($cookie->matchesPath('/foo'));

    }

    public function cookieValidateProvider()
    {
        return array(
            array('foo', 'baz', 'bar', true),
            array('0', '0', '0', true),
            array('', 'baz', 'bar', 'The cookie name must not be empty'),
            array('foo', '', 'bar', 'The cookie value must not be empty'),
            array('foo', 'baz', '', 'The cookie domain must not be empty'),
            array('foo\\', 'baz', '0', 'The cookie name must not contain invalid characters: foo\\'),
        );
    }

    /**
     * @dataProvider cookieValidateProvider
     */
    public function testValidatesCookies($name, $value, $domain, $result)
    {
        $cookie = new Cookie(array(
            'name'   => $name,
            'value'  => $value,
            'domain' => $domain
        ));
        $this->assertSame($result, $cookie->validate());
    }

    public function testCreatesInvalidCharacterString()
    {
        $m = new \ReflectionMethod('Guzzle\Plugin\Cookie\Cookie', 'getInvalidCharacters');
        $m->setAccessible(true);
        $p = new \ReflectionProperty('Guzzle\Plugin\Cookie\Cookie', 'invalidCharString');
        $p->setAccessible(true);
        $p->setValue('');
        // Expects a string containing 51 invalid characters
        $this->assertEquals(51, strlen($m->invoke($m)));
        $this->assertContains('@', $m->invoke($m));
    }
}
