<?php

namespace Guzzle\Tests\Plugin\Cookie\CookieJar;

use Guzzle\Plugin\Cookie\Cookie;
use Guzzle\Plugin\Cookie\CookieJar\ArrayCookieJar;
use Guzzle\Http\Message\Response;
use Guzzle\Http\Message\Request;

/**
 * @covers Guzzle\Plugin\Cookie\CookieJar\ArrayCookieJar
 */
class ArrayCookieJarTest extends \Guzzle\Tests\GuzzleTestCase
{
    /**
     * @var ArrayCookieJar
     */
    private $jar;

    public function setUp()
    {
        $this->jar = new ArrayCookieJar();
    }

    protected function getTestCookies()
    {
        return array(
            new Cookie(array('name' => 'foo',  'value' => 'bar', 'domain' => 'foo.com', 'path' => '/',    'discard' => true)),
            new Cookie(array('name' => 'test', 'value' => '123', 'domain' => 'baz.com', 'path' => '/foo', 'expires' => 2)),
            new Cookie(array('name' => 'you',  'value' => '123', 'domain' => 'bar.com', 'path' => '/boo', 'expires' => time() + 1000))
        );
    }

    /**
     * Provides test data for cookie cookieJar retrieval
     */
    public function getCookiesDataProvider()
    {
        return array(
            array(array('foo', 'baz', 'test', 'muppet', 'googoo'), '', '', '', false),
            array(array('foo', 'baz', 'muppet', 'googoo'), '', '', '', true),
            array(array('googoo'), 'www.example.com', '', '', false),
            array(array('muppet', 'googoo'), 'test.y.example.com', '', '', false),
            array(array('foo', 'baz'), 'example.com', '', '', false),
            array(array('muppet'), 'x.y.example.com', '/acme/', '', false),
            array(array('muppet'), 'x.y.example.com', '/acme/test/', '', false),
            array(array('googoo'), 'x.y.example.com', '/test/acme/test/', '', false),
            array(array('foo', 'baz'), 'example.com', '', '', false),
            array(array('baz'), 'example.com', '', 'baz', false),
        );
    }

    public function testStoresAndRetrievesCookies()
    {
        $cookies = $this->getTestCookies();
        foreach ($cookies as $cookie) {
            $this->assertTrue($this->jar->add($cookie));
        }

        $this->assertEquals(3, count($this->jar));
        $this->assertEquals(3, count($this->jar->getIterator()));
        $this->assertEquals($cookies, $this->jar->all(null, null, null, false, false));
    }

    public function testRemovesExpiredCookies()
    {
        $cookies = $this->getTestCookies();
        foreach ($this->getTestCookies() as $cookie) {
            $this->jar->add($cookie);
        }
        $this->jar->removeExpired();
        $this->assertEquals(array($cookies[0], $cookies[2]), $this->jar->all());
    }

    public function testRemovesTemporaryCookies()
    {
        $cookies = $this->getTestCookies();
        foreach ($this->getTestCookies() as $cookie) {
            $this->jar->add($cookie);
        }
        $this->jar->removeTemporary();
        $this->assertEquals(array($cookies[2]), $this->jar->all());
    }

    public function testIsSerializable()
    {
        $this->assertEquals('[]', $this->jar->serialize());
        $this->jar->unserialize('[]');
        $this->assertEquals(array(), $this->jar->all());

        $cookies = $this->getTestCookies();
        foreach ($this->getTestCookies() as $cookie) {
            $this->jar->add($cookie);
        }

        // Remove discard and expired cookies
        $serialized = $this->jar->serialize();
        $data = json_decode($serialized, true);
        $this->assertEquals(1, count($data));

        $a = new ArrayCookieJar();
        $a->unserialize($serialized);
        $this->assertEquals(1, count($a));
    }

    public function testRemovesSelectively()
    {
        $cookies = $this->getTestCookies();
        foreach ($this->getTestCookies() as $cookie) {
            $this->jar->add($cookie);
        }

        // Remove foo.com cookies
        $this->jar->remove('foo.com');
        $this->assertEquals(2, count($this->jar));
        // Try again, removing no further cookies
        $this->jar->remove('foo.com');
        $this->assertEquals(2, count($this->jar));

        // Remove bar.com cookies with path of /boo
        $this->jar->remove('bar.com', '/boo');
        $this->assertEquals(1, count($this->jar));

        // Remove cookie by name
        $this->jar->remove(null, null, 'test');
        $this->assertEquals(0, count($this->jar));
    }

    public function testDoesNotAddIncompleteCookies()
    {
        $this->assertEquals(false, $this->jar->add(new Cookie()));
        $this->assertFalse($this->jar->add(new Cookie(array(
            'name' => 'foo'
        ))));
        $this->assertFalse($this->jar->add(new Cookie(array(
            'name' => false
        ))));
        $this->assertFalse($this->jar->add(new Cookie(array(
            'name' => true
        ))));
        $this->assertFalse($this->jar->add(new Cookie(array(
            'name'   => 'foo',
            'domain' => 'foo.com'
        ))));
    }

    public function testDoesAddValidCookies()
    {
        $this->assertTrue($this->jar->add(new Cookie(array(
            'name'   => 'foo',
            'domain' => 'foo.com',
            'value'  => 0
        ))));
        $this->assertTrue($this->jar->add(new Cookie(array(
            'name'   => 'foo',
            'domain' => 'foo.com',
            'value'  => 0.0
        ))));
        $this->assertTrue($this->jar->add(new Cookie(array(
            'name'   => 'foo',
            'domain' => 'foo.com',
            'value'  => '0'
        ))));
    }

    public function testOverwritesCookiesThatAreOlderOrDiscardable()
    {
        $t = time() + 1000;
        $data = array(
            'name'    => 'foo',
            'value'   => 'bar',
            'domain'  => '.example.com',
            'path'    => '/',
            'max_age' => '86400',
            'port'    => array(80, 8080),
            'version' => '1',
            'secure'  => true,
            'discard' => true,
            'expires' => $t
        );

        // Make sure that the discard cookie is overridden with the non-discard
        $this->assertTrue($this->jar->add(new Cookie($data)));

        unset($data['discard']);
        $this->assertTrue($this->jar->add(new Cookie($data)));
        $this->assertEquals(1, count($this->jar));

        $c = $this->jar->all();
        $this->assertEquals(false, $c[0]->getDiscard());

        // Make sure it doesn't duplicate the cookie
        $this->jar->add(new Cookie($data));
        $this->assertEquals(1, count($this->jar));

        // Make sure the more future-ful expiration date supersede the other
        $data['expires'] = time() + 2000;
        $this->assertTrue($this->jar->add(new Cookie($data)));
        $this->assertEquals(1, count($this->jar));
        $c = $this->jar->all();
        $this->assertNotEquals($t, $c[0]->getExpires());
    }

    public function testOverwritesCookiesThatHaveChanged()
    {
        $t = time() + 1000;
        $data = array(
            'name'    => 'foo',
            'value'   => 'bar',
            'domain'  => '.example.com',
            'path'    => '/',
            'max_age' => '86400',
            'port'    => array(80, 8080),
            'version' => '1',
            'secure'  => true,
            'discard' => true,
            'expires' => $t
        );

        // Make sure that the discard cookie is overridden with the non-discard
        $this->assertTrue($this->jar->add(new Cookie($data)));

        $data['value'] = 'boo';
        $this->assertTrue($this->jar->add(new Cookie($data)));
        $this->assertEquals(1, count($this->jar));

        // Changing the value plus a parameter also must overwrite the existing one
        $data['value'] = 'zoo';
        $data['secure'] = false;
        $this->assertTrue($this->jar->add(new Cookie($data)));
        $this->assertEquals(1, count($this->jar));

        $c = $this->jar->all();
        $this->assertEquals('zoo', $c[0]->getValue());
    }

    public function testAddsCookiesFromResponseWithNoRequest()
    {
        $response = new Response(200, array(
            'Set-Cookie' => array(
                "fpc=d=.Hm.yh4.1XmJWjJfs4orLQzKzPImxklQoxXSHOZATHUSEFciRueW_7704iYUtsXNEXq0M92Px2glMdWypmJ7HIQl6XIUvrZimWjQ3vIdeuRbI.FNQMAfcxu_XN1zSx7l.AcPdKL6guHc2V7hIQFhnjRW0rxm2oHY1P4bGQxFNz7f.tHm12ZD3DbdMDiDy7TBXsuP4DM-&v=2; expires=Fri, 02-Mar-2019 02:17:40 GMT; path=/; domain=127.0.0.1",
                "FPCK3=AgBNbvoQAGpGEABZLRAAbFsQAF1tEABkDhAAeO0=; expires=Sat, 02-Apr-2019 02:17:40 GMT; path=/; domain=127.0.0.1",
                "CH=deleted; expires=Wed, 03-Mar-2010 02:17:39 GMT; path=/; domain=127.0.0.1",
                "CH=AgBNbvoQAAEcEAApuhAAMJcQADQvEAAvGxAALe0QAD6uEAATwhAAC1AQAC8t; expires=Sat, 02-Apr-2019 02:17:40 GMT; path=/; domain=127.0.0.1"
            )
        ));

        $this->jar->addCookiesFromResponse($response);
        $this->assertEquals(3, count($this->jar));
        $this->assertEquals(1, count($this->jar->all(null, null, 'fpc')));
        $this->assertEquals(1, count($this->jar->all(null, null, 'FPCK3')));
        $this->assertEquals(1, count($this->jar->all(null, null, 'CH')));
    }

    public function testAddsCookiesFromResponseWithRequest()
    {
        $response = new Response(200, array(
            'Set-Cookie' => "fpc=d=.Hm.yh4.1XmJWjJfs4orLQzKzPImxklQoxXSHOZATHUSEFciRueW_7704iYUtsXNEXq0M92Px2glMdWypmJ7HIQl6XIUvrZimWjQ3vIdeuRbI.FNQMAfcxu_XN1zSx7l.AcPdKL6guHc2V7hIQFhnjRW0rxm2oHY1P4bGQxFNz7f.tHm12ZD3DbdMDiDy7TBXsuP4DM-&v=2; expires=Fri, 02-Mar-2019 02:17:40 GMT;"
        ));
        $request = new Request('GET', 'http://www.example.com');
        $this->jar->addCookiesFromResponse($response, $request);
        $this->assertEquals(1, count($this->jar));
    }

    public function getMatchingCookiesDataProvider()
    {
        return array(
            array('https://example.com', array(0)),
            array('http://example.com', array()),
            array('https://example.com:8912', array()),
            array('https://foo.example.com', array(0)),
            array('http://foo.example.com/test/acme/', array(4))
        );
    }

    /**
     * @dataProvider getMatchingCookiesDataProvider
     */
    public function testReturnsCookiesMatchingRequests($url, $cookies)
    {
        $bag = array(
            new Cookie(array(
                'name'    => 'foo',
                'value'   => 'bar',
                'domain'  => 'example.com',
                'path'    => '/',
                'max_age' => '86400',
                'port'    => array(443, 8080),
                'version' => '1',
                'secure'  => true
            )),
            new Cookie(array(
                'name'    => 'baz',
                'value'   => 'foobar',
                'domain'  => 'example.com',
                'path'    => '/',
                'max_age' => '86400',
                'port'    => array(80, 8080),
                'version' => '1',
                'secure'  => true
            )),
            new Cookie(array(
                'name'    => 'test',
                'value'   => '123',
                'domain'  => 'www.foobar.com',
                'path'    => '/path/',
                'discard' => true
            )),
            new Cookie(array(
                'name'    => 'muppet',
                'value'   => 'cookie_monster',
                'domain'  => '.y.example.com',
                'path'    => '/acme/',
                'comment' => 'Comment goes here...',
                'expires' => time() + 86400
            )),
            new Cookie(array(
                'name'    => 'googoo',
                'value'   => 'gaga',
                'domain'  => '.example.com',
                'path'    => '/test/acme/',
                'max_age' => 1500,
                'version' => 2
            ))
        );

        foreach ($bag as $cookie) {
            $this->jar->add($cookie);
        }

        $request = new Request('GET', $url);
        $results = $this->jar->getMatchingCookies($request);
        $this->assertEquals(count($cookies), count($results));
        foreach ($cookies as $i) {
            $this->assertContains($bag[$i], $results);
        }
    }

    /**
     * @expectedException \Guzzle\Plugin\Cookie\Exception\InvalidCookieException
     * @expectedExceptionMessage The cookie name must not contain invalid characters: abc:@123
     */
    public function testThrowsExceptionWithStrictMode()
    {
        $a = new ArrayCookieJar();
        $a->setStrictMode(true);
        $a->add(new Cookie(array(
            'name'   => 'abc:@123',
            'value'  => 'foo',
            'domain' => 'bar'
        )));
    }

    public function testRemoveExistingCookieIfEmpty()
    {
        // Add a cookie that should not be affected
        $a = new Cookie(array(
            'name' => 'foo',
            'value' => 'nope',
            'domain' => 'foo.com',
            'path' => '/abc'
        ));
        $this->jar->add($a);

        $data = array(
            'name' => 'foo',
            'value' => 'bar',
            'domain' => 'foo.com',
            'path' => '/'
        );

        $b = new Cookie($data);
        $this->assertTrue($this->jar->add($b));
        $this->assertEquals(2, count($this->jar));

        // Try to re-set the same cookie with no value: assert that cookie is not added
        $data['value'] = null;
        $this->assertFalse($this->jar->add(new Cookie($data)));
        // assert that original cookie has been deleted
        $cookies = $this->jar->all('foo.com');
        $this->assertTrue(in_array($a, $cookies, true));
        $this->assertFalse(in_array($b, $cookies, true));
        $this->assertEquals(1, count($this->jar));
    }
}
