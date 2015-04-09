<?php

namespace Guzzle\Tests\Common;

use Guzzle\Common\Version;

/**
 * @covers Guzzle\Common\Version
 */
class VersionTest extends \Guzzle\Tests\GuzzleTestCase
{
    /**
     * @expectedException \PHPUnit_Framework_Error_Deprecated
     */
    public function testEmitsWarnings()
    {
        Version::$emitWarnings = true;
        Version::warn('testing!');
    }

    public function testCanSilenceWarnings()
    {
        Version::$emitWarnings = false;
        Version::warn('testing!');
        Version::$emitWarnings = true;
    }
}
