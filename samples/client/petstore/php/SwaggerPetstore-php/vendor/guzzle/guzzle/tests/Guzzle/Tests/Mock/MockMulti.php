<?php

namespace Guzzle\Tests\Mock;

class MockMulti extends \Guzzle\Http\Curl\CurlMulti
{
    public function getHandle()
    {
        return $this->multiHandle;
    }
}
