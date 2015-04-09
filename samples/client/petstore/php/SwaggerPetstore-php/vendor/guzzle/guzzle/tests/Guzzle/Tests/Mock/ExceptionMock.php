<?php

namespace Guzzle\Tests\Mock;

class ExceptionMock
{
    public function __construct()
    {
        throw new \Exception('Oh no!');
    }
}
