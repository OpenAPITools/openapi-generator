<?php

namespace spec\Prophecy\Exception\Doubler;

use PhpSpec\ObjectBehavior;

class DoubleExceptionSpec extends ObjectBehavior
{
    function it_is_a_double_exception()
    {
        $this->shouldBeAnInstanceOf('RuntimeException');
        $this->shouldBeAnInstanceOf('Prophecy\Exception\Doubler\DoublerException');
    }
}
