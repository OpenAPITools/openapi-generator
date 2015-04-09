<?php

namespace spec\Prophecy\Exception\Doubler;

use PhpSpec\ObjectBehavior;
use spec\Prophecy\Exception\Prophecy;

class ClassNotFoundExceptionSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('msg', 'CustomClass');
    }

    function it_is_a_prophecy_exception()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Exception\Exception');
        $this->shouldBeAnInstanceOf('Prophecy\Exception\Doubler\DoubleException');
    }

    function its_getClassname_returns_classname()
    {
        $this->getClassname()->shouldReturn('CustomClass');
    }
}
