<?php

namespace spec\Prophecy\Exception\Doubler;

use PhpSpec\ObjectBehavior;
use spec\Prophecy\Exception\Prophecy;

class InterfaceNotFoundExceptionSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('msg', 'CustomInterface');
    }

    function it_extends_ClassNotFoundException()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Exception\Doubler\ClassNotFoundException');
    }

    function its_getClassname_returns_classname()
    {
        $this->getClassname()->shouldReturn('CustomInterface');
    }
}
