<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class IdenticalValueTokenSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith(42);
    }

    function it_is_initializable()
    {
        $this->shouldHaveType('Prophecy\Argument\Token\IdenticalValueToken');
    }

    function it_scores_11_if_string_value_is_identical_to_argument()
    {
        $this->beConstructedWith('foo');
        $this->scoreArgument('foo')->shouldReturn(11);
    }

    function it_scores_11_if_boolean_value_is_identical_to_argument()
    {
        $this->beConstructedWith(false);
        $this->scoreArgument(false)->shouldReturn(11);
    }

    function it_scores_11_if_integer_value_is_identical_to_argument()
    {
        $this->beConstructedWith(31);
        $this->scoreArgument(31)->shouldReturn(11);
    }

    function it_scores_11_if_float_value_is_identical_to_argument()
    {
        $this->beConstructedWith(31.12);
        $this->scoreArgument(31.12)->shouldReturn(11);
    }

    function it_scores_11_if_array_value_is_identical_to_argument()
    {
        $this->beConstructedWith(array('foo' => 'bar'));
        $this->scoreArgument(array('foo' => 'bar'))->shouldReturn(11);
    }

    function it_scores_11_if_object_value_is_identical_to_argument()
    {
        $object = new \stdClass();

        $this->beConstructedWith($object);
        $this->scoreArgument($object)->shouldReturn(11);
    }

    function it_scores_false_if_value_is_not_identical_to_argument()
    {
        $this->beConstructedWith(new \stdClass());
        $this->scoreArgument('foo')->shouldReturn(false);
    }

    function it_scores_false_if_object_value_is_not_the_same_instance_than_argument()
    {
        $this->beConstructedWith(new \stdClass());
        $this->scoreArgument(new \stdClass())->shouldReturn(false);
    }

    function it_scores_false_if_integer_value_is_not_identical_to_boolean_argument()
    {
        $this->beConstructedWith(1);
        $this->scoreArgument(true)->shouldReturn(false);
    }

    function it_is_not_last()
    {
        $this->shouldNotBeLast();
    }

    function it_generates_proper_string_representation_for_integer()
    {
        $this->beConstructedWith(42);
        $this->__toString()->shouldReturn('identical(42)');
    }

    function it_generates_proper_string_representation_for_string()
    {
        $this->beConstructedWith('some string');
        $this->__toString()->shouldReturn('identical("some string")');
    }

    function it_generates_single_line_representation_for_multiline_string()
    {
        $this->beConstructedWith("some\nstring");
        $this->__toString()->shouldReturn('identical("some\\nstring")');
    }

    function it_generates_proper_string_representation_for_double()
    {
        $this->beConstructedWith(42.3);
        $this->__toString()->shouldReturn('identical(42.3)');
    }

    function it_generates_proper_string_representation_for_boolean_true()
    {
        $this->beConstructedWith(true);
        $this->__toString()->shouldReturn('identical(true)');
    }

    function it_generates_proper_string_representation_for_boolean_false()
    {
        $this->beConstructedWith(false);
        $this->__toString()->shouldReturn('identical(false)');
    }

    function it_generates_proper_string_representation_for_null()
    {
        $this->beConstructedWith(null);
        $this->__toString()->shouldReturn('identical(null)');
    }

    function it_generates_proper_string_representation_for_empty_array()
    {
        $this->beConstructedWith(array());
        $this->__toString()->shouldReturn('identical([])');
    }

    function it_generates_proper_string_representation_for_array()
    {
        $this->beConstructedWith(array('zet', 42));
        $this->__toString()->shouldReturn('identical(["zet", 42])');
    }

    function it_generates_proper_string_representation_for_resource()
    {
        $resource = fopen(__FILE__, 'r');
        $this->beConstructedWith($resource);
        $this->__toString()->shouldReturn('identical(stream:'.$resource.')');
    }

    function it_generates_proper_string_representation_for_object($object)
    {
        $objHash = sprintf('%s:%s',
            get_class($object->getWrappedObject()),
            spl_object_hash($object->getWrappedObject())
        );

        $this->beConstructedWith($object);
        $this->__toString()->shouldReturn("identical($objHash Object (\n    'objectProphecy' => Prophecy\Prophecy\ObjectProphecy Object (*Prophecy*)\n))");
    }
}
