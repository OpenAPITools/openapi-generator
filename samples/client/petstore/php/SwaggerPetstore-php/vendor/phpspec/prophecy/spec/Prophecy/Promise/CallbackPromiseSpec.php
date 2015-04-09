<?php

namespace spec\Prophecy\Promise;

use PhpSpec\ObjectBehavior;

class CallbackPromiseSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('get_class');
    }

    function it_is_promise()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Promise\PromiseInterface');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_should_execute_closure_callback($object, $method)
    {
        $firstArgumentCallback = function ($args) {
            return $args[0];
        };

        $this->beConstructedWith($firstArgumentCallback);

        $this->execute(array('one', 'two'), $object, $method)->shouldReturn('one');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_should_execute_static_array_callback($object, $method)
    {
        $firstArgumentCallback = array('spec\Prophecy\Promise\ClassCallback', 'staticCallbackMethod');

        $this->beConstructedWith($firstArgumentCallback);

        $this->execute(array('one', 'two'), $object, $method)->shouldReturn('one');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_should_execute_instance_array_callback($object, $method)
    {
        $class = new ClassCallback();
        $firstArgumentCallback = array($class, 'callbackMethod');

        $this->beConstructedWith($firstArgumentCallback);

        $this->execute(array('one', 'two'), $object, $method)->shouldReturn('one');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_should_execute_string_function_callback($object, $method)
    {
        $firstArgumentCallback = 'spec\Prophecy\Promise\functionCallbackFirstArgument';

        $this->beConstructedWith($firstArgumentCallback);

        $this->execute(array('one', 'two'), $object, $method)->shouldReturn('one');
    }

}

/**
 * Class used to test callbackpromise
 *
 * @param array
 * @return string
 */
class ClassCallback
{
    /**
     * @param array $args
     */
    function callbackMethod($args)
    {
        return $args[0];
    }

    /**
     * @param array $args
     */
    static function staticCallbackMethod($args)
    {
        return $args[0];
    }
}

/**
 * Callback function used to test callbackpromise
 *
 * @param array
 * @return string
 */
function functionCallbackFirstArgument($args)
{
    return $args[0];
}
