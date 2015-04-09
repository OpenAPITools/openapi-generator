<?php

namespace Guzzle\Tests\Service\Command;

use Guzzle\Http\Message\RequestFactory;
use Guzzle\Service\Command\ClosureCommand;
use Guzzle\Service\Client;

/**
 * @covers Guzzle\Service\Command\ClosureCommand
 */
class ClosureCommandTest extends \Guzzle\Tests\GuzzleTestCase
{
    /**
     * @expectedException InvalidArgumentException
     * @expectedExceptionMessage A closure must be passed in the parameters array
     */
    public function testConstructorValidatesClosure()
    {
        $c = new ClosureCommand();
    }

    public function testExecutesClosure()
    {
        $c = new ClosureCommand(array(
            'closure' => function($command, $api) {
                $command->set('testing', '123');
                $request = RequestFactory::getInstance()->create('GET', 'http://www.test.com/');
                return $request;
            }
        ));

        $client = $this->getServiceBuilder()->get('mock');
        $c->setClient($client)->prepare();
        $this->assertEquals('123', $c->get('testing'));
        $this->assertEquals('http://www.test.com/', $c->getRequest()->getUrl());
    }

    /**
     * @expectedException UnexpectedValueException
     * @expectedExceptionMessage Closure command did not return a RequestInterface object
     */
    public function testMustReturnRequest()
    {
        $c = new ClosureCommand(array(
            'closure' => function($command, $api) {
                return false;
            }
        ));

        $client = $this->getServiceBuilder()->get('mock');
        $c->setClient($client)->prepare();
    }
}
