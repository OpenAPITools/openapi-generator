<?php

namespace Guzzle\Tests\Service\Exception;

use Guzzle\Http\Exception\MultiTransferException;
use Guzzle\Http\Message\Request;
use Guzzle\Service\Exception\CommandTransferException;
use Guzzle\Tests\Service\Mock\Command\MockCommand;

/**
 * @covers Guzzle\Service\Exception\CommandTransferException
 */
class CommandTransferExceptionTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testStoresCommands()
    {
        $c1 = new MockCommand();
        $c2 = new MockCommand();
        $e = new CommandTransferException('Test');
        $e->addSuccessfulCommand($c1)->addFailedCommand($c2);
        $this->assertSame(array($c1), $e->getSuccessfulCommands());
        $this->assertSame(array($c2), $e->getFailedCommands());
        $this->assertSame(array($c1, $c2), $e->getAllCommands());
    }

    public function testConvertsMultiExceptionIntoCommandTransfer()
    {
        $r1 = new Request('GET', 'http://foo.com');
        $r2 = new Request('GET', 'http://foobaz.com');
        $e = new MultiTransferException('Test', 123);
        $e->addSuccessfulRequest($r1)->addFailedRequest($r2);
        $ce = CommandTransferException::fromMultiTransferException($e);

        $this->assertInstanceOf('Guzzle\Service\Exception\CommandTransferException', $ce);
        $this->assertEquals('Test', $ce->getMessage());
        $this->assertEquals(123, $ce->getCode());
        $this->assertSame(array($r1), $ce->getSuccessfulRequests());
        $this->assertSame(array($r2), $ce->getFailedRequests());
    }

    public function testCanRetrieveExceptionForCommand()
    {
        $r1 = new Request('GET', 'http://foo.com');
        $e1 = new \Exception('foo');
        $c1 = $this->getMockBuilder('Guzzle\Tests\Service\Mock\Command\MockCommand')
            ->setMethods(array('getRequest'))
            ->getMock();
        $c1->expects($this->once())->method('getRequest')->will($this->returnValue($r1));

        $e = new MultiTransferException('Test', 123);
        $e->addFailedRequestWithException($r1, $e1);
        $ce = CommandTransferException::fromMultiTransferException($e);
        $ce->addFailedCommand($c1);

        $this->assertSame($e1, $ce->getExceptionForFailedCommand($c1));
    }

    public function testAddsNonRequestExceptions()
    {
        $e = new MultiTransferException();
        $e->add(new \Exception('bar'));
        $e->addFailedRequestWithException(new Request('GET', 'http://www.foo.com'), new \Exception('foo'));
        $ce = CommandTransferException::fromMultiTransferException($e);
        $this->assertEquals(2, count($ce));
    }
}
