<?php

namespace Guzzle\Tests\Service\Command\LocationVisitor\Request;

use Guzzle\Service\Client;
use Guzzle\Service\Description\ServiceDescription;
use Guzzle\Http\Message\PostFile;
use Guzzle\Service\Command\LocationVisitor\Request\PostFileVisitor as Visitor;

/**
 * @covers Guzzle\Service\Command\LocationVisitor\Request\PostFileVisitor
 */
class PostFileVisitorTest extends AbstractVisitorTestCase
{
    public function testVisitsLocation()
    {
        $visitor = new Visitor();
        $param = $this->getNestedCommand('postFile')->getParam('foo');

        // Test using a path to a file
        $visitor->visit($this->command, $this->request, $param->setSentAs('test_3'), __FILE__);
        $this->assertInternalType('array', $this->request->getPostFile('test_3'));

        // Test with a PostFile
        $visitor->visit($this->command, $this->request, $param->setSentAs(null), new PostFile('baz', __FILE__));
        $this->assertInternalType('array', $this->request->getPostFile('baz'));
    }

    public function testVisitsLocationWithMultipleFiles()
    {
        $description = ServiceDescription::factory(array(
            'operations' => array(
                'DoPost' => array(
                    'httpMethod' => 'POST',
                    'parameters' => array(
                        'foo' => array(
                            'location' => 'postFile',
                            'type' => array('string', 'array')
                        )
                    )
                )
            )
        ));
        $this->getServer()->flush();
        $this->getServer()->enqueue(array("HTTP/1.1 200 OK\r\nContent-Length:0\r\n\r\n"));
        $client = new Client($this->getServer()->getUrl());
        $client->setDescription($description);
        $command = $client->getCommand('DoPost', array('foo' => array(__FILE__, __FILE__)));
        $command->execute();
        $received = $this->getServer()->getReceivedRequests();
        $this->assertContains('name="foo[0]";', $received[0]);
        $this->assertContains('name="foo[1]";', $received[0]);
    }
}
