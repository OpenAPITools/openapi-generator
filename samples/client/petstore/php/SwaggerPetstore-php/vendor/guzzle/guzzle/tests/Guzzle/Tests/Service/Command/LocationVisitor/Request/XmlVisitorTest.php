<?php

namespace Guzzle\Tests\Service\Command\LocationVisitor\Request;

use Guzzle\Service\Command\LocationVisitor\Request\XmlVisitor;
use Guzzle\Service\Client;
use Guzzle\Service\Description\Parameter;
use Guzzle\Service\Description\Operation;
use Guzzle\Http\Message\EntityEnclosingRequest;

/**
 * @covers Guzzle\Service\Command\LocationVisitor\Request\XmlVisitor
 */
class XmlVisitorTest extends AbstractVisitorTestCase
{
    public function xmlProvider()
    {
        return array(
            array(
                array(
                    'data' => array(
                        'xmlRoot' => array(
                            'name'       => 'test',
                            'namespaces' => 'http://foo.com'
                        )
                    ),
                    'parameters' => array(
                        'Foo' => array('location' => 'xml', 'type' => 'string'),
                        'Baz' => array('location' => 'xml', 'type' => 'string')
                    )
                ),
                array('Foo' => 'test', 'Baz' => 'bar'),
                '<test xmlns="http://foo.com"><Foo>test</Foo><Baz>bar</Baz></test>'
            ),
            // Ensure that the content-type is not added
            array(array('parameters' => array('Foo' => array('location' => 'xml', 'type' => 'string'))), array(), ''),
            // Test with adding attributes and no namespace
            array(
                array(
                    'data' => array(
                        'xmlRoot' => array(
                            'name' => 'test'
                        )
                    ),
                    'parameters' => array(
                        'Foo' => array('location' => 'xml', 'type' => 'string', 'data' => array('xmlAttribute' => true))
                    )
                ),
                array('Foo' => 'test', 'Baz' => 'bar'),
                '<test Foo="test"/>'
            ),
            // Test adding with an array
            array(
                array(
                    'parameters' => array(
                        'Foo' => array('location' => 'xml', 'type' => 'string'),
                        'Baz' => array(
                            'type'     => 'array',
                            'location' => 'xml',
                            'items' => array(
                                'type'   => 'numeric',
                                'sentAs' => 'Bar'
                            )
                        )
                    )
                ),
                array('Foo' => 'test', 'Baz' => array(1, 2)),
                '<Request><Foo>test</Foo><Baz><Bar>1</Bar><Bar>2</Bar></Baz></Request>'
            ),
            // Test adding an object
            array(
                array(
                    'parameters' => array(
                        'Foo' => array('location' => 'xml', 'type' => 'string'),
                        'Baz' => array(
                            'type'     => 'object',
                            'location' => 'xml',
                            'properties' => array(
                                'Bar' => array('type' => 'string'),
                                'Bam' => array()
                            )
                        )
                    )
                ),
                array('Foo' => 'test', 'Baz' => array('Bar' => 'abc', 'Bam' => 'foo')),
                '<Request><Foo>test</Foo><Baz><Bar>abc</Bar><Bam>foo</Bam></Baz></Request>'
            ),
            // Add an array that contains an object
            array(
                array(
                    'parameters' => array(
                        'Baz' => array(
                            'type'     => 'array',
                            'location' => 'xml',
                            'items' => array(
                                'type'       => 'object',
                                'sentAs'     => 'Bar',
                                'properties' => array('A' => array(), 'B' => array())
                            )
                        )
                    )
                ),
                array('Baz' => array(
                    array('A' => '1', 'B' => '2'),
                    array('A' => '3', 'B' => '4')
                )),
                '<Request><Baz><Bar><A>1</A><B>2</B></Bar><Bar><A>3</A><B>4</B></Bar></Baz></Request>'
            ),
            // Add an object of attributes
            array(
                array(
                    'parameters' => array(
                        'Foo' => array('location' => 'xml', 'type' => 'string'),
                        'Baz' => array(
                            'type'     => 'object',
                            'location' => 'xml',
                            'properties' => array(
                                'Bar' => array('type' => 'string', 'data' => array('xmlAttribute' => true)),
                                'Bam' => array()
                            )
                        )
                    )
                ),
                array('Foo' => 'test', 'Baz' => array('Bar' => 'abc', 'Bam' => 'foo')),
                '<Request><Foo>test</Foo><Baz Bar="abc"><Bam>foo</Bam></Baz></Request>'
            ),
            // Check order doesn't matter
            array(
                array(
                    'parameters' => array(
                        'Foo' => array('location' => 'xml', 'type' => 'string'),
                        'Baz' => array(
                            'type'     => 'object',
                            'location' => 'xml',
                            'properties' => array(
                                'Bar' => array('type' => 'string', 'data' => array('xmlAttribute' => true)),
                                'Bam' => array()
                            )
                        )
                    )
                ),
                array('Foo' => 'test', 'Baz' => array('Bam' => 'foo', 'Bar' => 'abc')),
                '<Request><Foo>test</Foo><Baz Bar="abc"><Bam>foo</Bam></Baz></Request>'
            ),
            // Add values with custom namespaces
            array(
                array(
                    'parameters' => array(
                        'Foo' => array(
                            'location' => 'xml',
                            'type' => 'string',
                            'data' => array(
                                'xmlNamespace' => 'http://foo.com'
                            )
                        )
                    )
                ),
                array('Foo' => 'test'),
                '<Request><Foo xmlns="http://foo.com">test</Foo></Request>'
            ),
            // Add attributes with custom namespace prefix
            array(
                array(
                    'parameters' => array(
                        'Wrap' => array(
                            'type' => 'object',
                            'location' => 'xml',
                            'properties' => array(
                                'Foo' => array(
                                    'type' => 'string',
                                    'sentAs' => 'xsi:baz',
                                    'data' => array(
                                        'xmlNamespace' => 'http://foo.com',
                                        'xmlAttribute' => true
                                    )
                                )
                            )
                        ),
                    )
                ),
                array('Wrap' => array(
                    'Foo' => 'test'
                )),
                '<Request><Wrap xsi:baz="test" xmlns:xsi="http://foo.com"/></Request>'
            ),
            // Add nodes with custom namespace prefix
            array(
                array(
                    'parameters' => array(
                        'Wrap' => array(
                            'type' => 'object',
                            'location' => 'xml',
                            'properties' => array(
                                'Foo' => array(
                                    'type' => 'string',
                                    'sentAs' => 'xsi:Foo',
                                    'data' => array(
                                        'xmlNamespace' => 'http://foobar.com'
                                    )
                                )
                            )
                        ),
                    )
                ),
                array('Wrap' => array(
                    'Foo' => 'test'
                )),
                '<Request><Wrap><xsi:Foo xmlns:xsi="http://foobar.com">test</xsi:Foo></Wrap></Request>'
            ),
            array(
                array(
                    'parameters' => array(
                        'Foo' => array(
                            'location' => 'xml',
                            'type' => 'string',
                            'data' => array(
                                'xmlNamespace' => 'http://foo.com'
                            )
                        )
                    )
                ),
                array('Foo' => '<h1>This is a title</h1>'),
                '<Request><Foo xmlns="http://foo.com"><![CDATA[<h1>This is a title</h1>]]></Foo></Request>'
            ),
            // Flat array at top level
            array(
                array(
                    'parameters' => array(
                        'Bars' => array(
                            'type'     => 'array',
                            'data'     => array('xmlFlattened' => true),
                            'location' => 'xml',
                            'items' => array(
                                'type'       => 'object',
                                'sentAs'     => 'Bar',
                                'properties' => array(
                                    'A' => array(),
                                    'B' => array()
                                )
                            )
                        ),
                        'Boos' => array(
                            'type'     => 'array',
                            'data'     => array('xmlFlattened' => true),
                            'location' => 'xml',
                            'items'  => array(
                                'sentAs' => 'Boo',
                                'type' => 'string'
                            )
                        )
                    )
                ),
                array(
                    'Bars' => array(
                        array('A' => '1', 'B' => '2'),
                        array('A' => '3', 'B' => '4')
                    ),
                    'Boos' => array('test', '123')
                ),
                '<Request><Bar><A>1</A><B>2</B></Bar><Bar><A>3</A><B>4</B></Bar><Boo>test</Boo><Boo>123</Boo></Request>'
            ),
            // Nested flat arrays
            array(
                array(
                    'parameters' => array(
                        'Delete' => array(
                            'type'     => 'object',
                            'location' => 'xml',
                            'properties' => array(
                                'Items' => array(
                                    'type' => 'array',
                                    'data' => array('xmlFlattened' => true),
                                    'items' => array(
                                        'type'       => 'object',
                                        'sentAs'     => 'Item',
                                        'properties' => array(
                                            'A' => array(),
                                            'B' => array()
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
                array(
                    'Delete' => array(
                        'Items' => array(
                            array('A' => '1', 'B' => '2'),
                            array('A' => '3', 'B' => '4')
                        )
                    )
                ),
                '<Request><Delete><Item><A>1</A><B>2</B></Item><Item><A>3</A><B>4</B></Item></Delete></Request>'
            )
        );
    }

    /**
     * @dataProvider xmlProvider
     */
    public function testSerializesXml(array $operation, array $input, $xml)
    {
        $operation = new Operation($operation);
        $command = $this->getMockBuilder('Guzzle\Service\Command\OperationCommand')
            ->setConstructorArgs(array($input, $operation))
            ->getMockForAbstractClass();
        $command->setClient(new Client('http://www.test.com/some/path.php'));
        $request = $command->prepare();
        if (!empty($input)) {
            $this->assertEquals('application/xml', (string) $request->getHeader('Content-Type'));
        } else {
            $this->assertNull($request->getHeader('Content-Type'));
        }
        $body = str_replace(array("\n", "<?xml version=\"1.0\"?>"), '', (string) $request->getBody());
        $this->assertEquals($xml, $body);
    }

    public function testAddsContentTypeAndTopLevelValues()
    {
        $operation = new Operation(array(
            'data' => array(
                'xmlRoot'      => array(
                    'name' => 'test',
                    'namespaces' => array(
                        'xsi' => 'http://foo.com'
                    )
                )
            ),
            'parameters' => array(
                'Foo' => array('location' => 'xml', 'type' => 'string'),
                'Baz' => array('location' => 'xml', 'type' => 'string')
            )
        ));

        $command = $this->getMockBuilder('Guzzle\Service\Command\OperationCommand')
            ->setConstructorArgs(array(array(
                'Foo' => 'test',
                'Baz' => 'bar'
            ), $operation))
            ->getMockForAbstractClass();

        $command->setClient(new Client());
        $request = $command->prepare();
        $this->assertEquals('application/xml', (string) $request->getHeader('Content-Type'));
        $this->assertEquals(
            '<?xml version="1.0"?>' . "\n"
            . '<test xmlns:xsi="http://foo.com"><Foo>test</Foo><Baz>bar</Baz></test>' . "\n",
            (string) $request->getBody()
        );
    }

    public function testCanChangeContentType()
    {
        $visitor = new XmlVisitor();
        $visitor->setContentTypeHeader('application/foo');
        $this->assertEquals('application/foo', $this->readAttribute($visitor, 'contentType'));
    }

    public function testCanAddArrayOfSimpleTypes()
    {
        $request = new EntityEnclosingRequest('POST', 'http://foo.com');
        $visitor = new XmlVisitor();
        $param = new Parameter(array(
            'type'     => 'object',
            'location' => 'xml',
            'name'     => 'Out',
            'properties' => array(
                'Nodes' => array(
                    'required' => true,
                    'type'     => 'array',
                    'min'      => 1,
                    'items'    => array('type' => 'string', 'sentAs' => 'Node')
                )
            )
        ));

        $param->setParent(new Operation(array(
            'data' => array(
                'xmlRoot' => array(
                    'name' => 'Test',
                    'namespaces' => array(
                        'https://foo/'
                    )
                )
            )
        )));

        $value = array('Nodes' => array('foo', 'baz'));
        $this->assertTrue($this->validator->validate($param, $value));
        $visitor->visit($this->command, $request, $param, $value);
        $visitor->after($this->command, $request);

        $this->assertEquals(
            "<?xml version=\"1.0\"?>\n"
            . "<Test xmlns=\"https://foo/\"><Out><Nodes><Node>foo</Node><Node>baz</Node></Nodes></Out></Test>\n",
            (string) $request->getBody()
        );
    }

    public function testCanAddMultipleNamespacesToRoot()
    {
        $operation = new Operation(array(
            'data' => array(
                'xmlRoot' => array(
                    'name' => 'Hi',
                    'namespaces' => array(
                        'xsi' => 'http://foo.com',
                        'foo' => 'http://foobar.com'
                    )
                )
            ),
            'parameters' => array(
                'Foo' => array('location' => 'xml', 'type' => 'string')
            )
        ));

        $command = $this->getMockBuilder('Guzzle\Service\Command\OperationCommand')
            ->setConstructorArgs(array(array(
                'Foo' => 'test'
            ), $operation))
            ->getMockForAbstractClass();

        $command->setClient(new Client());
        $request = $command->prepare();
        $this->assertEquals('application/xml', (string) $request->getHeader('Content-Type'));
        $this->assertEquals(
            '<?xml version="1.0"?>' . "\n"
            . '<Hi xmlns:xsi="http://foo.com" xmlns:foo="http://foobar.com"><Foo>test</Foo></Hi>' . "\n",
            (string) $request->getBody()
        );
    }

    public function testValuesAreFiltered()
    {
        $operation = new Operation(array(
            'parameters' => array(
                'Foo' => array(
                    'location' => 'xml',
                    'type'     => 'string',
                    'filters'  => array('strtoupper')
                ),
                'Bar' => array(
                    'location' => 'xml',
                    'type'     => 'object',
                    'properties' => array(
                        'Baz' => array(
                            'filters'  => array('strtoupper')
                        )
                    )
                )
            )
        ));

        $command = $this->getMockBuilder('Guzzle\Service\Command\OperationCommand')
            ->setConstructorArgs(array(array(
                'Foo' => 'test',
                'Bar' => array(
                    'Baz' => 'abc'
                )
            ), $operation))
            ->getMockForAbstractClass();

        $command->setClient(new Client());
        $request = $command->prepare();
        $this->assertEquals(
            '<?xml version="1.0"?>' . "\n"
            . '<Request><Foo>TEST</Foo><Bar><Baz>ABC</Baz></Bar></Request>' . "\n",
            (string) $request->getBody()
        );
    }

    public function testSkipsNullValues()
    {
        $operation = new Operation(array(
            'parameters' => array(
                'Foo' => array(
                    'location' => 'xml',
                    'type'     => 'string'
                ),
                'Bar' => array(
                    'location' => 'xml',
                    'type'     => 'object',
                    'properties' => array(
                        'Baz' => array(),
                        'Bam' => array(),
                    )
                ),
                'Arr' => array(
                    'type'  => 'array',
                    'items' => array(
                        'type' => 'string'
                    )
                )
            )
        ));

        $command = $this->getMockBuilder('Guzzle\Service\Command\OperationCommand')
            ->setConstructorArgs(array(array(
                'Foo' => null,
                'Bar' => array(
                    'Bar' => null,
                    'Bam' => 'test'
                ),
                'Arr' => array(null)
            ), $operation))
            ->getMockForAbstractClass();

        $command->setClient(new Client());
        $request = $command->prepare();
        $this->assertEquals(
            '<?xml version="1.0"?>' . "\n"
            . '<Request><Bar><Bam>test</Bam></Bar></Request>' . "\n",
            (string) $request->getBody()
        );
    }

    public function testAllowsXmlEncoding()
    {
        $operation = new Operation(array(
            'data' => array(
                'xmlEncoding' => 'UTF-8'
            ),
            'parameters' => array(
                'Foo' => array('location' => 'xml')
            )
        ));
        $command = $this->getMockBuilder('Guzzle\Service\Command\OperationCommand')
            ->setConstructorArgs(array(array('Foo' => 'test'), $operation))
            ->getMockForAbstractClass();
        $command->setClient(new Client());
        $request = $command->prepare();
        $this->assertEquals(
            '<?xml version="1.0" encoding="UTF-8"?>' . "\n"
                . '<Request><Foo>test</Foo></Request>' . "\n",
            (string) $request->getBody()
        );
    }

    public function testAllowsSendingXmlPayloadIfNoXmlParamsWereSet()
    {
        $operation = new Operation(array(
            'httpMethod' => 'POST',
            'data' => array('xmlAllowEmpty' => true),
            'parameters' => array('Foo' => array('location' => 'xml'))
        ));
        $command = $this->getMockBuilder('Guzzle\Service\Command\OperationCommand')
            ->setConstructorArgs(array(array(), $operation))
            ->getMockForAbstractClass();
        $command->setClient(new Client('http://foo.com'));
        $request = $command->prepare();
        $this->assertEquals(
            '<?xml version="1.0"?>' . "\n"
            . '<Request/>' . "\n",
            (string) $request->getBody()
        );
    }
}
