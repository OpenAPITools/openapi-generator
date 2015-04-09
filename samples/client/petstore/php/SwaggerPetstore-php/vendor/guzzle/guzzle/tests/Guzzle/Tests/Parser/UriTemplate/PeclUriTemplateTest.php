<?php

namespace Guzzle\Tests\Parsers\UriTemplate;

use Guzzle\Parser\UriTemplate\PeclUriTemplate;

/**
 * @covers Guzzle\Parser\UriTemplate\PeclUriTemplate
 */
class PeclUriTemplateTest extends AbstractUriTemplateTest
{
    protected function setUp()
    {
        if (!extension_loaded('uri_template')) {
            $this->markTestSkipped('uri_template PECL extension must be installed to test PeclUriTemplate');
        }
    }

    /**
     * @dataProvider templateProvider
     */
    public function testExpandsUriTemplates($template, $expansion, $params)
    {
        $uri = new PeclUriTemplate($template);
        $this->assertEquals($expansion, $uri->expand($template, $params));
    }
}
