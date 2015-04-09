<?php
/*
 * This file is part of the Diff package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace SebastianBergmann\Diff;

use PHPUnit_Framework_TestCase;

class ParserTest extends PHPUnit_Framework_TestCase
{
    /**
     * @var Parser
     */
    private $parser;

    protected function setUp()
    {
        $this->parser = new Parser;
    }

    public function testParse()
    {
        $content = file_get_contents(__DIR__ . '/fixtures/patch.txt');

        $diffs = $this->parser->parse($content);

        $this->assertCount(1, $diffs);

        $chunks = $diffs[0]->getChunks();
        $this->assertCount(1, $chunks);

        $this->assertEquals(20, $chunks[0]->getStart());

        $this->assertCount(5, $chunks[0]->getLines());
    }

    public function testParseWithMultipleChunks()
    {
        $content = file_get_contents(__DIR__ . '/fixtures/patch2.txt');

        $diffs = $this->parser->parse($content);

        $this->assertCount(1, $diffs);

        $chunks = $diffs[0]->getChunks();
        $this->assertCount(3, $chunks);

        $this->assertEquals(20, $chunks[0]->getStart());
        $this->assertEquals(320, $chunks[1]->getStart());
        $this->assertEquals(600, $chunks[2]->getStart());

        $this->assertCount(5, $chunks[0]->getLines());
        $this->assertCount(5, $chunks[1]->getLines());
        $this->assertCount(5, $chunks[2]->getLines());
    }
}
