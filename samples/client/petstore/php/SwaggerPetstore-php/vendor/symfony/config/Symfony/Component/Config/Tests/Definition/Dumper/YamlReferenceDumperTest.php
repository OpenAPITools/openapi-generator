<?php

/*
 * This file is part of the Symfony package.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Symfony\Component\Config\Tests\Definition\Dumper;

use Symfony\Component\Config\Definition\Dumper\YamlReferenceDumper;
use Symfony\Component\Config\Tests\Fixtures\Configuration\ExampleConfiguration;

class YamlReferenceDumperTest extends \PHPUnit_Framework_TestCase
{
    public function testDumper()
    {
        $configuration = new ExampleConfiguration();

        $dumper = new YamlReferenceDumper();

        $this->markTestIncomplete('The Yaml Dumper currently does not support prototyped arrays');
        $this->assertEquals($this->getConfigurationAsString(), $dumper->dump($configuration));
    }

    private function getConfigurationAsString()
    {
        return <<<EOL
acme_root:
    boolean:              true
    scalar_empty:         ~
    scalar_null:          ~
    scalar_true:          true
    scalar_false:         false
    scalar_default:       default
    scalar_array_empty:   []
    scalar_array_defaults:

        # Defaults:
        - elem1
        - elem2
    scalar_required:      ~ # Required
    enum:                 ~ # One of "this"; "that"

    # some info
    array:
        child1:               ~
        child2:               ~

        # this is a long
        # multi-line info text
        # which should be indented
        child3:               ~ # Example: example setting
    parameters:

        # Prototype
        name:                 ~
    connections:
        # Prototype
        - { user: ~, pass: ~ }

EOL;
    }
}
