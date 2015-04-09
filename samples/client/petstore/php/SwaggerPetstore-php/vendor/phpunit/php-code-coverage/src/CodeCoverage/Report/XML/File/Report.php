<?php
/*
 * This file is part of the PHP_CodeCoverage package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * @category   PHP
 * @package    CodeCoverage
 * @author     Arne Blankerts <arne@blankerts.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/php-code-coverage
 * @since      Class available since Release 2.0.0
 */
class PHP_CodeCoverage_Report_XML_File_Report extends PHP_CodeCoverage_Report_XML_File
{
    public function __construct($name)
    {
        $this->dom = new DOMDocument;
        $this->dom->loadXML('<?xml version="1.0" ?><phpunit xmlns="http://schema.phpunit.de/coverage/1.0"><file /></phpunit>');

        $this->contextNode = $this->dom->getElementsByTagNameNS(
            'http://schema.phpunit.de/coverage/1.0', 'file'
        )->item(0);

        $this->setName($name);
    }

    private function setName($name)
    {
        $this->contextNode->setAttribute('name', $name);
    }

    public function asDom()
    {
        return $this->dom;
    }

    public function getFunctionObject($name)
    {
        $node = $this->contextNode->appendChild(
            $this->dom->createElementNS(
                'http://schema.phpunit.de/coverage/1.0', 'function'
            )
        );

        return new PHP_CodeCoverage_Report_XML_File_Method($node, $name);
    }

    public function getClassObject($name)
    {
        return $this->getUnitObject('class', $name);
    }

    public function getTraitObject($name)
    {
        return $this->getUnitObject('trait', $name);
    }

    private function getUnitObject($tagName, $name)
    {
        $node = $this->contextNode->appendChild(
            $this->dom->createElementNS(
                'http://schema.phpunit.de/coverage/1.0', $tagName
            )
        );

        return new PHP_CodeCoverage_Report_XML_File_Unit($node, $name);
    }
}
