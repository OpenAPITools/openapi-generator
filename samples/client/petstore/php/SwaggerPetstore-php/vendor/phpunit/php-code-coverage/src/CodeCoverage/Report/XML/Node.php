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
class PHP_CodeCoverage_Report_XML_Node
{
    /**
     * @var DOMDocument
     */
    private $dom;

    /**
     * @var DOMElement
     */
    private $contextNode;

    public function __construct(DOMElement $context)
    {
        $this->setContextNode($context);
    }

    protected function setContextNode(DOMElement $context)
    {
        $this->dom         = $context->ownerDocument;
        $this->contextNode = $context;
    }

    public function getDom()
    {
        return $this->dom;
    }

    protected function getContextNode()
    {
        return $this->contextNode;
    }

    public function getTotals()
    {
        $totalsContainer = $this->getContextNode()->firstChild;

        if (!$totalsContainer) {
            $totalsContainer = $this->getContextNode()->appendChild(
                $this->dom->createElementNS(
                    'http://schema.phpunit.de/coverage/1.0', 'totals'
                )
            );
        }

        return new PHP_CodeCoverage_Report_XML_Totals($totalsContainer);
    }

    public function addDirectory($name)
    {
        $dirNode = $this->getDom()->createElementNS(
            'http://schema.phpunit.de/coverage/1.0', 'directory'
        );

        $dirNode->setAttribute('name', $name);
        $this->getContextNode()->appendChild($dirNode);

        return new PHP_CodeCoverage_Report_XML_Directory($dirNode);
    }

    public function addFile($name, $href)
    {
        $fileNode = $this->getDom()->createElementNS(
            'http://schema.phpunit.de/coverage/1.0', 'file'
        );

        $fileNode->setAttribute('name', $name);
        $fileNode->setAttribute('href', $href);
        $this->getContextNode()->appendChild($fileNode);

        return new PHP_CodeCoverage_Report_XML_File($fileNode);
    }
}
