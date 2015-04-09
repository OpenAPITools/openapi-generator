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
class PHP_CodeCoverage_Report_XML_File_Method
{
    /**
     * @var DOMElement
     */
    private $contextNode;

    public function __construct(DOMElement $context, $name)
    {
        $this->contextNode = $context;

        $this->setName($name);
    }

    private function setName($name)
    {
        $this->contextNode->setAttribute('name', $name);
    }

    public function setSignature($signature)
    {
        $this->contextNode->setAttribute('signature', $signature);
    }

    public function setLines($start, $end = null)
    {
        $this->contextNode->setAttribute('start', $start);

        if ($end !== null) {
            $this->contextNode->setAttribute('end', $end);
        }
    }

    public function setTotals($executable, $executed, $coverage)
    {
        $this->contextNode->setAttribute('executable', $executable);
        $this->contextNode->setAttribute('executed', $executed);
        $this->contextNode->setAttribute('coverage', $coverage);
    }

    public function setCrap($crap)
    {
        $this->contextNode->setAttribute('crap', $crap);
    }
}
