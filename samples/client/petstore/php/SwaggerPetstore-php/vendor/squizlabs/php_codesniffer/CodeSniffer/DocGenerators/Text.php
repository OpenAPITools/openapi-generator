<?php
/**
 * A doc generator that outputs text-based documentation.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Marc McIntyre <mmcintyre@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

if (class_exists('PHP_CodeSniffer_DocGenerators_Generator', true) === false) {
    throw new PHP_CodeSniffer_Exception('Class PHP_CodeSniffer_DocGenerators_Generator not found');
}

/**
 * A doc generator that outputs text-based documentation.
 *
 * Output is designed to be displayed in a terminal and is wrapped to 100 characters.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Marc McIntyre <mmcintyre@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_DocGenerators_Text extends PHP_CodeSniffer_DocGenerators_Generator
{


    /**
     * Process the documentation for a single sniff.
     *
     * @param DOMNode $doc The DOMNode object for the sniff.
     *                     It represents the "documentation" tag in the XML
     *                     standard file.
     *
     * @return void
     */
    public function processSniff(DOMNode $doc)
    {
        $this->printTitle($doc);

        foreach ($doc->childNodes as $node) {
            if ($node->nodeName === 'standard') {
                $this->printTextBlock($node);
            } else if ($node->nodeName === 'code_comparison') {
                $this->printCodeComparisonBlock($node);
            }
        }

    }//end processSniff()


    /**
     * Prints the title area for a single sniff.
     *
     * @param DOMNode $doc The DOMNode object for the sniff.
     *                     It represents the "documentation" tag in the XML
     *                     standard file.
     *
     * @return void
     */
    protected function printTitle(DOMNode $doc)
    {
        $title    = $this->getTitle($doc);
        $standard = $this->getStandard();

        echo PHP_EOL;
        echo str_repeat('-', (strlen("$standard CODING STANDARD: $title") + 4));
        echo strtoupper(PHP_EOL."| $standard CODING STANDARD: $title |".PHP_EOL);
        echo str_repeat('-', (strlen("$standard CODING STANDARD: $title") + 4));
        echo PHP_EOL.PHP_EOL;

    }//end printTitle()


    /**
     * Print a text block found in a standard.
     *
     * @param DOMNode $node The DOMNode object for the text block.
     *
     * @return void
     */
    protected function printTextBlock($node)
    {
        $text = trim($node->nodeValue);
        $text = str_replace('<em>', '*', $text);
        $text = str_replace('</em>', '*', $text);

        $lines    = array();
        $tempLine = '';
        $words    = explode(' ', $text);

        foreach ($words as $word) {
            if (strlen($tempLine.$word) >= 99) {
                if (strlen($tempLine.$word) === 99) {
                    // Adding the extra space will push us to the edge
                    // so we are done.
                    $lines[]  = $tempLine.$word;
                    $tempLine = '';
                } else if (strlen($tempLine.$word) === 100) {
                    // We are already at the edge, so we are done.
                    $lines[]  = $tempLine.$word;
                    $tempLine = '';
                } else {
                    $lines[]  = rtrim($tempLine);
                    $tempLine = $word.' ';
                }
            } else {
                $tempLine .= $word.' ';
            }
        }//end foreach

        if ($tempLine !== '') {
            $lines[] = rtrim($tempLine);
        }

        echo implode(PHP_EOL, $lines).PHP_EOL.PHP_EOL;

    }//end printTextBlock()


    /**
     * Print a code comparison block found in a standard.
     *
     * @param DOMNode $node The DOMNode object for the code comparison block.
     *
     * @return void
     */
    protected function printCodeComparisonBlock($node)
    {
        $codeBlocks = $node->getElementsByTagName('code');
        $first      = trim($codeBlocks->item(0)->nodeValue);
        $firstTitle = $codeBlocks->item(0)->getAttribute('title');

        $firstTitleLines = array();
        $tempTitle       = '';
        $words           = explode(' ', $firstTitle);

        foreach ($words as $word) {
            if (strlen($tempTitle.$word) >= 45) {
                if (strlen($tempTitle.$word) === 45) {
                    // Adding the extra space will push us to the edge
                    // so we are done.
                    $firstTitleLines[] = $tempTitle.$word;
                    $tempTitle         = '';
                } else if (strlen($tempTitle.$word) === 46) {
                    // We are already at the edge, so we are done.
                    $firstTitleLines[] = $tempTitle.$word;
                    $tempTitle         = '';
                } else {
                    $firstTitleLines[] = $tempTitle;
                    $tempTitle         = $word;
                }
            } else {
                $tempTitle .= $word.' ';
            }
        }//end foreach

        if ($tempTitle !== '') {
            $firstTitleLines[] = $tempTitle;
        }

        $first      = str_replace('<em>', '', $first);
        $first      = str_replace('</em>', '', $first);
        $firstLines = explode("\n", $first);

        $second      = trim($codeBlocks->item(1)->nodeValue);
        $secondTitle = $codeBlocks->item(1)->getAttribute('title');

        $secondTitleLines = array();
        $tempTitle        = '';
        $words            = explode(' ', $secondTitle);

        foreach ($words as $word) {
            if (strlen($tempTitle.$word) >= 45) {
                if (strlen($tempTitle.$word) === 45) {
                    // Adding the extra space will push us to the edge
                    // so we are done.
                    $secondTitleLines[] = $tempTitle.$word;
                    $tempTitle          = '';
                } else if (strlen($tempTitle.$word) === 46) {
                    // We are already at the edge, so we are done.
                    $secondTitleLines[] = $tempTitle.$word;
                    $tempTitle          = '';
                } else {
                    $secondTitleLines[] = $tempTitle;
                    $tempTitle          = $word;
                }
            } else {
                $tempTitle .= $word.' ';
            }
        }//end foreach

        if ($tempTitle !== '') {
            $secondTitleLines[] = $tempTitle;
        }

        $second      = str_replace('<em>', '', $second);
        $second      = str_replace('</em>', '', $second);
        $secondLines = explode("\n", $second);

        $maxCodeLines  = max(count($firstLines), count($secondLines));
        $maxTitleLines = max(count($firstTitleLines), count($secondTitleLines));

        echo str_repeat('-', 41);
        echo ' CODE COMPARISON ';
        echo str_repeat('-', 42).PHP_EOL;

        for ($i = 0; $i < $maxTitleLines; $i++) {
            if (isset($firstTitleLines[$i]) === true) {
                $firstLineText = $firstTitleLines[$i];
            } else {
                $firstLineText = '';
            }

            if (isset($secondTitleLines[$i]) === true) {
                $secondLineText = $secondTitleLines[$i];
            } else {
                $secondLineText = '';
            }

            echo '| ';
            echo $firstLineText.str_repeat(' ', (46 - strlen($firstLineText)));
            echo ' | ';
            echo $secondLineText.str_repeat(' ', (47 - strlen($secondLineText)));
            echo ' |'.PHP_EOL;
        }//end for

        echo str_repeat('-', 100).PHP_EOL;

        for ($i = 0; $i < $maxCodeLines; $i++) {
            if (isset($firstLines[$i]) === true) {
                $firstLineText = $firstLines[$i];
            } else {
                $firstLineText = '';
            }

            if (isset($secondLines[$i]) === true) {
                $secondLineText = $secondLines[$i];
            } else {
                $secondLineText = '';
            }

            echo '| ';
            echo $firstLineText.str_repeat(' ', (47 - strlen($firstLineText)));
            echo '| ';
            echo $secondLineText.str_repeat(' ', (48 - strlen($secondLineText)));
            echo '|'.PHP_EOL;
        }//end for

        echo str_repeat('-', 100).PHP_EOL.PHP_EOL;

    }//end printCodeComparisonBlock()


}//end class
