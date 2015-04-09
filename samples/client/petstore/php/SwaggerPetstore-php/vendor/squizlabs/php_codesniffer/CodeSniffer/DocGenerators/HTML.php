<?php
/**
 * A doc generator that outputs documentation in one big HTML file.
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
 * A doc generator that outputs documentation in one big HTML file.
 *
 * Output is in one large HTML file and is designed for you to style with
 * your own stylesheet. It contains a table of contents at the top with anchors
 * to each sniff.
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
class PHP_CodeSniffer_DocGenerators_HTML extends PHP_CodeSniffer_DocGenerators_Generator
{


    /**
     * Generates the documentation for a standard.
     *
     * @return void
     * @see    processSniff()
     */
    public function generate()
    {
        ob_start();
        $this->printHeader();

        $standardFiles = $this->getStandardFiles();
        $this->printToc($standardFiles);

        foreach ($standardFiles as $standard) {
            $doc = new DOMDocument();
            $doc->load($standard);
            $documentation = $doc->getElementsByTagName('documentation')->item(0);
            $this->processSniff($documentation);
        }

        $this->printFooter();

        $content = ob_get_contents();
        ob_end_clean();

        echo $content;

    }//end generate()


    /**
     * Print the header of the HTML page.
     *
     * @return void
     */
    protected function printHeader()
    {
        $standard = $this->getStandard();
        echo '<html>'.PHP_EOL;
        echo ' <head>'.PHP_EOL;
        echo "  <title>$standard Coding Standards</title>".PHP_EOL;
        echo '  <style>
                    body {
                        background-color: #FFFFFF;
                        font-size: 14px;
                        font-family: Arial, Helvetica, sans-serif;
                        color: #000000;
                    }

                    h1 {
                        color: #666666;
                        font-size: 20px;
                        font-weight: bold;
                        margin-top: 0px;
                        background-color: #E6E7E8;
                        padding: 20px;
                        border: 1px solid #BBBBBB;
                    }

                    h2 {
                        color: #00A5E3;
                        font-size: 16px;
                        font-weight: normal;
                        margin-top: 50px;
                    }

                    .code-comparison {
                        width: 100%;
                    }

                    .code-comparison td {
                        border: 1px solid #CCCCCC;
                    }

                    .code-comparison-title, .code-comparison-code {
                        font-family: Arial, Helvetica, sans-serif;
                        font-size: 12px;
                        color: #000000;
                        vertical-align: top;
                        padding: 4px;
                        width: 50%;
                        background-color: #F1F1F1;
                        line-height: 15px;
                    }

                    .code-comparison-code {
                        font-family: Courier;
                        background-color: #F9F9F9;
                    }

                    .code-comparison-highlight {
                        background-color: #DDF1F7;
                        border: 1px solid #00A5E3;
                        line-height: 15px;
                    }

                    .tag-line {
                        text-align: center;
                        width: 100%;
                        margin-top: 30px;
                        font-size: 12px;
                    }

                    .tag-line a {
                        color: #000000;
                    }
                </style>'.PHP_EOL;
        echo ' </head>'.PHP_EOL;
        echo ' <body>'.PHP_EOL;
        echo "  <h1>$standard Coding Standards</h1>".PHP_EOL;

    }//end printHeader()


    /**
     * Print the table of contents for the standard.
     *
     * The TOC is just an unordered list of bookmarks to sniffs on the page.
     *
     * @param array $standardFiles An array of paths to the XML standard files.
     *
     * @return void
     */
    protected function printToc($standardFiles)
    {
        echo '  <h2>Table of Contents</h2>'.PHP_EOL;
        echo '  <ul class="toc">'.PHP_EOL;

        foreach ($standardFiles as $standard) {
            $doc = new DOMDocument();
            $doc->load($standard);
            $documentation = $doc->getElementsByTagName('documentation')->item(0);
            $title         = $this->getTitle($documentation);
            echo '   <li><a href="#'.str_replace(' ', '-', $title)."\">$title</a></li>".PHP_EOL;
        }

        echo '  </ul>'.PHP_EOL;

    }//end printToc()


    /**
     * Print the footer of the HTML page.
     *
     * @return void
     */
    protected function printFooter()
    {
        // Turn off errors so we don't get timezone warnings if people
        // don't have their timezone set.
        $errorLevel = error_reporting(0);
        echo '  <div class="tag-line">';
        echo 'Documentation generated on '.date('r');
        echo ' by <a href="https://github.com/squizlabs/PHP_CodeSniffer">PHP_CodeSniffer '.PHP_CodeSniffer::VERSION.'</a>';
        echo '</div>'.PHP_EOL;
        error_reporting($errorLevel);

        echo ' </body>'.PHP_EOL;
        echo '</html>'.PHP_EOL;

    }//end printFooter()


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
        $title = $this->getTitle($doc);
        echo '  <a name="'.str_replace(' ', '-', $title).'" />'.PHP_EOL;
        echo "  <h2>$title</h2>".PHP_EOL;

        foreach ($doc->childNodes as $node) {
            if ($node->nodeName === 'standard') {
                $this->printTextBlock($node);
            } else if ($node->nodeName === 'code_comparison') {
                $this->printCodeComparisonBlock($node);
            }
        }

    }//end processSniff()


    /**
     * Print a text block found in a standard.
     *
     * @param DOMNode $node The DOMNode object for the text block.
     *
     * @return void
     */
    protected function printTextBlock($node)
    {
        $content = trim($node->nodeValue);
        $content = htmlspecialchars($content);

        // Allow em tags only.
        $content = str_replace('&lt;em&gt;', '<em>', $content);
        $content = str_replace('&lt;/em&gt;', '</em>', $content);

        echo "  <p class=\"text\">$content</p>".PHP_EOL;

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

        $firstTitle = $codeBlocks->item(0)->getAttribute('title');
        $first      = trim($codeBlocks->item(0)->nodeValue);
        $first      = str_replace('<?php', '&lt;?php', $first);
        $first      = str_replace("\n", '</br>', $first);
        $first      = str_replace(' ', '&nbsp;', $first);
        $first      = str_replace('<em>', '<span class="code-comparison-highlight">', $first);
        $first      = str_replace('</em>', '</span>', $first);

        $secondTitle = $codeBlocks->item(1)->getAttribute('title');
        $second      = trim($codeBlocks->item(1)->nodeValue);
        $second      = str_replace('<?php', '&lt;?php', $second);
        $second      = str_replace("\n", '</br>', $second);
        $second      = str_replace(' ', '&nbsp;', $second);
        $second      = str_replace('<em>', '<span class="code-comparison-highlight">', $second);
        $second      = str_replace('</em>', '</span>', $second);

        echo '  <table class="code-comparison">'.PHP_EOL;
        echo '   <tr>'.PHP_EOL;
        echo "    <td class=\"code-comparison-title\">$firstTitle</td>".PHP_EOL;
        echo "    <td class=\"code-comparison-title\">$secondTitle</td>".PHP_EOL;
        echo '   </tr>'.PHP_EOL;
        echo '   <tr>'.PHP_EOL;
        echo "    <td class=\"code-comparison-code\">$first</td>".PHP_EOL;
        echo "    <td class=\"code-comparison-code\">$second</td>".PHP_EOL;
        echo '   </tr>'.PHP_EOL;
        echo '  </table>'.PHP_EOL;

    }//end printCodeComparisonBlock()


}//end class
