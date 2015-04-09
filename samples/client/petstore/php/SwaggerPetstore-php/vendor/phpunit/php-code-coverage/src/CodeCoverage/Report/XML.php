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
class PHP_CodeCoverage_Report_XML
{
    /**
     * @var string
     */
    private $target;

    /**
     * @var PHP_CodeCoverage_Report_XML_Project
     */
    private $project;

    public function process(PHP_CodeCoverage $coverage, $target)
    {
        if (substr($target, -1, 1) != DIRECTORY_SEPARATOR) {
            $target .= DIRECTORY_SEPARATOR;
        }

        $this->target = $target;
        $this->initTargetDirectory($target);

        $report = $coverage->getReport();

        $this->project = new PHP_CodeCoverage_Report_XML_Project(
            $coverage->getReport()->getName()
        );

        $this->processTests($coverage->getTests());
        $this->processDirectory($report, $this->project);

        $index = $this->project->asDom();
        $index->formatOutput = true;
        $index->preserveWhiteSpace = false;
        $index->save($target . '/index.xml');
    }

    private function initTargetDirectory($dir)
    {
        if (file_exists($dir)) {
            if (!is_dir($dir)) {
                throw new PHP_CodeCoverage_Exception(
                    "'$dir' exists but is not a directory."
                );
            }

            if (!is_writable($dir)) {
                throw new PHP_CodeCoverage_Exception(
                    "'$dir' exists but is not writable."
                );
            }
        } elseif (!@mkdir($dir, 0777, true)) {
            throw new PHP_CodeCoverage_Exception(
                "'$dir' could not be created."
            );
        }
    }

    private function processDirectory(PHP_CodeCoverage_Report_Node_Directory $directory, PHP_CodeCoverage_Report_XML_Node $context)
    {
        $dirObject = $context->addDirectory($directory->getName());

        $this->setTotals($directory, $dirObject->getTotals());

        foreach ($directory as $node) {
            if ($node instanceof PHP_CodeCoverage_Report_Node_Directory) {
                $this->processDirectory($node, $dirObject);
                continue;
            }

            if ($node instanceof PHP_CodeCoverage_Report_Node_File) {
                $this->processFile($node, $dirObject);
                continue;
            }

            throw new PHP_CodeCoverage_Exception(
                'Unknown node type for XML report'
            );
        }
    }

    private function processFile(PHP_CodeCoverage_Report_Node_File $file, PHP_CodeCoverage_Report_XML_Directory $context)
    {
        $fileObject = $context->addFile(
            $file->getName(),
            $file->getId() . '.xml'
        );

        $this->setTotals($file, $fileObject->getTotals());

        $fileReport = new PHP_CodeCoverage_Report_XML_File_Report(
            $file->getName()
        );

        $this->setTotals($file, $fileReport->getTotals());

        foreach ($file->getClassesAndTraits() as $unit) {
            $this->processUnit($unit, $fileReport);
        }

        foreach ($file->getFunctions() as $function) {
            $this->processFunction($function, $fileReport);
        }

        foreach ($file->getCoverageData() as $line => $tests) {
            if (!is_array($tests) || count($tests) == 0) {
                continue;
            }

            $coverage = $fileReport->getLineCoverage($line);

            foreach ($tests as $test) {
                $coverage->addTest($test);
            }

            $coverage->finalize();
        }

        $this->initTargetDirectory(
            $this->target . dirname($file->getId()) . '/'
        );

        $fileDom = $fileReport->asDom();
        $fileDom->formatOutput = true;
        $fileDom->preserveWhiteSpace = false;
        $fileDom->save($this->target . $file->getId() . '.xml');
    }

    private function processUnit($unit, PHP_CodeCoverage_Report_XML_File_Report $report)
    {
        if (isset($unit['className'])) {
            $unitObject = $report->getClassObject($unit['className']);
        } else {
            $unitObject = $report->getTraitObject($unit['traitName']);
        }

        $unitObject->setLines(
            $unit['startLine'],
            $unit['executableLines'],
            $unit['executedLines']
        );

        $unitObject->setCrap($unit['crap']);

        $unitObject->setPackage(
            $unit['package']['fullPackage'],
            $unit['package']['package'],
            $unit['package']['subpackage'],
            $unit['package']['category']
        );

        $unitObject->setNamespace($unit['package']['namespace']);

        foreach ($unit['methods'] as $method) {
            $methodObject = $unitObject->addMethod($method['methodName']);
            $methodObject->setSignature($method['signature']);
            $methodObject->setLines($method['startLine'], $method['endLine']);
            $methodObject->setCrap($method['crap']);
            $methodObject->setTotals(
                $method['executableLines'],
                $method['executedLines'],
                $method['coverage']
            );
        }
    }

    private function processFunction($function, PHP_CodeCoverage_Report_XML_File_Report $report)
    {
        $functionObject = $report->getFunctionObject($function['functionName']);

        $functionObject->setSignature($function['signature']);
        $functionObject->setLines($function['startLine']);
        $functionObject->setCrap($function['crap']);
        $functionObject->setTotals($function['executableLines'], $function['executedLines'], $function['coverage']);
    }

    private function processTests(array $tests)
    {
        $testsObject = $this->project->getTests();

        foreach ($tests as $test => $result) {
            if ($test == 'UNCOVERED_FILES_FROM_WHITELIST') {
                continue;
            }

            $testsObject->addTest($test, $result);
        }
    }

    private function setTotals(PHP_CodeCoverage_Report_Node $node, PHP_CodeCoverage_Report_XML_Totals $totals)
    {
        $loc = $node->getLinesOfCode();

        $totals->setNumLines(
            $loc['loc'],
            $loc['cloc'],
            $loc['ncloc'],
            $node->getNumExecutableLines(),
            $node->getNumExecutedLines()
        );

        $totals->setNumClasses(
            $node->getNumClasses(),
            $node->getNumTestedClasses()
        );

        $totals->setNumTraits(
            $node->getNumTraits(),
            $node->getNumTestedTraits()
        );

        $totals->setNumMethods(
            $node->getNumMethods(),
            $node->getNumTestedMethods()
        );

        $totals->setNumFunctions(
            $node->getNumFunctions(),
            $node->getNumTestedFunctions()
        );
    }
}
