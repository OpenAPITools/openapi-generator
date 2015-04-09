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
 * Generates human readable output from an PHP_CodeCoverage object.
 *
 * The output gets put into a text file our written to the CLI.
 *
 * @category   PHP
 * @package    CodeCoverage
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/php-code-coverage
 * @since      Class available since Release 1.1.0
 */
class PHP_CodeCoverage_Report_Text
{
    protected $lowUpperBound;
    protected $highLowerBound;
    protected $showUncoveredFiles;
    protected $showOnlySummary;

    protected $colors = array(
        'green'  => "\x1b[30;42m",
        'yellow' => "\x1b[30;43m",
        'red'    => "\x1b[37;41m",
        'header' => "\x1b[1;37;40m",
        'reset'  => "\x1b[0m",
        'eol'    => "\x1b[2K",
    );

    public function __construct($lowUpperBound, $highLowerBound, $showUncoveredFiles, $showOnlySummary)
    {
        $this->lowUpperBound      = $lowUpperBound;
        $this->highLowerBound     = $highLowerBound;
        $this->showUncoveredFiles = $showUncoveredFiles;
        $this->showOnlySummary    = $showOnlySummary;
    }

    /**
     * @param  PHP_CodeCoverage $coverage
     * @param  bool             $showColors
     * @return string
     */
    public function process(PHP_CodeCoverage $coverage, $showColors = false)
    {
        $output = PHP_EOL . PHP_EOL;
        $report = $coverage->getReport();
        unset($coverage);

        $colors = array(
            'header'  => '',
            'classes' => '',
            'methods' => '',
            'lines'   => '',
            'reset'   => '',
            'eol'     => ''
        );

        if ($showColors) {
            $colors['classes'] = $this->getCoverageColor(
                $report->getNumTestedClassesAndTraits(),
                $report->getNumClassesAndTraits()
            );
            $colors['methods'] = $this->getCoverageColor(
                $report->getNumTestedMethods(),
                $report->getNumMethods()
            );
            $colors['lines']   = $this->getCoverageColor(
                $report->getNumExecutedLines(),
                $report->getNumExecutableLines()
            );
            $colors['reset']   = $this->colors['reset'];
            $colors['header']  = $this->colors['header'];
            $colors['eol']     = $this->colors['eol'];
        }

        $classes = sprintf(
            '  Classes: %6s (%d/%d)',
            PHP_CodeCoverage_Util::percent(
                $report->getNumTestedClassesAndTraits(),
                $report->getNumClassesAndTraits(),
                true
            ),
            $report->getNumTestedClassesAndTraits(),
            $report->getNumClassesAndTraits()
        );

        $methods = sprintf(
            '  Methods: %6s (%d/%d)',
            PHP_CodeCoverage_Util::percent(
                $report->getNumTestedMethods(),
                $report->getNumMethods(),
                true
            ),
            $report->getNumTestedMethods(),
            $report->getNumMethods()
        );

        $lines = sprintf(
            '  Lines:   %6s (%d/%d)',
            PHP_CodeCoverage_Util::percent(
                $report->getNumExecutedLines(),
                $report->getNumExecutableLines(),
                true
            ),
            $report->getNumExecutedLines(),
            $report->getNumExecutableLines()
        );

        $padding = max(array_map('strlen', array($classes, $methods, $lines)));

        if ($this->showOnlySummary) {
            $title = 'Code Coverage Report Summary:';
            $padding = max($padding, strlen($title));

            $output .= $this->format($colors['header'], $padding, $title);
        } else {
            $date = date('  Y-m-d H:i:s', $_SERVER['REQUEST_TIME']);
            $title = 'Code Coverage Report:';

            $output .= $this->format($colors['header'], $padding, $title);
            $output .= $this->format($colors['header'], $padding, $date);
            $output .= $this->format($colors['header'], $padding, '');
            $output .= $this->format($colors['header'], $padding, ' Summary:');
        }

        $output .= $this->format($colors['classes'], $padding, $classes);
        $output .= $this->format($colors['methods'], $padding, $methods);
        $output .= $this->format($colors['lines'], $padding, $lines);

        if ($this->showOnlySummary) {
            return $output . PHP_EOL;
        }

        $classCoverage = array();

        foreach ($report as $item) {
            if (!$item instanceof PHP_CodeCoverage_Report_Node_File) {
                continue;
            }

            $classes  = $item->getClassesAndTraits();

            foreach ($classes as $className => $class) {
                $classStatements        = 0;
                $coveredClassStatements = 0;
                $coveredMethods         = 0;
                $classMethods           = 0;

                foreach ($class['methods'] as $method) {
                    if ($method['executableLines'] == 0)
                        continue;

                    $classMethods++;
                    $classStatements        += $method['executableLines'];
                    $coveredClassStatements += $method['executedLines'];
                    if ($method['coverage'] == 100) {
                        $coveredMethods++;
                    }
                }

                if (!empty($class['package']['namespace'])) {
                    $namespace = '\\' . $class['package']['namespace'] . '::';
                } elseif (!empty($class['package']['fullPackage'])) {
                    $namespace = '@' . $class['package']['fullPackage'] . '::';
                } else {
                    $namespace = '';
                }

                $classCoverage[$namespace . $className] = array(
                    'namespace'         => $namespace,
                    'className '        => $className,
                    'methodsCovered'    => $coveredMethods,
                    'methodCount'       => $classMethods,
                    'statementsCovered' => $coveredClassStatements,
                    'statementCount'    => $classStatements,
                );
            }
        }

        ksort($classCoverage);

        $methodColor = '';
        $linesColor  = '';
        $resetColor  = '';

        foreach ($classCoverage as $fullQualifiedPath => $classInfo) {
            if ($classInfo['statementsCovered'] != 0 ||
                $this->showUncoveredFiles) {

                if ($showColors) {
                    $methodColor = $this->getCoverageColor($classInfo['methodsCovered'], $classInfo['methodCount']);
                    $linesColor  = $this->getCoverageColor($classInfo['statementsCovered'], $classInfo['statementCount']);
                    $resetColor  = $colors['reset'];
                }

                $output .= PHP_EOL . $fullQualifiedPath . PHP_EOL
                    . '  ' . $methodColor . 'Methods: ' . $this->printCoverageCounts($classInfo['methodsCovered'], $classInfo['methodCount'], 2) . $resetColor . ' '
                    . '  ' . $linesColor  . 'Lines: ' . $this->printCoverageCounts($classInfo['statementsCovered'], $classInfo['statementCount'], 3) . $resetColor
                ;
            }
        }

        return $output . PHP_EOL;
    }

    protected function getCoverageColor($numberOfCoveredElements, $totalNumberOfElements)
    {
        $coverage = PHP_CodeCoverage_Util::percent(
            $numberOfCoveredElements, $totalNumberOfElements
        );

        if ($coverage > $this->highLowerBound) {
            return $this->colors['green'];
        } elseif ($coverage > $this->lowUpperBound) {
            return $this->colors['yellow'];
        }

        return $this->colors['red'];
    }

    protected function printCoverageCounts($numberOfCoveredElements, $totalNumberOfElements, $presicion)
    {
        $format = '%' . $presicion . 's';

        return PHP_CodeCoverage_Util::percent(
            $numberOfCoveredElements, $totalNumberOfElements, true, true
        ) .
        ' (' . sprintf($format, $numberOfCoveredElements) . '/' .
        sprintf($format, $totalNumberOfElements) . ')';
    }

    private function format($color, $padding, $string)
    {
        $reset = $color ? $this->colors['reset'] : '';

        return $color . str_pad($string, $padding) . $reset . PHP_EOL;
    }
}
