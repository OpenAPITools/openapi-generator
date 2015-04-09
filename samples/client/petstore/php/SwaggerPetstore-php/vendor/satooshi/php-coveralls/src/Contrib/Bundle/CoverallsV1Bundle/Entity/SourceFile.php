<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Entity;

/**
 * Data represents "source_files" element of Coveralls' "json_file".
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class SourceFile extends Coveralls
{
    /**
     * Source filename.
     *
     * @var string
     */
    protected $name;

    /**
     * Source content.
     *
     * @var string
     */
    protected $source;

    /**
     * Coverage data of the source file.
     *
     * @var array
     */
    protected $coverage;

    /**
     * Absolute path.
     *
     * @var string
     */
    protected $path;

    /**
     * Line number of the source file.
     *
     * @var integer
     */
    protected $fileLines;

    /**
     * Metrics.
     *
     * @var Metrics
     */
    protected $metrics;

    /**
     * Constructor.
     *
     * @param string $path Absolute path.
     * @param string $name Source filename.
     * @param string $eol  End of line.
     */
    public function __construct($path, $name, $eol = "\n")
    {
        $this->path   = $path;
        $this->name   = $name;
        $this->source = trim(file_get_contents($path));

        $lines = explode($eol, $this->source);
        $this->fileLines = count($lines);
        $this->coverage  = array_fill(0, $this->fileLines, null);
    }

    /**
     * {@inheritdoc}
     *
     * @see \Contrib\Bundle\CoverallsBundle\Entity\ArrayConvertable::toArray()
     */
    public function toArray()
    {
        return array(
            'name'     => $this->name,
            'source'   => $this->source,
            'coverage' => $this->coverage,
        );
    }

    // API

    /**
     * Add coverage.
     *
     * @param  integer $lineNum Line number.
     * @param  integer $count   Number of covered.
     * @return void
     */
    public function addCoverage($lineNum, $count)
    {
        if (array_key_exists($lineNum, $this->coverage)) {
            $this->coverage[$lineNum] += $count;
        }
    }

    /**
     * Return line coverage.
     *
     * @return float
     */
    public function reportLineCoverage()
    {
        return $this->getMetrics()->getLineCoverage();
    }

    // accessor

    /**
     * Return source filename.
     *
     * @return string
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * Return source content.
     *
     * @return string
     */
    public function getSource()
    {
        return $this->source;
    }

    /**
     * Return coverage data of the source file.
     *
     * @return array
     */
    public function getCoverage()
    {
        return $this->coverage;
    }

    /**
     * Return absolute path.
     *
     * @return string
     */
    public function getPath()
    {
        return $this->path;
    }

    /**
     * Return line number of the source file.
     *
     * @return integer
     */
    public function getFileLines()
    {
        return $this->fileLines;
    }

    /**
     * Return metrics.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Metrics
     */
    public function getMetrics()
    {
        if (!isset($this->metrics)) {
            $this->metrics = new Metrics($this->coverage);
        }

        return $this->metrics;
    }
}
