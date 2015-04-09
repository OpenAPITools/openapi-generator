<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Config;

use Contrib\Component\File\Path;
use Symfony\Component\Yaml\Yaml;
use Symfony\Component\Config\Definition\Processor;
use Symfony\Component\Config\Definition\Exception\InvalidConfigurationException;

/**
 * Coveralls API configurator.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class Configurator
{
    // API

    /**
     * Load configuration.
     *
     * @param  string                                                 $coverallsYmlPath Path to .coveralls.yml.
     * @param  string                                                 $rootDir          Path to project root directory.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function load($coverallsYmlPath, $rootDir)
    {
        $yml     = $this->parse($coverallsYmlPath);
        $options = $this->process($yml);

        return $this->createConfiguration($options, $rootDir);
    }

    // Internal method

    /**
     * Parse .coveralls.yml.
     *
     * @param  string $coverallsYmlPath Path to .coveralls.yml.
     * @return array
     */
    protected function parse($coverallsYmlPath)
    {
        $file = new Path();
        $path = realpath($coverallsYmlPath);

        if ($file->isRealFileReadable($path)) {
            $yml = Yaml::parse($path);

            return empty($yml) ? array() : $yml;
        }

        return array();
    }

    /**
     * Process parsed configuration according to the configuration definition.
     *
     * @param  array $yml Parsed configuration.
     * @return array
     */
    protected function process(array $yml)
    {
        $processor     = new Processor();
        $configuration = new CoverallsConfiguration();

        return $processor->processConfiguration($configuration, array('coveralls' => $yml));
    }

    /**
     * Create coveralls configuration.
     *
     * @param  array                                                  $options Processed configuration.
     * @param  string                                                 $rootDir Path to project root directory.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    protected function createConfiguration(array $options, $rootDir)
    {
        $configuration = new Configuration();
        $file          = new Path();

        $repoToken       = $options['repo_token'];
        $repoSecretToken = $options['repo_secret_token'];

        return $configuration
        ->setRepoToken($repoToken !== null ? $repoToken : $repoSecretToken)
        ->setServiceName($options['service_name'])
        // for PHP lib
        ->setSrcDir($this->ensureSrcDir($options['src_dir'], $rootDir, $file))
        ->setCloverXmlPaths($this->ensureCloverXmlPaths($options['coverage_clover'], $rootDir, $file))
        ->setJsonPath($this->ensureJsonPath($options['json_path'], $rootDir, $file))
        ->setExcludeNoStatements($options['exclude_no_stmt']);
    }

    /**
     * Ensure src_dir is valid.
     *
     * @param  string                        $option  src_dir option.
     * @param  string                        $rootDir Path to project root directory.
     * @param  Path                          $file    Path object.
     * @return string                        Valid src_dir.
     * @throws InvalidConfigurationException
     */
    protected function ensureSrcDir($option, $rootDir, Path $file)
    {
        // normalize
        $realpath = $file->getRealPath($option, $rootDir);

        // validate
        if (!$file->isRealDirExist($realpath)) {
            throw new InvalidConfigurationException('src directory is not found');
        }

        return $realpath;
    }

    /**
     * Ensure coverage_clover is valid.
     *
     * @param  string                        $option  coverage_clover option.
     * @param  string                        $rootDir Path to project root directory.
     * @param  Path                          $file    Path object.
     * @return array                         Valid Absolute pathes of coverage_clover.
     * @throws InvalidConfigurationException
     */
    protected function ensureCloverXmlPaths($option, $rootDir, Path $file)
    {
        if (is_array($option)) {
            return $this->getGlobPathsFromArrayOption($option, $rootDir, $file);
        }

        return $this->getGlobPathsFromStringOption($option, $rootDir, $file);
    }

    /**
     * Return absolute paths from glob path.
     *
     * @param  string                        $path Absolute path.
     * @return array                         Absolute paths.
     * @throws InvalidConfigurationException
     */
    protected function getGlobPaths($path)
    {
        $paths    = array();
        $iterator = new \GlobIterator($path);

        foreach ($iterator as $fileInfo) {
            /* @var $fileInfo \SplFileInfo */
            $paths[] = $fileInfo->getPathname();
        }

        // validate
        if (count($paths) === 0) {
            throw new InvalidConfigurationException('coverage_clover XML file is not readable');
        }

        return $paths;
    }

    /**
     * Return absolute paths from string option value.
     *
     * @param  string                        $option  coverage_clover option value.
     * @param  string                        $rootDir Path to project root directory.
     * @param  Path                          $file    Path object.
     * @return array                         Absolute pathes.
     * @throws InvalidConfigurationException
     */
    protected function getGlobPathsFromStringOption($option, $rootDir, Path $file)
    {
        if (!is_string($option)) {
            throw new InvalidConfigurationException('coverage_clover XML file is not readable');
        }

        // normalize
        $path = $file->toAbsolutePath($option, $rootDir);

        return $this->getGlobPaths($path);
    }

    /**
     * Return absolute paths from array option values.
     *
     * @param  array  $options coverage_clover option values.
     * @param  string $rootDir Path to project root directory.
     * @param  Path   $file    Path object.
     * @return array  Absolute pathes.
     */
    protected function getGlobPathsFromArrayOption(array $options, $rootDir, Path $file)
    {
        $paths = array();

        foreach ($options as $option) {
            $paths = array_merge($paths, $this->getGlobPathsFromStringOption($option, $rootDir, $file));
        }

        return $paths;
    }

    /**
     * Ensure json_path is valid.
     *
     * @param  string                        $option  json_path option.
     * @param  string                        $rootDir Path to project root directory.
     * @param  Path                          $file    Path object.
     * @return string                        Valid json_path.
     * @throws InvalidConfigurationException
     */
    protected function ensureJsonPath($option, $rootDir, Path $file)
    {
        // normalize
        $realpath = $file->getRealWritingFilePath($option, $rootDir);

        // validate file
        $realFilePath = $file->getRealPath($realpath, $rootDir);

        if ($realFilePath !== false && !$file->isRealFileWritable($realFilePath)) {
            throw new InvalidConfigurationException('json_path is not writable');
        }

        // validate parent dir
        $realDir = $file->getRealDir($realpath, $rootDir);

        if (!$file->isRealDirWritable($realDir)) {
            throw new InvalidConfigurationException('json_path is not writable');
        }

        return $realpath;
    }
}
