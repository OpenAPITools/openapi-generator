<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Command;

use Psr\Log\NullLogger;
use Contrib\Component\Log\ConsoleLogger;
use Contrib\Bundle\CoverallsV1Bundle\Api\Jobs;
use Contrib\Bundle\CoverallsV1Bundle\Config\Configurator;
use Contrib\Bundle\CoverallsV1Bundle\Config\Configuration;
use Contrib\Bundle\CoverallsV1Bundle\Entity\JsonFile;
use Guzzle\Http\Client;
use Guzzle\Http\Exception\ClientErrorResponseException;
use Guzzle\Http\Exception\ServerErrorResponseException;
use Guzzle\Http\Exception\CurlException;
use Guzzle\Http\Message\Response;
use Symfony\Component\Console\Command\Command;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Input\InputOption;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Stopwatch\Stopwatch;

/**
 * Coveralls Jobs API v1 command.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 * @SuppressWarnings(PHPMD.CouplingBetweenObjects)
 */
class CoverallsV1JobsCommand extends Command
{
    /**
     * Path to project root directory.
     *
     * @var string
     */
    protected $rootDir;

    /**
     * Coveralls Jobs API.
     *
     * @var \Contrib\Bundle\CoverallsV1Bundle\Api\Jobs
     */
    protected $api;

    /**
     * Logger.
     *
     * @var \Psr\Log\LoggerInterface
     */
    protected $logger;

    // internal method

    /**
     * {@inheritdoc}
     *
     * @see \Symfony\Component\Console\Command\Command::configure()
     */
    protected function configure()
    {
        $this
        ->setName('coveralls:v1:jobs')
        ->setDescription('Coveralls Jobs API v1')
        ->addOption(
            'config',
            '-c',
            InputOption::VALUE_OPTIONAL,
            '.coveralls.yml path',
            '.coveralls.yml'
        )
        ->addOption(
            'dry-run',
            null,
            InputOption::VALUE_NONE,
            'Do not send json_file to Jobs API'
        )
        ->addOption(
            'exclude-no-stmt',
            null,
            InputOption::VALUE_NONE,
            'Exclude source files that have no executable statements'
        )
        ->addOption(
            'env',
            '-e',
            InputOption::VALUE_OPTIONAL,
            'Runtime environment name: test, dev, prod',
            'prod'
        );
    }

    /**
     * {@inheritdoc}
     *
     * @see \Symfony\Component\Console\Command\Command::execute()
     */
    protected function execute(InputInterface $input, OutputInterface $output)
    {
        $stopwatch = new Stopwatch();
        $stopwatch->start(__CLASS__);

        $config = $this->loadConfiguration($input, $this->rootDir);
        $this->logger = $config->isVerbose() && !$config->isTestEnv() ? new ConsoleLogger($output) : new NullLogger();

        $this->runApi($config);

        $event = $stopwatch->stop(__CLASS__);
        $time  = number_format($event->getDuration() / 1000, 3);        // sec
        $mem   = number_format($event->getMemory() / (1024 * 1024), 2); // MB
        $this->logger->info(sprintf('elapsed time: <info>%s</info> sec memory: <info>%s</info> MB', $time, $mem));

        return 0;
    }

    // for Jobs API

    /**
     * Load configuration.
     *
     * @param  InputInterface                                         $input   Input arguments.
     * @param  string                                                 $rootDir Path to project root directory.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    protected function loadConfiguration(InputInterface $input, $rootDir)
    {
        $coverallsYmlPath = $input->getOption('config');

        $ymlPath      = $this->rootDir . DIRECTORY_SEPARATOR . $coverallsYmlPath;
        $configurator = new Configurator();

        return $configurator
        ->load($ymlPath, $rootDir)
        ->setDryRun($input->getOption('dry-run'))
        ->setExcludeNoStatementsUnlessFalse($input->getOption('exclude-no-stmt'))
        ->setVerbose($input->getOption('verbose'))
        ->setEnv($input->getOption('env'));
    }

    /**
     * Run Jobs API.
     *
     * @param  Configuration $config Configuration.
     * @return void
     */
    protected function runApi(Configuration $config)
    {
        $client    = new Client();
        $this->api = new Jobs($config, $client);

        $this
        ->collectCloverXml($config)
        ->collectGitInfo()
        ->collectEnvVars()
        ->dumpJsonFile($config)
        ->send();
    }

    /**
     * Collect clover XML into json_file.
     *
     * @param  Configuration                                                    $config Configuration.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Command\CoverallsV1JobsCommand
     */
    protected function collectCloverXml(Configuration $config)
    {
        $this->logger->info('Load coverage clover log:');

        foreach ($config->getCloverXmlPaths() as $path) {
            $this->logger->info(sprintf('  - %s', $path));
        }

        $this->api->collectCloverXml();

        $jsonFile = $this->api->getJsonFile();

        if ($jsonFile->hasSourceFiles()) {
            $this->logCollectedSourceFiles($jsonFile);
        }

        return $this;
    }

    /**
     * Log collected source files.
     *
     * @param  JsonFile $jsonFile
     * @return void
     */
    protected function logCollectedSourceFiles(JsonFile $jsonFile)
    {
        // @codeCoverageIgnoreStart
        $color = function ($coverage, $format) {
            // green  90% - 100% <info>
            // yellow 80% -  90% <comment>
            // red     0% -  80% <fg=red>
            if ($coverage >= 90) {
                return sprintf('<info>%s</info>', $format);
            } elseif ($coverage >= 80) {
                return sprintf('<comment>%s</comment>', $format);
            } else {
                return sprintf('<fg=red>%s</fg=red>', $format);
            }
        };
        // @codeCoverageIgnoreEnd

        $sourceFiles = $jsonFile->getSourceFiles();
        $numFiles    = count($sourceFiles);

        $this->logger->info(sprintf('Found <info>%s</info> source file%s:', number_format($numFiles), $numFiles > 1 ? 's' : ''));

        foreach ($sourceFiles as $sourceFile) {
            /* @var $sourceFile \Contrib\Bundle\CoverallsV1Bundle\Entity\SourceFile */
            $coverage = $sourceFile->reportLineCoverage();
            $template = '  - ' . $color($coverage, '%6.2f%%') . ' %s';

            $this->logger->info(sprintf($template, $coverage, $sourceFile->getName()));
        }

        $coverage = $jsonFile->reportLineCoverage();
        $template = 'Coverage: ' . $color($coverage, '%6.2f%% (%d/%d)');
        $metrics  = $jsonFile->getMetrics();

        $this->logger->info(sprintf($template, $coverage, $metrics->getCoveredStatements(), $metrics->getStatements()));
    }

    /**
     * Collect git repository info into json_file.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Command\CoverallsV1JobsCommand
     */
    protected function collectGitInfo()
    {
        $this->logger->info('Collect git info');

        $this->api->collectGitInfo();

        return $this;
    }

    /**
     * Collect environment variables.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Command\CoverallsV1JobsCommand
     */
    protected function collectEnvVars()
    {
        $this->logger->info('Read environment variables');

        $this->api->collectEnvVars($_SERVER);

        return $this;
    }

    /**
     * Dump uploading json file.
     *
     * @param  Configuration                                                    $config Configuration.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Command\CoverallsV1JobsCommand
     */
    protected function dumpJsonFile(Configuration $config)
    {
        $jsonPath = $config->getJsonPath();
        $this->logger->info(sprintf('Dump uploading json file: %s', $jsonPath));

        $this->api->dumpJsonFile();

        $filesize = number_format(filesize($jsonPath) / 1024, 2); // kB
        $this->logger->info(sprintf('File size: <info>%s</info> kB', $filesize));

        return $this;
    }

    /**
     * Send json_file to jobs API.
     *
     * @return void
     */
    protected function send()
    {
        $this->logger->info(sprintf('Submitting to %s', Jobs::URL));

        try {
            $response = $this->api->send();

            $message = $response
                ? sprintf('Finish submitting. status: %s %s', $response->getStatusCode(), $response->getReasonPhrase())
                : 'Finish dry run';

            $this->logger->info($message);

            // @codeCoverageIgnoreStart
            if ($response instanceof Response) {
                $this->logResponse($response);
            }

            return;
        } catch (CurlException $e) {
            // connection error
            // tested with network disconnected and got message:
            //   Connection error occurred.
            //   [curl] 6: Could not resolve host:
            //   (nil); nodename nor servname provided, or not known [url] https://coveralls.io/api/v1/jobs
            $message  = sprintf("Connection error occurred. %s\n\n%s", $e->getMessage(), $e->getTraceAsString());
        } catch (ClientErrorResponseException $e) {
            // 422 Unprocessable Entity
            $response = $e->getResponse();
            $message  = sprintf('Client error occurred. status: %s %s', $response->getStatusCode(), $response->getReasonPhrase());
        } catch (ServerErrorResponseException $e) {
            // 503 Service Unavailable
            $response = $e->getResponse();
            $message  = sprintf('Server error occurred. status: %s %s', $response->getStatusCode(), $response->getReasonPhrase());
        } catch (\Exception $e) {
            $message  = sprintf("%s\n\n%s", $e->getMessage(), $e->getTraceAsString());
        }

        $this->logger->error($message);

        if (isset($response)) {
            $this->logResponse($response);
        }
    } // @codeCoverageIgnoreEnd

    /**
     * Log response.
     *
     * @param  Response $response API response.
     * @return void
     *
     * @codeCoverageIgnore
     */
    protected function logResponse(Response $response)
    {
        $body = $response->json();

        if (isset($body['error'])) {
            if (isset($body['message'])) {
                $this->logger->error($body['message']);
            }
        } else {
            if (isset($body['message'])) {
                $this->logger->info(sprintf('Accepted %s', $body['message']));
            }

            if (isset($body['url'])) {
                $this->logger->info(sprintf('You can see the build on %s', $body['url']));
            }
        }
    }

    // accessor

    /**
     * Set root directory.
     *
     * @param string $rootDir Path to project root directory.
     */
    public function setRootDir($rootDir)
    {
        $this->rootDir = $rootDir;
    }
}
