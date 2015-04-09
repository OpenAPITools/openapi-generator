<?php
namespace Contrib\Component\System;

/**
 * System command.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
abstract class SystemCommand
{
    /**
     * Command name or path.
     *
     * @var string
     */
    protected $commandPath;

    // API

    /**
     * Execute command.
     *
     * @return array
     */
    public function execute()
    {
        $command = $this->createCommand();

        return $this->executeCommand($command);
    }

    // internal method

    /**
     * Execute command.
     *
     * @param  string            $command
     * @return array
     * @throws \RuntimeException
     */
    protected function executeCommand($command)
    {
        exec($command, $result, $returnValue);

        if ($returnValue === 0) {
            return $result;
        }

        throw new \RuntimeException(sprintf('Failed to execute command: %s', $command), $returnValue);
    }

    /**
     * Create command.
     *
     * @param  string $args Command arguments.
     * @return string
     */
    protected function createCommand($args = null)
    {
        if ($args === null) {
            return $this->commandPath;
        }

        // escapeshellarg($args) ?
        return sprintf('%s %s', $this->commandPath, $args);
    }

    // accessor

    /**
     * Set command path.
     *
     * @param  string $commandPath Command name or path.
     * @return void
     */
    public function setCommandPath($commandPath)
    {
        $this->commandPath = $commandPath;
    }

    /**
     * Return command path.
     *
     * @return string
     */
    public function getCommandPath()
    {
        return $this->commandPath;
    }
}
