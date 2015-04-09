<?php
namespace Contrib\Component\System\Git;

use Contrib\Component\System\SystemCommand;

/**
 * Git command.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class GitCommand extends SystemCommand
{
    /**
     * Command name or path.
     *
     * @var string
     */
    protected $commandPath = 'git';

    // API

    /**
     * Return branch names.
     *
     * @return array
     */
    public function getBranches()
    {
        $command = $this->createCommand('branch');

        return $this->executeCommand($command);
    }

    /**
     * Return HEAD commit.
     *
     * @return array
     */
    public function getHeadCommit()
    {
        $command = $this->createCommand("log -1 --pretty=format:'%H\n%aN\n%ae\n%cN\n%ce\n%s'");

        return $this->executeCommand($command);
    }

    /**
     * Return remote repositories.
     *
     * @return array
     */
    public function getRemotes()
    {
        $command = $this->createCommand('remote -v');

        return $this->executeCommand($command);
    }
}
