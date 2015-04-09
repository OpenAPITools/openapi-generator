<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Collector;

use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Remote;
use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit;
use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Git;
use Contrib\Component\System\Git\GitCommand;

/**
 * Git repository info collector.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class GitInfoCollector
{
    /**
     * Git command.
     *
     * @var GitCommand
     */
    protected $command;

    /**
     * Constructor.
     *
     * @param GitCommand $gitCommand Git command
     */
    public function __construct(GitCommand $command)
    {
        $this->command = $command;
    }

    // API

    /**
     * Collect git repository info.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Git
     */
    public function collect()
    {
        $branch  = $this->collectBranch();
        $commit  = $this->collectCommit();
        $remotes = $this->collectRemotes();

        return new Git($branch, $commit, $remotes);
    }

    // internal method

    /**
     * Collect branch name.
     *
     * @return string
     * @throws \RuntimeException
     */
    protected function collectBranch()
    {
        $branchesResult = $this->command->getBranches();

        foreach ($branchesResult as $result) {
            if (strpos($result, '* ') === 0) {
                $exploded = explode('* ', $result, 2);

                return $exploded[1];
            }
        }

        throw new \RuntimeException();
    }

    /**
     * Collect commit info.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit
     * @throws \RuntimeException
     */
    protected function collectCommit()
    {
        $commitResult = $this->command->getHeadCommit();

        if (count($commitResult) !== 6 || array_keys($commitResult) !== range(0, 5)) {
            throw new \RuntimeException();
        }

        $commit = new Commit();

        return $commit
        ->setId($commitResult[0])
        ->setAuthorName($commitResult[1])
        ->setAuthorEmail($commitResult[2])
        ->setCommitterName($commitResult[3])
        ->setCommitterEmail($commitResult[4])
        ->setMessage($commitResult[5]);
    }

    /**
     * Collect remotes info.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Remote[]
     * @throws \RuntimeException
     */
    protected function collectRemotes()
    {
        $remotesResult = $this->command->getRemotes();

        if (count($remotesResult) === 0) {
            throw new \RuntimeException();
        }

        // parse command result
        $results = array();

        foreach ($remotesResult as $result) {
            if (strpos($result, ' ') !== false) {
                list($remote) = explode(' ', $result, 2);

                $results[] = $remote;
            }
        }

        // filter
        $results = array_unique($results);

        // create Remote instances
        $remotes = array();

        foreach ($results as $result) {
            if (strpos($result, "\t") !== false) {
                list($name, $url) = explode("\t", $result, 2);

                $remote = new Remote();
                $remotes[] = $remote->setName($name)->setUrl($url);
            }
        }

        return $remotes;
    }

    // accessor

    /**
     * Return git command.
     *
     * @return \Contrib\Component\System\Git\GitCommand
     */
    public function getCommand()
    {
        return $this->command;
    }
}
