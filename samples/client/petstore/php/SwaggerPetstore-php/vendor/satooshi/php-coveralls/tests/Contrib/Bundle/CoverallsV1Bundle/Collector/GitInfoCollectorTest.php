<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Collector;

use Contrib\Component\System\Git\GitCommand;

use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Git;
use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit;
use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Remote;

/**
 * @covers Contrib\Bundle\CoverallsV1Bundle\Collector\GitInfoCollector
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class GitInfoCollectorTest extends \PHPUnit_Framework_TestCase
{
    public function setUp()
    {
        $this->getBranchesValue = array(
            '  master',
            '* branch1',
            '  branch2',
        );
        $this->getHeadCommitValue = array(
            'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
            'Author Name',
            'author@satooshi.jp',
            'Committer Name',
            'committer@satooshi.jp',
            'commit message',
        );
        $this->getRemotesValue = array(
            "origin\tgit@github.com:satooshi/php-coveralls.git (fetch)",
            "origin\tgit@github.com:satooshi/php-coveralls.git (push)",
        );
    }

    protected function createGitCommandStub()
    {
        $class = 'Contrib\Component\System\Git\GitCommand';

        return $this->getMock($class);
    }

    protected function createGitCommandStubWith($getBranchesValue, $getHeadCommitValue, $getRemotesValue)
    {
        $stub = $this->createGitCommandStub();

        $this->setUpGitCommandStubWithGetBranchesOnce($stub, $getBranchesValue);
        $this->setUpGitCommandStubWithGetHeadCommitOnce($stub, $getHeadCommitValue);
        $this->setUpGitCommandStubWithGetRemotesOnce($stub, $getRemotesValue);

        return $stub;
    }

    protected function createGitCommandStubCalledBranches($getBranchesValue, $getHeadCommitValue, $getRemotesValue)
    {
        $stub = $this->createGitCommandStub();

        $this->setUpGitCommandStubWithGetBranchesOnce($stub, $getBranchesValue);
        $this->setUpGitCommandStubWithGetHeadCommitNeverCalled($stub, $getHeadCommitValue);
        $this->setUpGitCommandStubWithGetRemotesNeverCalled($stub, $getRemotesValue);

        return $stub;
    }

    protected function createGitCommandStubCalledHeadCommit($getBranchesValue, $getHeadCommitValue, $getRemotesValue)
    {
        $stub = $this->createGitCommandStub();

        $this->setUpGitCommandStubWithGetBranchesOnce($stub, $getBranchesValue);
        $this->setUpGitCommandStubWithGetHeadCommitOnce($stub, $getHeadCommitValue);
        $this->setUpGitCommandStubWithGetRemotesNeverCalled($stub, $getRemotesValue);

        return $stub;
    }

    protected function setUpGitCommandStubWithGetBranchesOnce($stub, $getBranchesValue)
    {
        $stub->expects($this->once())
        ->method('getBranches')
        ->will($this->returnValue($getBranchesValue));
    }

    protected function setUpGitCommandStubWithGetHeadCommitOnce($stub, $getHeadCommitValue)
    {
        $stub->expects($this->once())
        ->method('getHeadCommit')
        ->will($this->returnValue($getHeadCommitValue));
    }

    protected function setUpGitCommandStubWithGetHeadCommitNeverCalled($stub, $getHeadCommitValue)
    {
        $stub->expects($this->never())
        ->method('getHeadCommit')
        ->will($this->returnValue($getHeadCommitValue));
    }

    protected function setUpGitCommandStubWithGetRemotesOnce($stub, $getRemotesValue)
    {
        $stub->expects($this->once())
        ->method('getRemotes')
        ->will($this->returnValue($getRemotesValue));
    }

    protected function setUpGitCommandStubWithGetRemotesNeverCalled($stub, $getRemotesValue)
    {
        $stub->expects($this->never())
        ->method('getRemotes')
        ->will($this->returnValue($getRemotesValue));
    }

    // getCommand()

    /**
     * @test
     */
    public function getCommand()
    {
        $command = new GitCommand();
        $object = new GitInfoCollector($command);

        $this->assertSame($command, $object->getCommand());
    }

    // collect()

    /**
     * @test
     */
    public function collect()
    {
        $gitCommand = $this->createGitCommandStubWith($this->getBranchesValue, $this->getHeadCommitValue, $this->getRemotesValue);
        $object = new GitInfoCollector($gitCommand);

        $git = $object->collect();

        $gitClass = 'Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Git';
        $this->assertTrue($git instanceof $gitClass);
        $this->assertGit($git);
    }

    protected function assertGit(Git $git)
    {
        $this->assertEquals('branch1', $git->getBranch());

        $commit = $git->getHead();

        $commitClass = 'Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit';
        $this->assertTrue($commit instanceof $commitClass);
        $this->assertCommit($commit);

        $remotes = $git->getRemotes();
        $this->assertCount(1, $remotes);

        $remoteClass = 'Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Remote';
        $this->assertTrue($remotes[0] instanceof $remoteClass);
        $this->assertRemote($remotes[0]);
    }

    protected function assertCommit(Commit $commit)
    {
        $this->assertEquals('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', $commit->getId());
        $this->assertEquals('Author Name', $commit->getAuthorName());
        $this->assertEquals('author@satooshi.jp', $commit->getAuthorEmail());
        $this->assertEquals('Committer Name', $commit->getCommitterName());
        $this->assertEquals('committer@satooshi.jp', $commit->getCommitterEmail());
        $this->assertEquals('commit message', $commit->getMessage());
    }

    protected function assertRemote(Remote $remote)
    {
        $this->assertEquals('origin', $remote->getName());
        $this->assertEquals('git@github.com:satooshi/php-coveralls.git', $remote->getUrl());
    }

    // collectBranch() exception

    /**
     * @test
     * @expectedException RuntimeException
     */
    public function throwRuntimeExceptionIfCurrentBranchNotFound()
    {
        $getBranchesValue = array(
            '  master',
        );
        $gitCommand = $this->createGitCommandStubCalledBranches($getBranchesValue, $this->getHeadCommitValue, $this->getRemotesValue);

        $object = new GitInfoCollector($gitCommand);

        $object->collect();
    }

    // collectCommit() exception

    /**
     * @test
     * @expectedException RuntimeException
     */
    public function throwRuntimeExceptionIfHeadCommitIsInvalid()
    {
        $getHeadCommitValue = array();
        $gitCommand = $this->createGitCommandStubCalledHeadCommit($this->getBranchesValue, $getHeadCommitValue, $this->getRemotesValue);

        $object = new GitInfoCollector($gitCommand);

        $object->collect();
    }

    // collectRemotes() exception

    /**
     * @test
     * @expectedException RuntimeException
     */
    public function throwRuntimeExceptionIfRemoteIsInvalid()
    {
        $getRemotesValue = array();
        $gitCommand = $this->createGitCommandStubWith($this->getBranchesValue, $this->getHeadCommitValue, $getRemotesValue);

        $object = new GitInfoCollector($gitCommand);

        $object->collect();
    }
}
