<?php
namespace Contrib\Component\System\Git;

/**
 * @covers Contrib\Component\System\Git\GitCommand
 * @covers Contrib\Component\System\SystemCommand
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class GitCommandTest extends \PHPUnit_Framework_TestCase
{
    protected function createGitCommandMock($params)
    {
        $class = 'Contrib\Component\System\Git\GitCommand';
        $adapter = $this->getMock($class, array('executeCommand'));

        $adapter
        ->expects($this->once())
        ->method('executeCommand')
        ->with($this->equalTo($params));

        return $adapter;
    }

    // getCommandPath()

    /**
     * @test
     */
    public function commandPathIsGit()
    {
        $object = new GitCommand();

        $expected = 'git';

        $this->assertEquals($expected, $object->getCommandPath());
    }

    // getBranches()

    /**
     * @test
     */
    public function getBranchesExecuteCommand()
    {
        $expected = 'git branch';

        $object = $this->createGitCommandMock($expected);
        $object->getBranches();
    }

    /**
     * @test
     */
    public function getBranches()
    {
        $object = new GitCommand();
        $actual = $object->getBranches();

        $this->assertTrue(is_array($actual));
        $this->assertNotEmpty($actual);
    }

    // getHeadCommit()

    /**
     * @test
     */
    public function getHeadCommitExecuteCommand()
    {
        $expected = "git log -1 --pretty=format:'%H\n%aN\n%ae\n%cN\n%ce\n%s'";

        $object = $this->createGitCommandMock($expected);
        $object->getHeadCommit();
    }

    /**
     * @test
     */
    public function getHeadCommit()
    {
        $object = new GitCommand();
        $actual = $object->getHeadCommit();

        $this->assertTrue(is_array($actual));
        $this->assertNotEmpty($actual);
    }

    // getRemotes()

    /**
     * @test
     */
    public function getRemotesExecuteCommand()
    {
        $expected = 'git remote -v';

        $object = $this->createGitCommandMock($expected);
        $object->getRemotes();
    }

    /**
     * @test
     */
    public function getRemotes()
    {
        $object = new GitCommand();
        $actual = $object->getRemotes();

        $this->assertTrue(is_array($actual));
        $this->assertNotEmpty($actual);
    }

    // execute()

    /**
     * @test
     * @expectedException RuntimeException
     */
    public function throwRuntimeExceptionIfExecutedWithoutArgs()
    {
        // `git` return 1 and cause RuntimeException
        $object = new GitCommand();
        $object->execute();
    }

    // createCommand()

    /**
     * @test
     */
    public function getCommandPath()
    {
        $object = new GitCommand();
        $object->setCommandPath('ls');

        $actual = $object->execute();

        $this->assertTrue(is_array($actual));
        $this->assertNotEmpty($actual);
    }
}
