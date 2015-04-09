<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Entity\Git;

/**
 * @covers Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Git
 * @covers Contrib\Bundle\CoverallsV1Bundle\Entity\Coveralls
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class GitTest extends \PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
        $this->branchName = 'branch_name';
        $this->commit = $this->createCommit();
        $this->remote = $this->createRemote();

        $this->object = new Git($this->branchName, $this->commit, array($this->remote));
    }

    protected function createRemote($name = 'name', $url  = 'url')
    {
        $remote = new Remote();

        return $remote
        ->setName($name)
        ->setUrl($url);
    }

    protected function createCommit($id = 'id', $authorName  = 'author_name', $authorEmail = 'author_email', $committerName = 'committer_name', $committerEmail = 'committer_email', $message = 'message')
    {
        $commit = new Commit();

        return $commit
        ->setId($id)
        ->setAuthorName($authorName)
        ->setAuthorEmail($authorEmail)
        ->setCommitterName($committerName)
        ->setCommitterEmail($committerEmail)
        ->setMessage($message);
    }

    // getBranch()

    /**
     * @test
     */
    public function getBranch()
    {
        $this->assertEquals($this->branchName, $this->object->getBranch());
    }

    // getHead()

    /**
     * @test
     */
    public function getHead()
    {
        $this->assertSame($this->commit, $this->object->getHead());
    }

    // getRemotes()

    /**
     * @test
     */
    public function getRemotes()
    {
        $this->assertSame(array($this->remote), $this->object->getRemotes());
    }

    // toArray()

    /**
     * @test
     */
    public function toArray()
    {
        $expected = array(
            'branch'  => $this->branchName,
            'head'    => $this->commit->toArray(),
            'remotes' => array($this->remote->toArray()),
        );

        $this->assertSame($expected, $this->object->toArray());
        $this->assertSame(json_encode($expected), (string)$this->object);
    }
}
