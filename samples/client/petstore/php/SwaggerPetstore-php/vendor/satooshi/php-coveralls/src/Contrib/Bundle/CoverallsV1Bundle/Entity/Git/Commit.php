<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Entity\Git;

use Contrib\Bundle\CoverallsV1Bundle\Entity\Coveralls;

/**
 * Commit info.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class Commit extends Coveralls
{
    /**
     * Commit ID.
     *
     * @var string
     */
    protected $id;

    /**
     * Author name.
     *
     * @var string
     */
    protected $authorName;

    /**
     * Author email.
     *
     * @var string
     */
    protected $authorEmail;

    /**
     * Committer name.
     *
     * @var string
     */
    protected $committerName;

    /**
     * Committer email.
     *
     * @var string
     */
    protected $committerEmail;

    /**
     * Commit message.
     *
     * @var string
     */
    protected $message;

    // API

    /**
     * {@inheritdoc}
     *
     * @see \Contrib\Bundle\CoverallsBundle\Entity\ArrayConvertable::toArray()
     */
    public function toArray()
    {
        return array(
            'id'              => $this->id,
            'author_name'     => $this->authorName,
            'author_email'    => $this->authorEmail,
            'committer_name'  => $this->committerName,
            'committer_email' => $this->committerEmail,
            'message'         => $this->message,
        );
    }

    // accessor

    /**
     * Set commit ID.
     *
     * @param  string                                              $id
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit
     */
    public function setId($id)
    {
        $this->id = $id;

        return $this;
    }

    /**
     * Return commit ID.
     *
     * @return string|null
     */
    public function getId()
    {
        if (isset($this->id)) {
            return $this->id;
        }

        return null;
    }

    /**
     * Set author name.
     *
     * @param  string                                              $authorName
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit
     */
    public function setAuthorName($authorName)
    {
        $this->authorName = $authorName;

        return $this;
    }

    /**
     * Return author name.
     *
     * @return string|null
     */
    public function getAuthorName()
    {
        if (isset($this->authorName)) {
            return $this->authorName;
        }

        return null;
    }

    /**
     * Set author email.
     *
     * @param  string                                              $authorEmail
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit
     */
    public function setAuthorEmail($authorEmail)
    {
        $this->authorEmail = $authorEmail;

        return $this;
    }

    /**
     * Return author email.
     *
     * @return string|null
     */
    public function getAuthorEmail()
    {
        if (isset($this->authorEmail)) {
            return $this->authorEmail;
        }

        return null;
    }

    /**
     * Set committer name.
     *
     * @param  string                                              $committerName
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit
     */
    public function setCommitterName($committerName)
    {
        $this->committerName = $committerName;

        return $this;
    }

    /**
     * Return committer name.
     *
     * @return string|null
     */
    public function getCommitterName()
    {
        if (isset($this->committerName)) {
            return $this->committerName;
        }

        return null;
    }

    /**
     * Set committer email.
     *
     * @param  string                                              $committerEmail
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit
     */
    public function setCommitterEmail($committerEmail)
    {
        $this->committerEmail = $committerEmail;

        return $this;
    }

    /**
     * Return committer email.
     *
     * @return string|null
     */
    public function getCommitterEmail()
    {
        if (isset($this->committerEmail)) {
            return $this->committerEmail;
        }

        return null;
    }

    /**
     * Set commit message.
     *
     * @param  string                                              $message
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit
     */
    public function setMessage($message)
    {
        $this->message = $message;

        return $this;
    }

    /**
     * Return commit message.
     *
     * @return string|null
     */
    public function getMessage()
    {
        if (isset($this->message)) {
            return $this->message;
        }

        return null;
    }
}
