<?php

/*
 * This file is part of the Symfony package.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Symfony\Component\Console\Question;

/**
 * Represents a yes/no question.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
class ConfirmationQuestion extends Question
{
    /**
     * Constructor.
     *
     * @param string $question The question to ask to the user
     * @param bool   $default  The default answer to return, true or false
     */
    public function __construct($question, $default = true)
    {
        parent::__construct($question, (bool) $default);

        $this->setNormalizer($this->getDefaultNormalizer());
    }

    /**
     * Returns the default answer normalizer.
     *
     * @return callable
     */
    private function getDefaultNormalizer()
    {
        $default = $this->getDefault();

        return function ($answer) use ($default) {
            if (is_bool($answer)) {
                return $answer;
            }

            if (false === $default) {
                return $answer && 'y' === strtolower($answer[0]);
            }

            return !$answer || 'y' === strtolower($answer[0]);
        };
    }
}
