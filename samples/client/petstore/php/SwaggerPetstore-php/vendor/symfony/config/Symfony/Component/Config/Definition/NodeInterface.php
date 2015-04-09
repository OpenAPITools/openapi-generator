<?php

/*
 * This file is part of the Symfony package.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Symfony\Component\Config\Definition;

/**
 * Common Interface among all nodes.
 *
 * In most cases, it is better to inherit from BaseNode instead of implementing
 * this interface yourself.
 *
 * @author Johannes M. Schmitt <schmittjoh@gmail.com>
 */
interface NodeInterface
{
    /**
     * Returns the name of the node.
     *
     * @return string The name of the node
     */
    public function getName();

    /**
     * Returns the path of the node.
     *
     * @return string The node path
     */
    public function getPath();

    /**
     * Returns true when the node is required.
     *
     * @return bool If the node is required
     */
    public function isRequired();

    /**
     * Returns true when the node has a default value.
     *
     * @return bool If the node has a default value
     */
    public function hasDefaultValue();

    /**
     * Returns the default value of the node.
     *
     * @return mixed The default value
     *
     * @throws \RuntimeException if the node has no default value
     */
    public function getDefaultValue();

    /**
     * Normalizes the supplied value.
     *
     * @param mixed $value The value to normalize
     *
     * @return mixed The normalized value
     */
    public function normalize($value);

    /**
     * Merges two values together.
     *
     * @param mixed $leftSide
     * @param mixed $rightSide
     *
     * @return mixed The merged values
     */
    public function merge($leftSide, $rightSide);

    /**
     * Finalizes a value.
     *
     * @param mixed $value The value to finalize
     *
     * @return mixed The finalized value
     */
    public function finalize($value);
}
