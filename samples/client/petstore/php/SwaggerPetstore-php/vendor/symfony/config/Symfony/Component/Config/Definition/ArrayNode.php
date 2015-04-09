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

use Symfony\Component\Config\Definition\Exception\InvalidConfigurationException;
use Symfony\Component\Config\Definition\Exception\InvalidTypeException;
use Symfony\Component\Config\Definition\Exception\UnsetKeyException;

/**
 * Represents an Array node in the config tree.
 *
 * @author Johannes M. Schmitt <schmittjoh@gmail.com>
 */
class ArrayNode extends BaseNode implements PrototypeNodeInterface
{
    protected $xmlRemappings = array();
    protected $children = array();
    protected $allowFalse = false;
    protected $allowNewKeys = true;
    protected $addIfNotSet = false;
    protected $performDeepMerging = true;
    protected $ignoreExtraKeys = false;
    protected $normalizeKeys = true;

    public function setNormalizeKeys($normalizeKeys)
    {
        $this->normalizeKeys = (bool) $normalizeKeys;
    }

    /**
     * Normalizes keys between the different configuration formats.
     *
     * Namely, you mostly have foo_bar in YAML while you have foo-bar in XML.
     * After running this method, all keys are normalized to foo_bar.
     *
     * If you have a mixed key like foo-bar_moo, it will not be altered.
     * The key will also not be altered if the target key already exists.
     *
     * @param mixed $value
     *
     * @return array The value with normalized keys
     */
    protected function preNormalize($value)
    {
        if (!$this->normalizeKeys || !is_array($value)) {
            return $value;
        }

        foreach ($value as $k => $v) {
            if (false !== strpos($k, '-') && false === strpos($k, '_') && !array_key_exists($normalizedKey = str_replace('-', '_', $k), $value)) {
                $value[$normalizedKey] = $v;
                unset($value[$k]);
            }
        }

        return $value;
    }

    /**
     * Retrieves the children of this node.
     *
     * @return array The children
     */
    public function getChildren()
    {
        return $this->children;
    }

    /**
     * Sets the xml remappings that should be performed.
     *
     * @param array $remappings an array of the form array(array(string, string))
     */
    public function setXmlRemappings(array $remappings)
    {
        $this->xmlRemappings = $remappings;
    }

    /**
     * Gets the xml remappings that should be performed.
     *
     * @return array $remappings an array of the form array(array(string, string))
     */
    public function getXmlRemappings()
    {
        return $this->xmlRemappings;
    }

    /**
     * Sets whether to add default values for this array if it has not been
     * defined in any of the configuration files.
     *
     * @param bool $boolean
     */
    public function setAddIfNotSet($boolean)
    {
        $this->addIfNotSet = (bool) $boolean;
    }

    /**
     * Sets whether false is allowed as value indicating that the array should be unset.
     *
     * @param bool $allow
     */
    public function setAllowFalse($allow)
    {
        $this->allowFalse = (bool) $allow;
    }

    /**
     * Sets whether new keys can be defined in subsequent configurations.
     *
     * @param bool $allow
     */
    public function setAllowNewKeys($allow)
    {
        $this->allowNewKeys = (bool) $allow;
    }

    /**
     * Sets if deep merging should occur.
     *
     * @param bool $boolean
     */
    public function setPerformDeepMerging($boolean)
    {
        $this->performDeepMerging = (bool) $boolean;
    }

    /**
     * Whether extra keys should just be ignore without an exception.
     *
     * @param bool $boolean To allow extra keys
     */
    public function setIgnoreExtraKeys($boolean)
    {
        $this->ignoreExtraKeys = (bool) $boolean;
    }

    /**
     * Sets the node Name.
     *
     * @param string $name The node's name
     */
    public function setName($name)
    {
        $this->name = $name;
    }

    /**
     * Checks if the node has a default value.
     *
     * @return bool
     */
    public function hasDefaultValue()
    {
        return $this->addIfNotSet;
    }

    /**
     * Retrieves the default value.
     *
     * @return array The default value
     *
     * @throws \RuntimeException if the node has no default value
     */
    public function getDefaultValue()
    {
        if (!$this->hasDefaultValue()) {
            throw new \RuntimeException(sprintf('The node at path "%s" has no default value.', $this->getPath()));
        }

        $defaults = array();
        foreach ($this->children as $name => $child) {
            if ($child->hasDefaultValue()) {
                $defaults[$name] = $child->getDefaultValue();
            }
        }

        return $defaults;
    }

    /**
     * Adds a child node.
     *
     * @param NodeInterface $node The child node to add
     *
     * @throws \InvalidArgumentException when the child node has no name
     * @throws \InvalidArgumentException when the child node's name is not unique
     */
    public function addChild(NodeInterface $node)
    {
        $name = $node->getName();
        if (!strlen($name)) {
            throw new \InvalidArgumentException('Child nodes must be named.');
        }
        if (isset($this->children[$name])) {
            throw new \InvalidArgumentException(sprintf('A child node named "%s" already exists.', $name));
        }

        $this->children[$name] = $node;
    }

    /**
     * Finalizes the value of this node.
     *
     * @param mixed $value
     *
     * @return mixed The finalised value
     *
     * @throws UnsetKeyException
     * @throws InvalidConfigurationException if the node doesn't have enough children
     */
    protected function finalizeValue($value)
    {
        if (false === $value) {
            $msg = sprintf('Unsetting key for path "%s", value: %s', $this->getPath(), json_encode($value));
            throw new UnsetKeyException($msg);
        }

        foreach ($this->children as $name => $child) {
            if (!array_key_exists($name, $value)) {
                if ($child->isRequired()) {
                    $msg = sprintf('The child node "%s" at path "%s" must be configured.', $name, $this->getPath());
                    $ex = new InvalidConfigurationException($msg);
                    $ex->setPath($this->getPath());

                    throw $ex;
                }

                if ($child->hasDefaultValue()) {
                    $value[$name] = $child->getDefaultValue();
                }

                continue;
            }

            try {
                $value[$name] = $child->finalize($value[$name]);
            } catch (UnsetKeyException $unset) {
                unset($value[$name]);
            }
        }

        return $value;
    }

    /**
     * Validates the type of the value.
     *
     * @param mixed $value
     *
     * @throws InvalidTypeException
     */
    protected function validateType($value)
    {
        if (!is_array($value) && (!$this->allowFalse || false !== $value)) {
            $ex = new InvalidTypeException(sprintf(
                'Invalid type for path "%s". Expected array, but got %s',
                $this->getPath(),
                gettype($value)
            ));
            if ($hint = $this->getInfo()) {
                $ex->addHint($hint);
            }
            $ex->setPath($this->getPath());

            throw $ex;
        }
    }

    /**
     * Normalizes the value.
     *
     * @param mixed $value The value to normalize
     *
     * @return mixed The normalized value
     *
     * @throws InvalidConfigurationException
     */
    protected function normalizeValue($value)
    {
        if (false === $value) {
            return $value;
        }

        $value = $this->remapXml($value);

        $normalized = array();
        foreach ($value as $name => $val) {
            if (isset($this->children[$name])) {
                $normalized[$name] = $this->children[$name]->normalize($val);
                unset($value[$name]);
            }
        }

        // if extra fields are present, throw exception
        if (count($value) && !$this->ignoreExtraKeys) {
            $msg = sprintf('Unrecognized option%s "%s" under "%s"', 1 === count($value) ? '' : 's', implode(', ', array_keys($value)), $this->getPath());
            $ex = new InvalidConfigurationException($msg);
            $ex->setPath($this->getPath());

            throw $ex;
        }

        return $normalized;
    }

    /**
     * Remaps multiple singular values to a single plural value.
     *
     * @param array $value The source values
     *
     * @return array The remapped values
     */
    protected function remapXml($value)
    {
        foreach ($this->xmlRemappings as $transformation) {
            list($singular, $plural) = $transformation;

            if (!isset($value[$singular])) {
                continue;
            }

            $value[$plural] = Processor::normalizeConfig($value, $singular, $plural);
            unset($value[$singular]);
        }

        return $value;
    }

    /**
     * Merges values together.
     *
     * @param mixed $leftSide  The left side to merge.
     * @param mixed $rightSide The right side to merge.
     *
     * @return mixed The merged values
     *
     * @throws InvalidConfigurationException
     * @throws \RuntimeException
     */
    protected function mergeValues($leftSide, $rightSide)
    {
        if (false === $rightSide) {
            // if this is still false after the last config has been merged the
            // finalization pass will take care of removing this key entirely
            return false;
        }

        if (false === $leftSide || !$this->performDeepMerging) {
            return $rightSide;
        }

        foreach ($rightSide as $k => $v) {
            // no conflict
            if (!array_key_exists($k, $leftSide)) {
                if (!$this->allowNewKeys) {
                    $ex = new InvalidConfigurationException(sprintf(
                        'You are not allowed to define new elements for path "%s". '
                       .'Please define all elements for this path in one config file. '
                       .'If you are trying to overwrite an element, make sure you redefine it '
                       .'with the same name.',
                        $this->getPath()
                    ));
                    $ex->setPath($this->getPath());

                    throw $ex;
                }

                $leftSide[$k] = $v;
                continue;
            }

            if (!isset($this->children[$k])) {
                throw new \RuntimeException('merge() expects a normalized config array.');
            }

            $leftSide[$k] = $this->children[$k]->merge($leftSide[$k], $v);
        }

        return $leftSide;
    }
}
