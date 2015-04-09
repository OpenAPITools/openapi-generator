<?php
/**
 * GlobalState
 *
 * Copyright (c) 2001-2014, Sebastian Bergmann <sebastian@phpunit.de>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *
 *   * Neither the name of Sebastian Bergmann nor the names of his
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  2001-2014 Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.github.com/sebastianbergmann/global-state
 */

namespace SebastianBergmann\GlobalState;

use ReflectionClass;

/**
 * A blacklist for global state elements that should not be snapshotted.
 *
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  2001-2014 Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.github.com/sebastianbergmann/global-state
 */
class Blacklist
{
    /**
     * @var array
     */
    private $globalVariables = array();

    /**
     * @var array
     */
    private $classes = array();

    /**
     * @var array
     */
    private $classNamePrefixes = array();

    /**
     * @var array
     */
    private $parentClasses = array();

    /**
     * @var array
     */
    private $interfaces = array();

    /**
     * @var array
     */
    private $staticAttributes = array();

    /**
     * @param string $variableName
     */
    public function addGlobalVariable($variableName)
    {
        $this->globalVariables[$variableName] = true;
    }

    /**
     * @param string $className
     */
    public function addClass($className)
    {
        $this->classes[] = $className;
    }

    /**
     * @param string $className
     */
    public function addSubclassesOf($className)
    {
        $this->parentClasses[] = $className;
    }

    /**
     * @param string $interfaceName
     */
    public function addImplementorsOf($interfaceName)
    {
        $this->interfaces[] = $interfaceName;
    }

    /**
     * @param string $classNamePrefix
     */
    public function addClassNamePrefix($classNamePrefix)
    {
        $this->classNamePrefixes[] = $classNamePrefix;
    }

    /**
     * @param string $className
     * @param string $attributeName
     */
    public function addStaticAttribute($className, $attributeName)
    {
        if (!isset($this->staticAttributes[$className])) {
            $this->staticAttributes[$className] = array();
        }

        $this->staticAttributes[$className][$attributeName] = true;
    }

    /**
     * @param  string $variableName
     * @return boolean
     */
    public function isGlobalVariableBlacklisted($variableName)
    {
        return isset($this->globalVariables[$variableName]);
    }

    /**
     * @param  string $className
     * @param  string $attributeName
     * @return boolean
     */
    public function isStaticAttributeBlacklisted($className, $attributeName)
    {
        if (in_array($className, $this->classes)) {
            return true;
        }

        foreach ($this->classNamePrefixes as $prefix) {
            if (strpos($className, $prefix) === 0) {
                return true;
            }
        }

        $class = new ReflectionClass($className);

        foreach ($this->parentClasses as $type) {
            if ($class->isSubclassOf($type)) {
                return true;
            }
        }

        foreach ($this->interfaces as $type) {
            if ($class->implementsInterface($type)) {
                return true;
            }
        }

        if (isset($this->staticAttributes[$className][$attributeName])) {
            return true;
        }

        return false;
    }
}
