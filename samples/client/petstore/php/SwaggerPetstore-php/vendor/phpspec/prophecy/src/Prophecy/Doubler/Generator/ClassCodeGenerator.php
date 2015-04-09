<?php

/*
 * This file is part of the Prophecy.
 * (c) Konstantin Kudryashov <ever.zet@gmail.com>
 *     Marcello Duarte <marcello.duarte@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Prophecy\Doubler\Generator;

/**
 * Class code creator.
 * Generates PHP code for specific class node tree.
 *
 * @author Konstantin Kudryashov <ever.zet@gmail.com>
 */
class ClassCodeGenerator
{
    /**
     * Generates PHP code for class node.
     *
     * @param string         $classname
     * @param Node\ClassNode $class
     *
     * @return string
     */
    public function generate($classname, Node\ClassNode $class)
    {
        $parts     = explode('\\', $classname);
        $classname = array_pop($parts);
        $namespace = implode('\\', $parts);

        $code = sprintf("class %s extends \%s implements %s {\n",
            $classname, $class->getParentClass(), implode(', ',
                array_map(function ($interface) {return '\\'.$interface;}, $class->getInterfaces())
            )
        );

        foreach ($class->getProperties() as $name => $visibility) {
            $code .= sprintf("%s \$%s;\n", $visibility, $name);
        }
        $code .= "\n";

        foreach ($class->getMethods() as $method) {
            $code .= $this->generateMethod($method)."\n";
        }
        $code .= "\n}";

        return sprintf("namespace %s {\n%s\n}", $namespace, $code);
    }

    private function generateMethod(Node\MethodNode $method)
    {
        $php = sprintf("%s %s function %s%s(%s) {\n",
            $method->getVisibility(),
            $method->isStatic() ? 'static' : '',
            $method->returnsReference() ? '&':'',
            $method->getName(),
            implode(', ', $this->generateArguments($method->getArguments()))
        );
        $php .= $method->getCode()."\n";

        return $php.'}';
    }

    private function generateArguments(array $arguments)
    {
        return array_map(function (Node\ArgumentNode $argument) {
            $php = '';

            if ($hint = $argument->getTypeHint()) {
                if ('array' === $hint || 'callable' === $hint) {
                    $php .= $hint;
                } else {
                    $php .= '\\'.$hint;
                }
            }

            $php .= ' '.($argument->isPassedByReference() ? '&' : '').'$'.$argument->getName();

            if ($argument->isOptional()) {
                $php .= ' = '.var_export($argument->getDefault(), true);
            }

            return $php;
        }, $arguments);
    }
}
