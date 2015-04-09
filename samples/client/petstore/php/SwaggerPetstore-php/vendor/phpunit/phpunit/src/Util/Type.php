<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Utility class for textual type (and value) representation.
 *
 * @package    PHPUnit
 * @subpackage Util
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.0.0
 */
class PHPUnit_Util_Type
{
    public static function isType($type)
    {
        return in_array(
            $type,
            array(
            'numeric',
            'integer',
            'int',
            'float',
            'string',
            'boolean',
            'bool',
            'null',
            'array',
            'object',
            'resource',
            'scalar'
            )
        );
    }
}
