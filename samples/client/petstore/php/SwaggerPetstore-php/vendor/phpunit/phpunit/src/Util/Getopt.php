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
 * Command-line options parsing class.
 *
 * @package    PHPUnit
 * @subpackage Util
 * @author     Andrei Zmievski <andrei@php.net>
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.0.0
 */
class PHPUnit_Util_Getopt
{
    public static function getopt(array $args, $short_options, $long_options = null)
    {
        if (empty($args)) {
            return array(array(), array());
        }

        $opts     = array();
        $non_opts = array();

        if ($long_options) {
            sort($long_options);
        }

        if (isset($args[0][0]) && $args[0][0] != '-') {
            array_shift($args);
        }

        reset($args);
        array_map('trim', $args);

        while (list($i, $arg) = each($args)) {
            if ($arg == '') {
                continue;
            }

            if ($arg == '--') {
                $non_opts = array_merge($non_opts, array_slice($args, $i + 1));
                break;
            }

            if ($arg[0] != '-' ||
                (strlen($arg) > 1 && $arg[1] == '-' && !$long_options)) {
                $non_opts = array_merge($non_opts, array_slice($args, $i));
                break;
            } elseif (strlen($arg) > 1 && $arg[1] == '-') {
                self::parseLongOption(
                    substr($arg, 2),
                    $long_options,
                    $opts,
                    $args
                );
            } else {
                self::parseShortOption(
                    substr($arg, 1),
                    $short_options,
                    $opts,
                    $args
                );
            }
        }

        return array($opts, $non_opts);
    }

    protected static function parseShortOption($arg, $short_options, &$opts, &$args)
    {
        $argLen = strlen($arg);

        for ($i = 0; $i < $argLen; $i++) {
            $opt     = $arg[$i];
            $opt_arg = null;

            if (($spec = strstr($short_options, $opt)) === false ||
                $arg[$i] == ':') {
                throw new PHPUnit_Framework_Exception(
                    "unrecognized option -- $opt"
                );
            }

            if (strlen($spec) > 1 && $spec[1] == ':') {
                if (strlen($spec) > 2 && $spec[2] == ':') {
                    if ($i + 1 < $argLen) {
                        $opts[] = array($opt, substr($arg, $i + 1));
                        break;
                    }
                } else {
                    if ($i + 1 < $argLen) {
                        $opts[] = array($opt, substr($arg, $i + 1));
                        break;
                    } elseif (list(, $opt_arg) = each($args)) {
                    } else {
                        throw new PHPUnit_Framework_Exception(
                            "option requires an argument -- $opt"
                        );
                    }
                }
            }

            $opts[] = array($opt, $opt_arg);
        }
    }

    protected static function parseLongOption($arg, $long_options, &$opts, &$args)
    {
        $count   = count($long_options);
        $list    = explode('=', $arg);
        $opt     = $list[0];
        $opt_arg = null;

        if (count($list) > 1) {
            $opt_arg = $list[1];
        }

        $opt_len = strlen($opt);

        for ($i = 0; $i < $count; $i++) {
            $long_opt  = $long_options[$i];
            $opt_start = substr($long_opt, 0, $opt_len);

            if ($opt_start != $opt) {
                continue;
            }

            $opt_rest = substr($long_opt, $opt_len);

            if ($opt_rest != '' && $opt[0] != '=' && $i + 1 < $count &&
                $opt == substr($long_options[$i+1], 0, $opt_len)) {
                throw new PHPUnit_Framework_Exception(
                    "option --$opt is ambiguous"
                );
            }

            if (substr($long_opt, -1) == '=') {
                if (substr($long_opt, -2) != '==') {
                    if (!strlen($opt_arg) &&
                        !(list(, $opt_arg) = each($args))) {
                        throw new PHPUnit_Framework_Exception(
                            "option --$opt requires an argument"
                        );
                    }
                }
            } elseif ($opt_arg) {
                throw new PHPUnit_Framework_Exception(
                    "option --$opt doesn't allow an argument"
                );
            }

            $full_option = '--' . preg_replace('/={1,2}$/', '', $long_opt);
            $opts[] = array($full_option, $opt_arg);

            return;
        }

        throw new PHPUnit_Framework_Exception("unrecognized option --$opt");
    }
}
