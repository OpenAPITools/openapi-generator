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
 * Prints TestDox documentation in HTML format.
 *
 * @package    PHPUnit
 * @subpackage Util_TestDox
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 2.1.0
 */
class PHPUnit_Util_TestDox_ResultPrinter_HTML extends PHPUnit_Util_TestDox_ResultPrinter
{
    /**
     * @var    boolean
     */
    protected $printsHTML = true;

    /**
     * Handler for 'start run' event.
     *
     */
    protected function startRun()
    {
        $this->write('<html><body>');
    }

    /**
     * Handler for 'start class' event.
     *
     * @param string $name
     */
    protected function startClass($name)
    {
        $this->write(
            '<h2 id="' . $name . '">' . $this->currentTestClassPrettified .
            '</h2><ul>'
        );
    }

    /**
     * Handler for 'on test' event.
     *
     * @param string  $name
     * @param boolean $success
     */
    protected function onTest($name, $success = true)
    {
        if (!$success) {
            $strikeOpen  = '<span style="text-decoration:line-through;">';
            $strikeClose = '</span>';
        } else {
            $strikeOpen  = '';
            $strikeClose = '';
        }

        $this->write('<li>' . $strikeOpen . $name . $strikeClose . '</li>');
    }

    /**
     * Handler for 'end class' event.
     *
     * @param string $name
     */
    protected function endClass($name)
    {
        $this->write('</ul>');
    }

    /**
     * Handler for 'end run' event.
     *
     */
    protected function endRun()
    {
        $this->write('</body></html>');
    }
}
