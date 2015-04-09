<?php
function functionCallback()
{
    $args = func_get_args();

    if ($args == array('foo', 'bar')) {
        return 'pass';
    }
}
