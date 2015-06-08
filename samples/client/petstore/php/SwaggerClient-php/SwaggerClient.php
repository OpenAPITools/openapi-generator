<?php
// load models defined for endpoints
foreach (glob(dirname(__FILE__)."/lib/models/*.php") as $filename)
{
      require_once $filename;
}

// load classes for accessing the endpoints
foreach (glob(dirname(__FILE__)."/lib/*.php") as $filename)
{
      require_once $filename;
}
?>
