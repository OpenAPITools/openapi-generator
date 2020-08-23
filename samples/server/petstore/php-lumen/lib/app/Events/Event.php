<?php

/**
 * The Lumen framework is open-sourced software licensed under the [MIT license](https://opensource.org/licenses/MIT).
 */

namespace App\Events;

use Illuminate\Queue\SerializesModels;

abstract class Event
{
    use SerializesModels;
}
