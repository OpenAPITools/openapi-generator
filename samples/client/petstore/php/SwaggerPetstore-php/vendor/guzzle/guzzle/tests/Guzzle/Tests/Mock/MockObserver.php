<?php

namespace Guzzle\Tests\Mock;

use Guzzle\Common\Event;
use Symfony\Component\EventDispatcher\EventSubscriberInterface;

class MockObserver implements \Countable, EventSubscriberInterface
{
    public $events = array();

    public static function getSubscribedEvents()
    {
        return array();
    }

    public function has($eventName)
    {
        foreach ($this->events as $event) {
            if ($event->getName() == $eventName) {
                return true;
            }
        }

        return false;
    }

    public function getLastEvent()
    {
        return end($this->events);
    }

    public function count()
    {
        return count($this->events);
    }

    public function getGrouped()
    {
        $events = array();
        foreach ($this->events as $event) {
            if (!isset($events[$event->getName()])) {
                $events[$event->getName()] = array();
            }
            $events[$event->getName()][] = $event;
        }

        return $events;
    }

    public function getData($event, $key, $occurrence = 0)
    {
        $grouped = $this->getGrouped();
        if (isset($grouped[$event])) {
            return $grouped[$event][$occurrence][$key];
        }

        return null;
    }

    public function update(Event $event)
    {
        $this->events[] = $event;
    }
}
