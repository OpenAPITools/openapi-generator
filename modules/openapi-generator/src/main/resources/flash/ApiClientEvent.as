package io.swagger.event {
import io.swagger.event.Response;

import flash.events.Event;

/**
 * Event dispatched by the SDK to communicate success events and failure events.
 * If a custom dispatcher has been assigned by the consumer on the generated client then the dispatcher dispatches
 * the ApiClientEvent to indicate success or failure of the invocation using the Response
 */
public class ApiClientEvent extends Event{

    /**
     * Event type to indicate a unsuccessful invocation
     */
    public static const FAILURE_EVENT:String = "unsuccesfulInvocation";

    /**
     * Event type to indicate a successful invocation
     */
    public static const SUCCESS_EVENT:String = "successfulInvocation";
    
    /**
     * The Response object which contains response info
     */
    public var response: Response;
    /**
     * Any additional info
     */
    public var message:String;

    public function ApiClientEvent(type:String,bubbles:Boolean = false,cancelable:Boolean = false) {
        super(type, bubbles, cancelable);
    }
}
}