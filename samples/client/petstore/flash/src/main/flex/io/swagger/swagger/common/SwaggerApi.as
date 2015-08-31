package io.swagger.common
{
	import io.swagger.common.ApiUserCredentials;
	
	import flash.events.EventDispatcher;
	import flash.events.IEventDispatcher;
	
	import mx.utils.UIDUtil;
	
	public class SwaggerApi extends EventDispatcher
	{
			
		protected var _apiUsageCredentials:ApiUserCredentials;
		protected var _apiEventNotifier:EventDispatcher;
		protected var _apiInvoker: ApiInvoker;
		
		protected var _useProxyServer: Boolean = false;

		
		/**
		 * Constructor for the api client
		 * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
		 * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
		 */
		public function SwaggerApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
			super();
			_apiUsageCredentials = apiCredentials;
			_apiEventNotifier = eventDispatcher;
		}
		
		public function useProxyServer(value:Boolean, proxyServerUrl: String = null):void {
			_useProxyServer = value;
		}
		
		protected function getApiInvoker():ApiInvoker {
			if(_apiInvoker == null){
				if(_apiEventNotifier == null){
					_apiEventNotifier = this;
				}
				_apiInvoker = new ApiInvoker(_apiUsageCredentials, _apiEventNotifier, _useProxyServer);
			}
			return _apiInvoker;
		}
		
		protected function getUniqueId():String {
			return UIDUtil.createUID();
		}
		
		/**
		 * Method for returning the path value
		 * For a string value an empty value is returned if the value is null
		 * @param value
		 * @return
		 */
		protected static function toPathValue(value: Object): String {
			if(value is Array){
				return arrayToPathValue(value as Array);
			}
			return  value == null ? "" : value.toString();
		}
		
		/**
		 * Method for returning a path value
		 * For a list of objects a comma separated string is returned
		 * @param objects
		 * @return
		 */
		protected static function arrayToPathValue(objects: Array): String {
			var out: String = "";
			
			return objects.join(",");
		}
		
	}
}