'use strict';
myApp.controller('SwaggerController', [ '$scope', '$window',
		function($scope, $window) {

			var self = this;

			self.openSwaggerCatalog = function() {
				loadUrl("/swagger-ui.html");
			};
			self.openSwaggerUI = function() {
				loadUrl("/swagger-ui/index.html");
			};
			self.openSwaggerEUI = function() {
				loadUrl("/swagger-ui/swaggerex-ui.html");
			};
			self.openSwaggerEditor = function() {
				loadUrl("/swagger-editor/index.html")
			};
			
			function loadUrl(url) {
				$window.open(url, "_blank", "location=no,toolbar=yes,scrollbars=yes,resizable=yes,top=500,left=500,width=1000,height=600");
			}
		} ]);
