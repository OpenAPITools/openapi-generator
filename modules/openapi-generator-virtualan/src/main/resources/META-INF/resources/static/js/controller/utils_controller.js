'use strict';
myApp.controller('UtilsController', [ '$scope', '$filter', '$window',
		function($scope, $filter, $window) {

			var self = this;
			self.input = "Enter the Json here to get formatted";
			self.output = "You will see the Formatted Output or Error";
			self.jsonFormatter = function() {
				try {
					self.output = JSON.parse(self.input);
				} catch (throw_error) {
					self.output = throw_error.message;
				}
			}

		} ]);
