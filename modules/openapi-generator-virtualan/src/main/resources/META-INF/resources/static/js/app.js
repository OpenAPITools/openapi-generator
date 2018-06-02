'use strict';
var myApp =  angular
        .module('myApp', ['ui.router', 'ngAnimate', 'ngMaterial', 'ui.bootstrap', 'angular-json-tree' ]);

myApp.filter('start', function () {
	  return function (input, start) {
	    if (!input || !input.length) { return; }
	    start = +start;
	    return input.slice(start);
	  };
	});
