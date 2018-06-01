(function () {
    'use strict';
	myApp.config(config)
        .run(run);

    function config($stateProvider, $urlRouterProvider) {
        // default route
        $urlRouterProvider.otherwise("/");

        // app routes
        $stateProvider
            .state('mock', {
                url: '/mock',
                templateUrl: '/static/home/mock.html',
            })
            
            .state('swagger', {
                url: '/swagger',
                templateUrl: '/static/home/swagger.html',
            })
        
            .state('utils', {
                url: '/utils',
                templateUrl: '/static/home/utils.html',
            })
        
           }

    function run() {
    }
})();