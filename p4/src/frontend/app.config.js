angular.module('forumApp').config(['$routeProvider',
    function config($routeProvider) {
        $routeProvider
            .when('/', {
                template: '<home-view></home-view>'
            })
            .when('/themes/:themeId', {
                template: '<theme-view></theme-view>'
            })
            .when('/themes/:themeId/qs/:questionId', {
                template: '<question-view></question-view>'
            })
            //.otherwise('/')
    }
])
