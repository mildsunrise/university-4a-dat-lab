angular.module('forumApp', [
    'ngRoute',
    'homeView',
    'themeView',
    'questionView',
])

angular.module('forumApp').controller('forumApp', function ForumAppController($scope, $http) {
    $scope.API_BASE = API_BASE
    $http.get(`${API_BASE}/user`).then(response => {
        $scope.user = response.data.name ? response.data : null
    })
})
