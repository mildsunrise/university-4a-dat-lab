angular.module('homeView').component('homeView', {
    templateUrl: 'home-view/home-view.template.html',
    controller: function HomeViewController($http, $location, $routeParams) {
        $http.get(`${API_BASE}/user`).then(response => {
            this.user = response.data.name ? response.data : null
        })
        const THEMES_URL = `${API_BASE}/themes`
        $http.get(THEMES_URL).then(response => {
            this.themes = response.data.items
        })
        
        this.createTheme = makeFormHandler(
            newtheme => $http.post(THEMES_URL, newtheme),
            response => $location.url(`/themes/${response.data.id}`),
        )
    }
})
