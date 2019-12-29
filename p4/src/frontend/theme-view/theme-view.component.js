angular.module('themeView').component('themeView', {
    templateUrl: 'theme-view/theme-view.template.html',
    controller: function ThemeViewController($http, $location, $routeParams, $q, $scope) {
        const { themeId } = $routeParams
        $http.get(`${API_BASE}/user`).then(response => {
            this.user = response.data.name ? response.data : null
        })
        const THEME_URL = `${API_BASE}/themes/${themeId}`
        const QUESTIONS_URL = `${THEME_URL}/questions`
        $http.get(THEME_URL).then(response => {
            this.theme = response.data
            $scope.newtheme = { ...this.theme }
        }, response => {
            if (response.status === 404)
                return $location.url('/')
        })
        $http.get(QUESTIONS_URL).then(response => {
            this.questions = response.data.items
        })

        $scope.qids = {}
        this.deleteQuestions = makeFormHandler(
            qids => $q.all(Object.keys(qids).filter(k => qids[k]).map(
                qid => $http.delete(`${API_BASE}/questions/${qid}`).catch(x => x)
            )),
            _ => $http.get(QUESTIONS_URL).then(response => {
                this.questions = response.data.items
            }),
        )
        this.createQuestion = makeFormHandler(
            newquestion => $http.post(QUESTIONS_URL, newquestion),
            response => $location.url(`/themes/${themeId}/qs/${response.data.id}`),
        )
        this.updateTheme = makeFormHandler(
            newtheme => $http.put(THEME_URL, newtheme),
            response => this.theme = response.data,
            false,
        )
    }
})
