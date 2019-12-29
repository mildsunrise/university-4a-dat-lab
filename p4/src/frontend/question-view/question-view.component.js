angular.module('questionView').component('questionView', {
    templateUrl: 'question-view/question-view.template.html',
    controller: function QuestionViewController($http, $location, $routeParams, $q, $scope) {
        const { themeId, questionId } = $routeParams
        $http.get(`${API_BASE}/user`).then(response => {
            this.user = response.data.name ? response.data : null
        })
        const THEME_URL = `${API_BASE}/themes/${themeId}`
        const QUESTION_URL = `${API_BASE}/questions/${questionId}`
        const ANSWERS_URL = `${QUESTION_URL}/answers`
        $http.get(THEME_URL).then(response => {
            this.theme = response.data
        })
        $http.get(QUESTION_URL).then(response => {
            this.question = response.data
            if (this.question.theme !== themeId)
                return $location.url(`/themes/${this.question.themeId}/qs/${questionId}`)
        }, response => {
            if (response.status === 404)
                return $location.url('/')
        })
        $http.get(ANSWERS_URL).then(response => {
            this.answers = response.data.items
        })

        $scope.aids = {}
        this.deleteAnswers = makeFormHandler(
            aids => $q.all(Object.keys(aids).filter(k => aids[k]).map(
                aid => $http.delete(`${API_BASE}/answers/${aid}`).catch(x => x)
            )),
            _ => $http.get(ANSWERS_URL).then(response => {
                this.answers = response.data.items
            }),
        )
        this.createAnswer = makeFormHandler(
            newanswer => $http.post(ANSWERS_URL, newanswer),
            response => this.answers.push(response.data),
        )
    }
})
