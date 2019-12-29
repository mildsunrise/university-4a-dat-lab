// FIXME: put this constant inside service, cache user
const API_BASE = 'forum-backend.cgi'

// FIXME: put into service, use $q to cast action() to promise
function makeFormHandler(action, success, reset=true) {
    const form = {
        requesting: false,
        submit: (data, f) => {
            if (form.requesting) return
            form.requesting = true
            delete form.error
            action(data).then(result => {
                if (reset)
                    Object.keys(data).forEach(k => { delete data[k] })
                return result
            }).then(success, response => {
                form.error = response.data
            }).finally(() => {
                form.requesting = false
            })
        }
    }
    return form
}
