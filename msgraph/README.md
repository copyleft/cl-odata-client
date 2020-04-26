## MSGRAPH

MSGRAPH is a library for accessing Microsoft Graph API.

It uses CL-OData library.

Have a look at the demo applications:

* [Contacts app](contacts-app.lisp)
* [Mail app](mail-app.lisp)
* [Calendar app](calendar-app.lisp)

Some important comments:

* The library expects MS Graph credentials in `msgraph::*credentials*`, with: `:appid`, `:tenantid`, `:appname`, `:client-secret`.
* Token management is transparent for HTTP requests. When doing a request to MS API, if no token is found, a token is retrieved before the request. If the token is expired, another one is fetched.
* Instead of `odata/lang::fetch` and `odata/lang::post`, `msgraph::fetch` and `msgraph::post` should be used, that implement the authorization management. It is important to `:use` `:msgraph` *before* `:odata/lang` in the package definition, like in `contacts-app` package, so that the correct `msgraph` functions are used.
