function initOAuthPorts(app) {
    app.ports.initAuth.subscribe(initAuth);
    app.ports.startAuth.subscribe(startAuth);

    var initialized = false;
    var authWindow = null;

    function initAuth() {
        if (!initialized) {
            console.log("OAuth: initializing");
            window.addEventListener("message", receiveMessage, false);
            initialized = true;
            app.ports.initialized.send(true);
        }
    }

    function startAuth(url) {
        console.log("OAuth: starting auth: ", url);
        if (authWindow) {
            authWindow.close();
        }
        authWindow = window.open(url, "auth", "height=800,width=600,menubar=no,status=no,titlebar=no,toolbar=no");
    }

    function receiveMessage(event) {
        console.log("OAuth: message received: ", event.data);
        if (event.origin !== window.location.origin) {
            return;
        }

        if (event.data.status === "success") {
            app.ports.authComplete.send([true, event.data.access_token]);
        } else {
            app.ports.authComplete.send([false, event.data.status || "Unknown"]);
        }
    }
}
