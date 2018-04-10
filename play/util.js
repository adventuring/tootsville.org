if (! Tootsville.util) { Tootsville.util = {}; }

Tootsville.util.postJSONforJSON = function (uri, post, headers) {
    if (!post) { post = {}; }
    if (!headers) { headers = {}; }
    return new Promise( (pass, fail) => {
        let xhr = new XmlHttpRequest;
        xhr.open('POST', uri);
        xhr.onload = (response) => {
            pass(JSON.parse(response.body));
        };
        xhr.onerror = (failure) => { fail(failure); };
        for (header in headers) {
            if (headers.hasOwnProperty(header)) {
                xhr.setRequestHeader(header, headers[header]);
            }
        }
        xhr.setRequestHeader('Accept', 'application/json;encoding=utf-8');
        xhr.setRequestHeader('Content-Type', 'application/json;encoding=utf-8');
        xhr.send(JSON.strigify(post));
        });
};

Tootsville.util.assertValidHost = function (hostName) {
    return (['users', 'gossip', 'indira'].indexOf(hostName) > -1)
    }

Tootsville.util.rest = function (uri, post, headers) {
    var hostName = uri.split('/')[1];
    Tootsville.util.assertValidHostName(hostName);
    return Tootsville.util.postJSONforJSON('https://' + hostName + '.' + Tootsville.cluster + uri,
                                           post, headers);
};

Tootsville.util.loadScript = function (src) {
        return new Promise( finish => {
            var el = document.createElement('SCRIPT');
            el.onload = finish;
            el.src = src;
            document.body.appendChild(el);
        });
};
