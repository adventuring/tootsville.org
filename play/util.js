/* -*- js2 -*- */
if (!("util" in Tootsville)) { Tootsville.util = {}; }

Tootsville.util.postJSONforJSON = function (uri, post, headers)
{ if (!post) { post = {}; }
  if (!headers) { headers = {}; }
  return new Promise(
      (pass, fail) =>
          { let xhr = new XMLHttpRequest;
            xhr.open('POST', uri);
            xhr.onload = (response) =>
            { pass(JSON.parse(response.body)); };
            xhr.onerror = (failure) => { fail(failure); };
            for (header in headers)
            { if (headers.hasOwnProperty(header))
              { xhr.setRequestHeader(header, headers[header]); } }
            xhr.setRequestHeader('Accept', 'application/json;encoding=utf-8');
            xhr.setRequestHeader('Content-Type', 'application/json;encoding=utf-8');
            xhr.send(JSON.stringify(post)); }); };

Tootsville.util.assertValidHostName = function (hostName)
{ if ("users" == hostName || "toots" == hostName)
  { return Tootsville.host["users"]; }
  if ("gossip" == hostName || "meta-game" == hostName)
  { return Tootsville.host["gossip"]; }
  if ("world" == hostName)
  { return Tootsville.host["world"]; }
  if ("wiki" == hostName)
  { return "https://wiki.tootsville.org"; }
  if ("tootsbook" == hostName)
  { return "https://tootsbook.com"; }
  Tootsville.error ("Unknown which host handles " + hostName);
  return undefined;
};

Tootsville.util.rest = function (uri, post, headers)
{ var hostName = uri.split('/')[0];
  hostName = Tootsville.util.assertValidHostName(hostName);
  Tootsville.trace ("REST: URI " + hostName + '/' + uri);
  return Tootsville.util.postJSONforJSON(hostName + '/' + uri,
                                         post, headers); };

Tootsville.util.loadScript = function (src)
{ return new Promise( finish =>
                      { var el = document.createElement('SCRIPT');
                        el.onload = finish;
                        el.src = src;
                        document.body.appendChild(el); });};
