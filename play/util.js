/* -*- js2 -*- */
if (!("util" in Tootsville)) { Tootsville.util = {}; }

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

Tootsville.util.rest = function (method, uri, body, headers)
{ let hostName = uri.split('/')[0];
  hostName = Tootsville.util.assertValidHostName(hostName);
  uri = hostName + '/' + uri;
  Tootsville.trace ('REST: ' + method + ' ' + uri);
  if (!headers) { headers = {}; }
  if (! ('Accept' in headers))
  { headers['Accept'] = 'application/json;encoding=utf-8'; }
  if (Tootsville.login.firebaseAuth)
  { headers['X-Infinity-Auth'] = 'auth/Infinity/Alef/5.0 firebase ' + Tootsville.login.firebaseAuth; }
  let opts = { method: method };
  if (body && (! ('Content-Type' in headers)))
  { headers['Content-Type'] = 'application/json';
    opts.body = body; }
  opts.headers = headers;
  return fetch (uri, opts).then( response => response.json () ); };

Tootsville.util.loadScript = function (src)
{ return new Promise( finish =>
                      { var el = document.createElement('SCRIPT');
                        el.onload = finish;
                        el.src = src;
                        document.body.appendChild(el); });};

//

Tootsville.util.ensureServersReachable = function ()
{ Tootsville.util.rest ('GET', 'meta-game/ping').then
  ( (response) => { Tootsville.trace ("Ping replied", response); },
    (error) => { Tootsville.parrot.say (
        "Squawk! I don't see any servers!",
        "I'm not able to reach any of the Tootsville game servers." ); } ); };
