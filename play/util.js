/* -*- js2 -*-*/

/*@license
 *
 * ./play/util.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018,2019   The
 * Corporation for Inter-World Tourism and Adventuring (ciwta.org).
 *
 * This program is Free Software:  you can redistribute it and/or modify
 * it  under the  terms  of the  GNU Affero  General  Public License  as
 * published by  the Free Software  Foundation; either version 3  of the
 * License, or (at your option) any later version.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.  See the  GNU
 * Affero General Public License for more details.
 *
 * You should  have received  a copy  of the  GNU Affero  General Public
 * License     along    with     this    program.     If    not,     see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 *
 */
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
  if (!(hostName == "http" || hostName == 'https'))
  { hostName = Tootsville.util.assertValidHostName(hostName);
    uri = hostName + '/' + uri; }
  Tootsville.trace ('REST: ' + method + ' ' + uri);
  if (!headers) { headers = {}; }
  if (! ('Accept' in headers))
  { headers['Accept'] = 'application/json;encoding=utf-8'; }
  if (Tootsville.login.firebaseAuth)
  { headers['X-Infinity-Auth'] = 'auth/Infinity/Alef/5.0 firebase ' + Tootsville.login.firebaseAuth; }
  let opts = { method: method };
  if (body && (! ('Content-Type' in headers)))
  { headers['Content-Type'] = 'application/json';
    opts.body = JSON.stringify(body); }
  opts.headers = headers;
  return fetch (uri, opts).then(
      response =>
          { if (response.ok)
            { return response.json (); }
            else
            { var json = response.json ();
              Tootsville.warn("Server error from " + uri + " " + JSON.stringify(json));
              return Tootsville.parrot.ask (
                  "Uh-oh! Server trouble!",
                  Tootsville.parrot.parrotErrorText(json),
                  [{ tag: 'retry', text: "Retry the network operation" }]).then
              (() =>
               { return Tootsville.util.rest (method, uri, body, headers); }); }},
      error =>
          { Tootsville.warn("Fetch error " + error);
            Tootsville.parrot.ask (
                "Uh-oh! Network trouble!",
                "<P>I got a network error: <TT>" + error + "</TT> <SMALL>from <TT>" +
                    uri.replace('/', '/&shy;') +
                    "</TT></SMALL></P><P>Did we get disconnected?</P>",
                [{ tag: 'retry', text: "Retry the network operation" }]).then
            (() =>
             { return Tootsville.util.rest (method, uri, body, headers); });} ); };

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
        "I'm not able to reach any of the Tootsville game servers. This probably means you won't be able to sign in." ); } ); };
