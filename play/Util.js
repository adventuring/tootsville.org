/* -*- js2 -*-*/

/**@license
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
if (!("util" in Tootsville)) { Tootsville.Util = {}; }

/**
 * Ensure that @code{hostName} is a  valid hostname for the game cluster
 * we're in.
 */
Tootsville.Util.assertValidHostName = function (hostName)
{ if ("http" == hostName || "https" == hostName)
  { Tootsville.error ("Landed here with http/s as a hostName"); }
  if ("wiki" == hostName)
  { return "https://wiki.tootsville.org"; }
  if ("tootsbook" == hostName)
  { return "https://tootsbook.com"; }
  return Tootsville.host["game"]; };

/**
 * The main REST client.
 *
 * @table @code
 * @item method
 * GET, PUT, or POST
 * @item uri
 * The URI to access under the game host.
 * @item body
 * A JSON body for a PUT or POST
 * @item headers
 * An object which maps to additional headers to be set on the request.
 * X-Infinity-Auth will be set when logged in; Accept and Content-Type will be
 * defaulted to application/json if not set.
 * @end table
 */
Tootsville.Util.rest = function (method, uri, body, headers)
{ let hostName = uri.split('/')[0];
  let origURI = uri;
  if (hostName == "http" || hostName == 'https')
  { /* do not alter */ }
  else
  { hostName = Tootsville.Util.assertValidHostName (hostName);
    uri = hostName + '/' + uri; }
  if (!headers) { headers = {}; }
  if (! ('Accept' in headers))
  { headers['Accept'] = 'application/json;encoding=utf-8'; }
  if (Tootsville.Login.firebaseAuth)
  { headers['X-Infinity-Auth'] = 'auth/Infinity/Alef/5.0 firebase ' + Tootsville.Login.firebaseAuth; }
  let opts = { method: method };
  if (body && (! ('Content-Type' in headers)))
  { headers['Content-Type'] = 'application/json;charset=utf-8';
    opts.body = JSON.stringify(body); }
  opts.headers = headers;
  console.debug ('REST: ' + method + ' ' + uri, opts);
  return fetch (uri, opts).then(
      response =>
          { if (response.ok)
            { if (200 == response.status)
              { return response.json (); }
              else if (204 == response.status)
              { return null; } }
            else
            { Tootsville.warn("Server error from " + uri, response);
              if (response.status >= 400 && response.status <= 499)
              { return { error: response.status, response: response }; }
              response.json ().then (
                  json =>
                      { return Tootsville.Parrot.ask (
                          "Uh-oh! Server trouble!",
                          Tootsville.Parrot.parrotErrorText(json),
                          [{ tag: 'retry', text: "Retry the network operation" }]).then
                        (() =>
                         { console.log ("User-initiated retry for " + origURI);
                           return Tootsville.Util.rest (method, origURI, body, headers); }); } ); }
            return null; },
      error =>
          { Tootsville.warn("Fetch error ", error);
            Tootsville.Parrot.ask (
                "Uh-oh! Network trouble!",
                "<P>I got a network error: <TT>" + error + "</TT> <SMALL>from <TT>" +
                    uri.replace('/', '/&shy;') +
                    "</TT></SMALL></P><P>Did we get disconnected?</P><BR>" +
                    "<SMALL><A HREF=\"https://wiki.Tootsville.org/wiki/Network_Troubleshooting\">" +
                    "Network Troubleshooting</A></SMALL>",
                [{ tag: 'retry', text: "Retry the network operation" }]).then
            (() =>
             { console.log ("User-initiated retry after error for " + origURI);
               return Tootsville.Util.rest (method, origURI, body, headers); });} ); };

Tootsville.Util.loadScript = function (src)
{ return new Promise( finish =>
                      { let el = document.createElement('SCRIPT');
                        el.onload = finish;
                        el.src = src;
                        document.body.appendChild(el); });};

//

/**
 * Check for the game REST server. 
 * 
 * Calls @url{https://game.tootsville.org/meta-game/ping}  and complains
 * to the player if it can't be reached.
 */
Tootsville.Util.ensureServersReachable = function ()
{ Tootsville.Util.rest ('GET', 'meta-game/ping').then
  ( (response) => { Tootsville.trace ("Ping replied", response); },
    (error) => { Tootsville.Parrot.say (
        "Squawk! I don't see any servers!",
        "I'm not able to reach any of the Tootsville.Game servers. "+
            "This probably means you won't be able to sign in." ); } ); };

/**
 * Check for value equality of two objects
 */
Tootsville.Util.equalP = function (a, b)
{ if (!((a instanceof Object) && (b instanceof Object)))
  { return false; }
  const aProps = Object.getOwnPropertyNames(a);
  const bProps = Object.getOwnPropertyNames(b);
  if (aProps.length != bProps.length)
  { return false; }
  for (var i = 0; i < aProps.length; ++i)
  { var propName = aProps[i];
    if (a[propName] !== b[propName])
    { return false; } }
  return true; };

