/* -*- js2 -*-*/

/**@license
 *
 * ./play/Util.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2021   The
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
if (!("Util" in Tootsville)) { Tootsville.Util = {}; }

/**
 * Ensure that @code{hostName} is a  valid hostname for the game cluster
 * we're in.
 */
Tootsville.Util.assertValidHostName = function (hostName)
{ if ("http" === hostName || "https" === hostName)
  { Tootsville.error ("Landed here with http/s as a hostName"); }
  if ("wiki" === hostName)
  { return "https://wiki.tootsville.org"; }
  if ("tootsbook" === hostName)
  { return "https://tootsbook.com"; }
  return Tootsville.host.game; };

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
Tootsville.Util.rest = function (method, uri, body=undefined, headers=undefined)
{ let hostName = uri.split('/')[0];
  let origURI = uri;
  if ('http' === hostName || 'https' === hostName)
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
            { if (200 === response.status)
              { return response.json (); }
              else if (204 === response.status)
              { return null; } }
            else
            { Tootsville.warn("Server error from " + uri, response);
              if (response.status >= 400 && response.status <= 499)
              { return { error: response.status, response: response }; }
              response.json ().then (
                  json =>
                      { return Tootsville.Gossip.Parrot.ask (
                          "Uh-oh! Server trouble!",
                          Tootsville.Gossip.Parrot.parrotErrorText(json),
                          [{ tag: 'retry', text: "Retry the network operation" }]).then
                        (() =>
                         { console.log ("User-initiated retry for " + origURI);
                           return Tootsville.Util.rest (method, origURI, body, headers); }); } ); }
            return null; },
      error =>
          { Tootsville.warn("Fetch error ", error);
            Tootsville.Gossip.Parrot.ask (
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

/**
 * Load the Javascript referenced by SRC into the page
 */
Tootsville.Util.loadScript = function (src)
{ return new Promise( finish =>
    { let el = document.createElement('SCRIPT');
      el.addEventListener ('load', finish);
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
{ fetch (Tootsville.host.game + '/meta-game/ping').then
  ( (response) => { if (response.ok)
      Tootsville.trace ("Ping replied", response);
                    else
                        Tootsville.Gossip.Parrot.say (
        "Squawk! I don't see any servers!",
        `<p>I'm not able to reach any of the Tootsville game servers. </p>
<p>This probably means you won't be able to play right now. </p>
<p> <a href='https://wiki.tootsville.org/Network_Troubleshooting'>Network Troubleshooting</a></p>` );},
    (error) => { Tootsville.Gossip.Parrot.say (
        "Squawk! I don't see any servers!",
        `<p>A network error prevents access to the Tootsville game servers. </p>
<p>This probably means you won't be able to play right now. </p>
<p> <a href='https://wiki.tootsville.org/Network_Troubleshooting'>Network Troubleshooting</a></p>` ); });};

/**
 * Check for value equality of two objects
 */
Tootsville.Util.equalP = function (x, y)
{ 'use strict';

  if (x === null || x === undefined || y === null || y === undefined) { return x === y; }
  // after this just checking type of one would be enough
  if (x.constructor !== y.constructor) { return false; }
  // if they are functions, they should exactly refer to same one (because of closures)
  if (x instanceof Function) { return x === y; }
  // if they are regexps, they should exactly refer to same one (it is hard to better equality check on current ES)
  if (x instanceof RegExp) { return x === y; }
  if (x === y || x.valueOf() === y.valueOf()) { return true; }
  if (Array.isArray(x) && x.length !== y.length) { return false; }

  // if they are dates, they must had equal valueOf
  if (x instanceof Date) { return false; }

  // if they are strictly equal, they both need to be object at least
  if (!(x instanceof Object)) { return false; }
  if (!(y instanceof Object)) { return false; }

  // recursive object equality check
  let p = Object.keys(x);
  return Object.keys(y).every(function (i) { return p.indexOf(i) !== -1; }) &&
  p.every(function (i) { return Tootsville.Util.equalP (x[i], y[i]); }); };
