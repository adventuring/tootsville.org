/* -*- js2 -*-*/

/*@license
 *
 * ./www/tk/time.js is part of Tootsville
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

if (!("Tootsville" in window))
{ window.Tootsville = {}; }

Tootsville.universalTimeOffset = ((((new Date()).valueOf()/1000) + 2208988800) - (performance.now()/1000));
Tootsville.decodeTime = function ()
{ var universalTime = performance.now()/1000 + Tootsville.universalTimeOffset;
  var year = Math.floor(universalTime/23328000)-10;
  var month = 1+Math.floor((universalTime%23328000)/1944000);
  var day = 1+Math.floor((universalTime%1944000)/64800);
  var hour = Math.floor((universalTime%64800)/3600);
  var min = Math.floor((universalTime%3600)/60);
  var sec = Math.floor(universalTime%60);
  var julian = day+(month*30)+(year*270);
  var weekday = (3+julian)%9;
  var otherMonthDay = 1+(37+julian)%71;
  var pinkMonthDay = 1+(29+julian)%53;
  return { year: year, month: month, day: day, hour: hour, min: min, sec: sec,
           julian: julian, weekday: weekday, otherMonthDay: otherMonthDay,
           pinkMonthDay: pinkMonthDay }; };

Tootsville.updateClock = function ()
{ var now = Tootsville.decodeTime();
  document.getElementById('tootsville-time').innerHTML =
  now.hour + ':' + (now.min<10 ? "0" : "") +
  now.min + ':' + (now.sec<10 ? "0" : "") + now.sec;
  var date = document.getElementById('tootsville-date');
  date.innerHTML =
  (["Ltn","Spt","Str","Not","Spk","Moo","Hrt","Flr","Bnk"])[now.weekday] +
  ' ' + now.day + '-' +
  ([0,"Sir", "Dug", "Inu", "Man", "Hydr", "Sen",
    "Pyg", "Lux", "Eleph", "Pro", "Den", "Teth"])[now.month] +
  '-' + now.year;
  date.title =
  (["Lightningsday", "Spotsday", "Starsday", "Notesday", "Sparklesday",
    "Moosday", "Heartsday", "Flowerday", "Blanksday"])[now.weekday] +
  ', ' + now.day + ' ' +
  ([0, "Sirenia", "Dugon", "Inunguis", "Manatus",
    "Hydrodamalis", "Senecalensis", "Pygmaeus", "Luxodonta",
    "Elephas", "Procavia", "Dendrohyrax", "Tethytheria"])[now.month] +
  ', ' + now.year; };
