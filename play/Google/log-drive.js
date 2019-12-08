/* -*- js2 -*-*/

/**@license
 *
 * ./play/google/log-drive.js is part of Tootsville
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

(function ()
 { var previousLogSegmentStart = null;
   var currentLogSegmentStart = null;
   var previousLogSegment = null;
   var currentLogSegment = null;

   var previousLogSyncP = true;
   var lastSyncStart = 0;
   var currentLogDirtyP = false;

   var TheMaxLogSegmentLength = 4 * 1000 * 1000; /* 4MB */
   var TheMaxTimeBetweenSync = 3 * 60 * 1000; /* 3 min */

   function startLogging () {
       /* TODO */
   }

   function stopLogging () {
       /* TODO */
   }

   function appendToLog (text) {
       currentLogSegment += text;
       rotateLogIfNeeded();
   }

   function startNewLog () {
       if (currentLogSegmentStart &&
           (previousLogSegmentStart != currentLogSegmentStat)) {
           Tootsville.warn("startNewLog called with dirty currentLogSegment");
           rotateLogSegment();
           return;
       }
       currentLogSegmentStart = precision.now();
       currentLogSegment = '';
       currentLogDirtyP = false;
   }

   function rotateLogIfNeeded () {
       if (currentLogSegment.length > TheMaxLogSegmentLength) {
           rotateLogSegment();
       } else if (currentLogDirtyP && lastSyncStart < (precision.now() - TheMaxTimeBetweenSync)) {
           flushLogBuffers();
       }
   }

   function rotateLogSegment () {
       if (! previousLogSyncP) {
           flushLogBuffers();
           return false;
       }
       previousLogSegmentStart = curentLogSegmentStart;
       previousLogSegment = currentLogSegment;
       startNewLog();
       return true;
   }

   function logNameForStartTime (startTime) {
       return "Tootsville log for " +
           (Tootsville.character && Tootsville.character.name || 'No character') +
           "(" + (startTime.toISOString()) + ")";
   }

   function writeSegment (text, startTime) {

   }

   function flushLogBuffers () {
       writeSegment(currentLogSegment, currentLogSegmentStart);
       if (previousLogSegment && (! previousLogSyncP)) {
           writeSegment(previousLogSegment, previousLogSegmentStart).then( (success) => {
               previousLogSyncP = true;
           });
       }
       rotateLogIfNeeded();
   }

   Tootsville.GoogleDrive = {
       loggingP: false,
       loggingURI: null,
       startLogging: startLogging,
       appendToLog: appendToLog,
       stopLogging: stopLogging,
       flushLogBuffers: flushLogBuffers
   };

 })();
