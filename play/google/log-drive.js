(function () {
    var previousLogSegmentStart = null;
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
