Tootsville.inform = function (event, details) {
    console.info.apply(console, arguments);
    dataLayer.push({event: event, details: arguments});
    Rollbar.info(Array.prototype.splice.call(arguments).map((x) => {x.toSource()}).join('\t'));
};
Tootsville.warn = function (message) {
    console.warn.apply(console, arguments);
    dataLayer.push({event: 'warning', details: arguments});
    Rollbar.warn.apply(Rollbar, Array.prototype.splice.call(arguments).map((x) => {x.toSource()}).join('\t'));
};
Tootsville.error = function (message) {
    console.error.apply(console, arguments);
    dataLayer.push({event: 'error', details: arguments});
    throw new Error(message);
};
