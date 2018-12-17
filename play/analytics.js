Tootsville.trace = function (event, details) {
    console.log.apply(console, arguments);
};
Tootsville.inform = function (event, details) {
    console.info.apply(console, arguments);
    dataLayer.push({event: event, details: arguments});
    ga('send', arguments[0], Array.join(arguments.splice(1), "\n"));
    if (('Rollbar' in window) && Rollbar.info)
    { Rollbar.info(Array.prototype.splice.call(arguments).map((x) => {x.toSource()}).join('\t')); }
};
Tootsville.warn = function (message) {
    console.warn.apply(console, arguments);
    dataLayer.push({event: 'warning', details: arguments});
    ga('send', 'warning', Array.join(arguments, "\n"));
    if (('Rollbar' in window) && Rollbar.warn)
    { Rollbar.warn.apply(Rollbar, Array.prototype.splice.call(arguments).map((x) => {x.toSource()}).join('\t')); }
};
Tootsville.error = function (message) {
    console.error.apply(console, arguments);
    dataLayer.push({event: 'error', details: arguments});
    ga('send', 'error', Array.join(arguments, "\n"));
    throw new Error(message);
};
