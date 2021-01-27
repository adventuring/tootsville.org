if (!('traced' in window)) window.traced = {};

(function ()
 { const _setFnByName = function (name, fn)
   { let nameParts = name.split ('.');
     let lastPart, ref;
     if (nameParts)
     { lastPart = nameParts.pop ();
       for (ref = window; 0 < nameParts.length; ref = ref[nameParts.shift ()]); }
     else { lastPart = name;
            ref = window; }
     ref[ lastPart ] = fn; };

   const _getFnByName = function (name)
   { let nameParts = name.split ('.');
     let lastPart, ref;
     if (nameParts)
     { lastPart = nameParts.pop ();
       for (ref = window; 0 < nameParts.length; ref = ref[nameParts.shift ()]); }
     else { lastPart = name;
            ref = window; }
     return ref[ lastPart ]; };

   window.trace = function (fname)
   { if (window.traced[fname]) return;
     const fn = _getFnByName (fname);
     console.debug ("Tracing function " + fname, fn);
     window.traced[fname] = fn;
     _setFnByName (fname, function (...args)
                   { console.trace.apply (this, [fn].concat (args));
                     const ret = fn.apply (this, args);
                     console.debug (fname + ' => ', ret);
                     return ret; }); };

   window.untrace = function (fname)
   { if (! window.traced[fname]) return;
     console.debug ("Untracing function " + fname);
     _setFnByName (fname, window.traced[fname]);
     delete window.traced[fname]; };

})();
