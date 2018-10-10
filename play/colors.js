(function () {
    var tootColorMapping = {
        silver: "#ddd",
        charcoal: "#333",
        white: "#fff",
        black: "#000",

        "deep-purple": "#b117ff",
        yellow: "#ffff00", /* FIXME: incorrect shade */
        pink: "#e73e97",
        turquoise: "#00a290",
        periwinkle: "#96b4de",

        violet: "#9669ad",
        gold: "#f7d023",
        burgundy: "#9c0059",
        green: "#7ac142",
        blue: "#0082c8",

        lavender: "#ba9dca",
        tan: "#ffd2a0",
        red: "#e51b24",
        "spring-green": "#c4d82d",
        indigo: "#0028ff",

        orange: "#ff7b26"
    };

    window.interpretTootColor = function (name) {
        return tootColorMapping[name];
    }
})();
