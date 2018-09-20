(function() {
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());

    gtag('config', 'UA-80917352-1');
    gtag('set', {'appVersion': '0.0.5',
                });
    if (Tootsville.charID) {
        gtag('set', { 'user_id': Tootsville.charID });
    }
    if (Tootsville.charName != '？') {
        gtag('set', { 'charName': Tootsville.charName });
    }
    console.log("Inititalized Google Tag Manager");
})();
