(function() {
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());

    gtag('config', 'UA-80917352-1');
    gtag('set', {'appVersion': '0.0.5',
                });
    if (Tootsville.character && Tootsville.character.id) {
        gtag('set', { 'user_id': Tootsville.character.id });
    }
    if (Tootsville.character && Tootsville.character.name) {
        gtag('set', { 'charName': Tootsville.character.name });
    }
    console.log("Inititalized Google Tag Manager");
})();
