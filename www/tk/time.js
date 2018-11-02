window['Tootsville'] ? 0 : window.Tootsville = {};
Tootsville.universalTimeOffset = ((((new Date()).valueOf()/1000) + 2208988800) - (performance.now()/1000));
Tootsville.decodeTime = function ()
{
    var universalTime = performance.now()/1000 + Tootsville.universalTimeOffset;
    var year = Math.floor(universalTime/23328000)-10;
    var month = 1+Math.floor((universalTime%23328000)/1944000);
    var day = 1+Math.floor((universalTime%1944000)/64800);
    var hour = Math.floor((universalTime%64800)/3600);
    var min = Math.floor((universalTime%3600)/60);
    var sec = Math.floor(universalTime%60);
    var julian = day+(month*30)+(year*270);
    var weekday = (3*julian)%9;
    var otherMonthDay = 1+(37+julian)%71;
    var pinkMonthDay = 1+(29+julian)%53;
    return { year: year, month: month, day: day, hour: hour, min: min, sec: sec,
             julian: julian, weekday: weekday, otherMonthDay: otherMonthDay,
             pinkMonthDay: pinkMonthDay };
}
Tootsville.updateClock = function () {
    var now = Tootsville.decodeTime();
    document.getElementById('tootsville-time').innerHTML = now.hour + ':' + (now.min<10 ? "0" : "") +
        now.min + ':' + (now.sec<10 ? "0" : "") + now.sec;
    document.getElementById('tootsville-date').innerHTML =
        (["Ltn","Spt","Str","Not","Spk","Moo","Hrt","Flr","Bnk"])[now.weekday] +
        ' ' + now.day + '-' +
        ([0,"Sir", "Dug", "Inu", "Man", "Hydr", "Sen", "Pyg", "Lux", "Eleph", "Pro", "Den", "Teth"])[now.month] +
        '-' + now.year;
}

