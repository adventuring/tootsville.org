/* -*- js2 -*- */
if (!('Tootsville' in window)) { Tootsville = {}; }
if (!('ui' in Tootsville)) { Tootsville.ui = {}; }
if (!('newToot' in Tootsville.ui)) { Tootsville.ui.newToot = {}; }

Tootsville.ui.newToot.colors =
    { base: [ "Cyan", "Indigo", "Orange", "Pink", "Red", "Turquoise", "Violet", "White", "Yellow" ],
      pad: [ "Cyan", "Indigo", "Pink", "Red", "Spring Green", "Violet", "White", "Yellow" ],
      pattern: [ "Black", "Cyan", "Indigo", "Orange", "Pink", "Rainbow", "Turquoise", "Violet", "White", "Yellow" ],
      "t-shirt": [ "Cyan", "Indigo", "Pink", "Red", "Spring Green", "Violet", "White", "Yellow" ] };

Tootsville.ui.newToot.patterns =
    [ "Flowers", "Horseshoes", "Lightning", "Patches", "Polka-Dots", "Notes", "Sparkles", "Spots", "Stars", "Swirls" ];

Tootsville.ui.newToot.changePattern = function (button)
{ var picker = document.getElementById ("new-toot-pattern-picker");
  if (picker) { picker.opacity = 0;
                setTimeout ( function () { picker.display = 'none';
                                           picker.parentElement.removeChild (picker); }, 100 ); }
  else { Tootsville.ui.newToot.createPatternPicker (name, button); } };

Tootsville.ui.newToot.changeColor = function (name, button)
{ var picker = document.getElementById ("new-toot-color-picker-" + name);
  if (picker) { picker.opacity = 0;
                setTimeout ( function () { picker.display = 'none';
                                           picker.parentElement.removeChild (picker); }, 100 ); }
  else { Tootsville.ui.newToot.createColorPicker (name, button); } };

Tootsville.ui.newToot.updateAvatar = function (swatch, color)
{ if ("base" == swatch)
  { document.getElementById('skin').style =
    'fill:' + interpretTootColor (color); }
  else if ('pad' == swatch)
  { document.getElementById('hand-pad').style =
    'fill:' + interpretTootColor (color); } };

Tootsville.ui.newToot.rainbowGradient =
    "linear-gradient(to bottom, #ff0000 0%,#ff9900 13%,#ffff00 28%,#00ff00 45%,#0033ff 63%,#3300ff 80%,#9900ff 100%)";

Tootsville.ui.newToot.pickedColor = function (event)
{ var button = event.target;
  var picker = button.parentElement;
  var targetColor = picker.getAttribute ("data-target-color");
  Tootsville.ui.newToot.updateAvatar (targetColor, button.value);
  var widget = document.getElementById ("new-toot-" + targetColor + "-color");
  if ("Rainbow" == button.value)
  { widget.style.backgroundImage = Tootsville.ui.newToot.rainbowGradient;
    widget.style.backgroundColor = ""; }
  else
  { widget.style.backgroundImage = "";
    widget.style.backgroundColor = interpretTootColor (button.value); }
  widget.setAttribute ('data-color', button.value);
  Tootsville.ui.newToot.changeColor (targetColor, picker.style.backgroundColor); };

Tootsville.ui.newToot.pickedPattern = function (event)
{ var button = event.target;
  var picker = button.parentElement;
  var widget = document.getElementById ("new-toot-pattern");
  widget.style.backgroundImage = "url(https://jumbo.tootsville.org/Assets/Avatars/5/Patterns/" + button.value + ".png)";
  widget.setAttribute ('data-pattern', button.value);
  Tootsville.ui.newToot.changePattern (button.value); };

Tootsville.ui.newToot.notReady = function (reasons)
{ alert ("You're not quite ready yet." + reasons); }; // FIXME soft dialog

Tootsville.ui.newToot.ready = function ()
{ var name = document.getElementById ("new-toot-name").value;
  var baseColor = document.getElementById ("new-toot-base-color").getAttribute ("data-color");
  var padColor = document.getElementById ("new-toot-pad-color").getAttribute ("data-color");
  var patternColor = document.getElementById ("new-toot-pattern-color").getAttribute ("data-color");
  var tShirtColor = document.getElementById ("new-toot-t-shirt-color").getAttribute ("data-color");
  var pattern = document.getElementById ("new-toot-pattern").getAttribute ("data-pattern");
  var notReady = "";
  if (! ( (/^[A-Za-z]-?([A-Za-z]+-?)*[A-Za-z]*-?[0-9]?[0-9]?$/.test (name)) &&
          (name.length >= 3) && (name.length <= 32) &&
          ! (/[a-z]\1\1/.test (name.toLowerCase ())) ))
  { notReady += ("\n\nYou must give your Toot a name.");
    if (name.length > 0)
    { notReady += "\n(The name you have entered is not valid. Check the rules.)"; } };
  if (baseColor && baseColor == padColor)
  { notReady += ("\n\nYour base color and pad color should be different."); }
  if (baseColor && baseColor == patternColor)
  { notReady += ("\n\nYour base color and pattern color should be different."); }
  if (! baseColor)
  { notReady += ("\n\nPick a base color for your Toot. (Click on the box at the top-left to see available colors.)"); }
  if (! padColor)
  { notReady += ("\n\nPick a pad color for your Toot. (Click on the box at the middle-left to see available colors.)"); }
  if (! patternColor)
  { notReady += ("\n\nPick a pattern color for your Toot. (Click on the box at the top-right to see available colors.)"); }
  if (! tShirtColor)
  { notReady += ("\n\nPick a T-shirt color for your Toot. (Click on the box at the middle-right to see available colors.)"); }
  if (! pattern)
  { notReady += ("\n\nPick a pattern for your Toot. (Click on the box at the bottom-right to see available patterns.)"); }
  if ("" != notReady)
  { return Tootsville.ui.newToot.notReady (notReady); }
  Tootsville.util.rest ('POST', 'toots',
                        { name: name,
                          baseColor: baseColor,
                          padColor: padColor,
                          pattern: pattern,
                          patternColor: patternColor,
                          tShirtColor: tShirtColor });
  return true;};

Tootsville.ui.newToot.checkName = function ()
{ var name = document.getElementById ("new-toot-name").value;
  document.getElementById("new-toot-name-problem-3-32").style.color =
  ( (name.length < 3 || name.length > 32) ? "red" : "black" );
  document.getElementById("new-toot-name-problem-begins-with-letter").style.color =
  ( name.length == 0 || (name[0].toLowerCase() < "a" || name[0].toLowerCase() > "z") ? "red" : "black" );
  document.getElementById("new-toot-name-problem-mostly-letters").style.color =
  ( /^[A-Za-z]-?([A-Za-z]+-?)*[A-Za-z]*-?[0-9]?[0-9]?$/.test (name) ? "black" : "red" );
  document.getElementById("new-toot-name-problem-offensive").style.color =
  ( /(fuck|shit|sucker|nigger|nigga|\bfag\b|faggot|bitch|cunt|cunny|retard|penis|vagina|scrotum|testes|testicle|my-?balls|my-?nut|my-?dick|(suck|lick|eat)-?my|\bcock\b|stupid|dyke|faggy)/.test (name.toLowerCase()) ? "red" : "black" );
  document.getElementById("new-toot-name-problem-repeat").style.color =
  ( /[a-z]\1\1/.test (name.toLowerCase ()) ? "red" : "black" ); };

Tootsville.ui.newToot.createPatternPicker = function (button)
{ var picker = document.createElement ("DIV");
  picker.setAttribute ("ID", "new-toot-pattern-picker");
  picker.className = 'new-toot-pattern-picker';
  for (var n = 0; n < Tootsville.ui.newToot.patterns.length; ++n)
  { var pattern = Tootsville.ui.newToot.patterns[ n ];
    var patternButton = document.createElement ("INPUT");
    patternButton.setAttribute ("TYPE", "RADIO");
    patternButton.setAttribute ("VALUE", pattern);
    patternButton.setAttribute ("NAME", "new-toot-pattern-picker");
    patternButton.setAttribute ("ID", "new-toot-pattern-picker-" + pattern);
    patternButton.onchange = Tootsville.ui.newToot.pickedPattern;
    patternButton.className = "pattern-picker-button";
    var label = document.createElement ("LABEL");
    var image = document.createElement ("IMG");
    image.src = "https://jumbo.tootsville.org/Assets/Avatars/5/Patterns/" + pattern + ".png";
    label.appendChild (image);
    label.htmlFor = "new-toot-pattern-picker-" + pattern;
    picker.appendChild (patternButton);
    picker.appendChild (label); }
  var buttonBox = document.createElement ("DIV");
  var okButton = document.createElement ("BUTTON");
  okButton.innerText = "OK";
  okButton.onclick = function ()
  { picker.opacity = 0;
    picker.display = 'none';
    picker.parentElement.removeChild (picker); };
  buttonBox.appendChild (okButton);
  picker.appendChild (buttonBox);
  document.getElementById ('hud').appendChild (picker); };

Tootsville.ui.newToot.createColorPicker = function (name, button)
{ var picker = document.createElement ("DIV");
  picker.setAttribute ("ID", "new-toot-color-picker-" + name);
  picker.className = 'new-toot-color-picker';
  picker.setAttribute ("data-target-color", name);
  for (var n = 0; n < Tootsville.ui.newToot.colors[ name ].length; ++n)
  { var color = Tootsville.ui.newToot.colors[ name ][ n ];
    var colorButton = document.createElement ("INPUT");
    colorButton.setAttribute ("TYPE", "RADIO");
    colorButton.setAttribute ("VALUE", color);
    colorButton.setAttribute ("NAME", "new-toot-color-picker-" + name);
    colorButton.setAttribute ("ID", "new-toot-color-picker-" + name + "-" + color);
    colorButton.onchange = Tootsville.ui.newToot.pickedColor;
    colorButton.className = "color-picker-button";
    var label = document.createElement ("LABEL");
    label.appendChild (document.createTextNode (color));
    label.htmlFor = "new-toot-color-picker-" + name + "-" + color;
    if ("White" == color || "Yellow" == color) {
        label.style.color = "black";
    }
    if ("Rainbow" == color) {
        label.style.backgroundImage = Tootsville.ui.newToot.rainbowGradient;
    } else {
        label.style.backgroundColor = interpretTootColor (color.toLowerCase ());
    }
    picker.appendChild (colorButton);
    picker.appendChild (label); }
  var buttonBox = document.createElement ("DIV");
  var okButton = document.createElement ("BUTTON");
  okButton.innerText = "OK";
  okButton.onclick = function ()
  { picker.opacity = 0;
    picker.display = 'none';
    picker.parentElement.removeChild (picker); };
  buttonBox.appendChild (okButton);
  picker.appendChild (buttonBox);
  document.getElementById ('hud').appendChild (picker); };
  
