/* -*- js2 -*- */
if (!('Tootsville' in window)) { Tootsville = {}; }
if (!('ui' in Tootsville)) { Tootsville.UI = {}; }
if (!('newToot' in Tootsville.UI)) { Tootsville.UI.newToot = {}; }

Tootsville.UI.newToot.colors =
    { base: [ "Cyan", "Indigo", "Orange", "Pink", "Red", "Turquoise", "Violet", "White", "Yellow" ],
      pad: [ "Cyan", "Indigo", "Pink", "Red", "Spring Green", "Violet", "White", "Yellow" ],
      pattern: [ "Black", "Cyan", "Indigo", "Orange", "Pink", "Rainbow", "Turquoise", "Violet", "White", "Yellow" ],
      "t-shirt": [ "Cyan", "Indigo", "Pink", "Red", "Spring Green", "Violet", "White", "Yellow" ] };

Tootsville.UI.newToot.patterns =
    [ "Flowers", "Horseshoes", "Hearts", "Lightning", "Patches", "Polka-Dots", "Notes", "Sparkles", "Spots", "Stars", "Swirls" ];

Tootsville.UI.newToot.changePattern = function (button)
{ var picker = document.getElementById ("new-toot-pattern-picker");
  if (picker) { picker.opacity = 0;
                setTimeout ( function () { picker.display = 'none';
                                           picker.parentElement.removeChild (picker); }, 100 ); }
  else { Tootsville.UI.newToot.createPatternPicker (name, button); } };

Tootsville.UI.newToot.changeColor = function (name, button)
{ var picker = document.getElementById ("new-toot-color-picker-" + name);
  if (picker) { picker.opacity = 0;
                setTimeout ( function () { picker.display = 'none';
                                           picker.parentElement.removeChild (picker); }, 100 ); }
  else { Tootsville.UI.newToot.createColorPicker (name, button); } };

Tootsville.UI.newToot.updateAvatar = function (swatch, color)
{ if ("base" == swatch)
  { document.getElementById('skin').style =
   "fill:" +
   interpretTootColor (color) +
    ";fill-opacity:1;stroke:#000000;stroke-width:2.15044212;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;paint-order:stroke markers fill";
    Tootsville.UI.newToot.applyPatternColor (); }
  else if ('pad' == swatch)
  { document.getElementById('hand-pad').style =
   "fill:" +
   interpretTootColor (color) +
    ";fill-opacity:1;stroke:#000000;stroke-width:2.15044212;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;paint-order:stroke markers fill"; }
  else if ('t-shirt' == swatch)
  { document.getElementById('t-shirt').style =
    "fill:" +
    interpretTootColor (color) +
    ";fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:1;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1"; }
  else if ('pattern' == swatch)
  { Tootsville.UI.newToot.applyPatternColor (); } };

Tootsville.UI.newToot.randomize = function ()
{ Tootsville.UI.newToot.setColor ('base',
                                  Tootsville.UI.newToot.colors.base[ Math.floor (Math.random () * Tootsville.UI.newToot.colors.base.length) ]);
  Tootsville.UI.newToot.setColor ('pad', Tootsville.UI.newToot.randomPadColor ());
  Tootsville.UI.newToot.setColor ('pattern', Tootsville.UI.newToot.randomPatternColor ());
  Tootsville.UI.newToot.setColor ('t-shirt',
                                  Tootsville.UI.newToot.colors["t-shirt"][ Math.floor (Math.random () * Tootsville.UI.newToot.colors["t-shirt"].length) ]);
  Tootsville.UI.newToot.setPattern (Tootsville.UI.newToot.patterns[ Math.floor (Math.random () * Tootsville.UI.newToot.patterns.length) ]);};

Tootsville.UI.newToot.rainbowGradient =
    "linear-gradient(to bottom, #ff0000 0%,#ff9900 13%,#ffff00 28%,#00ff00 45%,#0033ff 63%,#3300ff 80%,#9900ff 100%)";

Tootsville.UI.newToot.pickedColor = function (event)
{ var button = event.target;
  var picker = button.parentElement;
  var targetColor = picker.getAttribute ("data-target-color");
  Tootsville.UI.newToot.setColor (targetColor, button.value);
  picker.style.display = 'none'; };

Tootsville.UI.newToot.setColor = function (targetColor, value)
{ var widget = document.getElementById ("new-toot-" + targetColor + "-color");
  if ("Rainbow" == value)
  { widget.style.backgroundImage = Tootsville.UI.newToot.rainbowGradient;
    widget.style.backgroundColor = ""; }
  else
  { widget.style.backgroundImage = "";
    widget.style.backgroundColor = interpretTootColor (value); }
  widget.setAttribute ('data-color', value);
  Tootsville.UI.newToot.updateAvatar (targetColor, value);
  Tootsville.UI.newToot.changeColor (targetColor, value); };

Tootsville.UI.newToot.randomPatternColor = function ()
{ var patternColors = Tootsville.UI.newToot.colors.pattern;
  var color = 'Rainbow';
  color = patternColors[ Math.floor (Math.random () * patternColors.length) ];
  if ( ('Rainbow' == color) ||
       (color == document.getElementById ("new-toot-base-color").getAttribute ("data-color")) )
  { return Tootsville.UI.newToot.randomPatternColor (); };
  return interpretTootColor (color);};

Tootsville.UI.newToot.randomPadColor = function ()
{ var padColors = Tootsville.UI.newToot.colors.pad;
  var color = padColors[ Math.floor (Math.random () * padColors.length) ];
  if (color == document.getElementById ("new-toot-base-color").getAttribute ("data-color"))
  { return Tootsville.UI.newToot.randomPadColor (); };
  return interpretTootColor (color);};

Tootsville.UI.newToot.applyPatternColor = function ()
{ var patternWidget = document.getElementById ("new-toot-pattern");
  var patternColor = document.getElementById ("new-toot-pattern-color").getAttribute ("data-color");
  patternWidget.style.backgroundColor = interpretTootColor (document.getElementById ("new-toot-base-color").getAttribute ("data-color"));
  if (0 == patternWidget.children.length) { return; }
  for (var i = 0; i < 3; ++i)
  { patternWidget.children[0].children[3].children[i].style = "fill:" +
    (  ('Rainbow' == patternColor) ? Tootsville.UI.newToot.randomPatternColor () : interpretTootColor (patternColor)  )+
    ";fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"; }};

Tootsville.UI.newToot.pickedPattern = function (event)
{ var button = event.target;
  Tootsville.UI.newToot.setPattern (button.value); };

Tootsville.UI.newToot.setPattern = function (value)
{ var widget = document.getElementById ("new-toot-pattern");
  fetch("https://jumbo.tootsville.org/Assets/Avatars/5/Patterns/" + value + ".svg").then (
      (response) =>
          { response.text ().then (
              (text) =>
                  { widget.innerHTML = text;
                    widget.children[0].setAttribute ('width', widget.clientWidth);
                    widget.children[0].setAttribute ('height', widget.clientHeight);
                    Tootsville.UI.newToot.applyPatternColor ();
                  } ); } );
  widget.setAttribute ('data-pattern', value);
  Tootsville.UI.newToot.changePattern (value); };

Tootsville.UI.newToot.notReady = function (reasons)
{ alert ("You're not quite ready yet." + reasons); }; // FIXME soft dialog

Tootsville.UI.newToot.ready = function ()
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
  { return Tootsville.UI.newToot.notReady (notReady); }
  Tootsville.util.rest ('POST', 'toots',
                        { name: name,
                          baseColor: baseColor,
                          padColor: padColor,
                          pattern: pattern,
                          patternColor: patternColor,
                          tShirtColor: tShirtColor }).then (Tootsville.UI.newToot.afterCreate);
  return true;};

Tootsville.UI.newToot.afterCreate = function (reply)
{ if (reply.error)
  { switch (reply.error)
    { case 400:
      Tootsville.parrot.say ("Program trouble!",
                             "Something in the game program running on your computer did not work correctly, and the server could not understand our request.");
      break;;
      case 409:
      Tootsville.parrot.say ("Name already taken",
                             "There is already another Toot with that name.");
      break;;
      case 422:
      Tootsville.parrot.say ("Pattern or color conflict",
                             "The pattern or color combination you chose is not available. Perhaps you can change colors or patterns?");
      break;;
    }; } };

Tootsville.UI.newToot.checkName = function ()
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

Tootsville.UI.newToot.createPatternPicker = function (button)
{ var picker = document.createElement ("DIV");
  picker.setAttribute ("ID", "new-toot-pattern-picker");
  picker.className = 'new-toot-pattern-picker';
  for (var n = 0; n < Tootsville.UI.newToot.patterns.length; ++n)
  { var pattern = Tootsville.UI.newToot.patterns[ n ];
    var patternButton = document.createElement ("INPUT");
    patternButton.setAttribute ("TYPE", "RADIO");
    patternButton.setAttribute ("VALUE", pattern);
    patternButton.setAttribute ("NAME", "new-toot-pattern-picker");
    patternButton.setAttribute ("ID", "new-toot-pattern-picker-" + pattern);
    patternButton.onchange = Tootsville.UI.newToot.pickedPattern;
    patternButton.className = "pattern-picker-button";
    var label = document.createElement ("LABEL");
    var image = document.createElement ("IMG");
    image.src = "https://jumbo.tootsville.org/Assets/Avatars/5/Patterns/" + pattern + ".svg";
    image.style.height = '1in';
    image.style.width = '1in';
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

Tootsville.UI.newToot.createColorPicker = function (name, button)
{ var picker = document.createElement ("DIV");
  picker.setAttribute ("ID", "new-toot-color-picker-" + name);
  picker.className = 'new-toot-color-picker';
  picker.setAttribute ("data-target-color", name);
  for (var n = 0; n < Tootsville.UI.newToot.colors[ name ].length; ++n)
  { var color = Tootsville.UI.newToot.colors[ name ][ n ];
    var colorButton = document.createElement ("INPUT");
    colorButton.setAttribute ("TYPE", "RADIO");
    colorButton.setAttribute ("VALUE", color);
    colorButton.setAttribute ("NAME", "new-toot-color-picker-" + name);
    colorButton.setAttribute ("ID", "new-toot-color-picker-" + name + "-" + color);
    colorButton.onchange = Tootsville.UI.newToot.pickedColor;
    colorButton.className = "color-picker-button";
    var label = document.createElement ("LABEL");
    label.appendChild (document.createTextNode (color));
    label.htmlFor = "new-toot-color-picker-" + name + "-" + color;
    if ("White" == color || "Yellow" == color) {
        label.style.color = "black";
    }
    if ("Rainbow" == color) {
        label.style.backgroundImage = Tootsville.UI.newToot.rainbowGradient;
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
  
