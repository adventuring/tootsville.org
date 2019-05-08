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
    [ "Flowers", "Horseshoes", "Lightning", "Patches", "Polka Dots", "Notes", "Sparkles", "Spots", "Stars", "Swirls" ];

Tootsville.ui.newToot.changeColor = function (name, button)
{ var picker = document.getElementById ("new-toot-color-picker-" + name);
  if (picker) { picker.opacity = 0;
                setTimeout ( function () { picker.display = 'none';
                                           picker.parent.removeChild (picker); }, 100 ); }
  else { Tootsville.ui.newToot.createColorPicker (name, button); } };

Tootsville.ui.newToot.pickedColor = function (event)
{ var button = event.target;
  var picker = button.parent;
  var targetColor = picker.getAttribute ("data-target-color");
  var widget = document.getElementById ("new-toot-" + targetColor + "-color");
  widget.style.backgroundColor = button.value;
  Tootsville.ui.newToot.changeColor (targetColor, picker.style.backgroundColor); };

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
          (name.length >= 3) && (name.length <= 32)))
  { notReady += ("\n\nYou must give your Toot a name."); };
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
  Tootsville.util.rest (FIXME);
};

Tootsville.ui.newToot.checkName = function ()
{ var name = document.getElementById ("new-toot-name").value;
  document.getElementById("new-toot-name-problem-3-32").style.color =
  ( (name.length < 3 || name.length > 32) ? "red" : "black" );
  document.getElementById("new-toot-name-problem-begins-with-letter").style.color =
  ( name.length == 0 || (name[0].toLowerCase() < "a" || name[0].toLowerCase() > "z") ? "red" : "black" );
  document.getElementById("new-toot-name-problem-mostly-letters").style.color =
  ( /^[A-Za-z]-?([A-Za-z]+-?)*[A-Za-z]*-?[0-9]?[0-9]?$/.test (name) ? "black" : "red" );
  document.getElementById("new-toot-name-problem-offensive").style.color =
  ( /(fuck|shit|sucker|nigger|nigga|\bfag\b|faggot|bitch|cunt|cunny|penis|vagina|scrotum|testes|testicle|my-?nut|my-?dick|(suck|lick|eat)-?my|\bcock\b|stupid|dyke|faggy)/.test (name.toLowerCase()) ? "red" : "black" );
  /* TODO test for triple letters */ };

Tootsville.ui.newToot.createColorPicker = function (name, button)
{ var picker = document.createElement ("DIV");
  picker.setAttribute ("ID", "new-toot-color-picker-" + name);
  picker.style.position = 'absolute';
  picker.style.right = 0;
  picker.style.top = 0;
  picker.style.zIndex = 5000;
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
        label.style.background = "linear-gradient(to bottom, #ff0000 0%,#ff9900 13%,#ffff00 28%,#00ff00 45%,#0033ff 63%,#3300ff 80%,#9900ff 100%);";
    } else {
        label.style.backgroundColor = interpretTootColor (color.toLowerCase ());
    }
    picker.appendChild (colorButton);
    picker.appendChild (label); }
  var buttonBox = document.createElement ("DIV");
  var okButton = document.createElement ("BUTTON");
  okButton.innerText = "OK";
  okButton.onClick = function ()
  { picker.opacity = 0;
    setTimeout ( function () { picker.display = 'none';
                               picker.parent.removeChild (picker); }, 100 ); };
  buttonBox.appendChild (okButton);
  picker.appendChild (buttonBox);
  document.getElementById ('hud').appendChild (picker); };
  
