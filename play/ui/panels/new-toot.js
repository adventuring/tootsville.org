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
{ var picker = event.target;
  var targetColor = picker.getAttribute ("data-target-color");
  var widget = document.getElementById ("new-toot-" + targetColor + "-color");
  widget.style.backgroundColor = picker.style.backgroundColor;
  Tootsville.ui.newToot.changeColor (targetColor, picker.style.backgroundColor); };
  

Tootsville.ui.newToot.createColorPicker = function (name, button)
{ var picker = document.createElement ("DIV");
  picker.setAttribute ("ID", "new-toot-color-picker-" + name);
  picker.style.position = 'absolute';
  picker.style.right = 0;
  picker.style.top = 0;
  picker.className = 'new-toot-color-picker';
  picker.setAttribute ("data-target-color", name);
  for (var n = 0; n < Tootsville.ui.newToot.colors[ name ].length; ++n)
  { var color = Tootsville.ui.newToot.colors[ name ][ n ];
    var colorButton = document.createElement ("INPUT");
    colorButton.setAttribute ("TYPE", "RADIO");
    colorButton.setAttribute ("VALUE", color);
    colorButton.setAttribute ("NAME", "new-toot-color-picker-" + name);
    colorButton.onchange = Tootsville.ui.newToot.pickedColor;
    colorButton.className = "color-picker-button";
    colorButton.style.backgroundColor = interpretTootColor (color.toLowerCase ());
    var label = document.createElement ("LABEL");
    label.appendChild (colorButton);
    label.appendChild (document.createTextNode (color));
    picker.appendChild (label); }};
  
