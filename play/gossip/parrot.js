if (!("parrot" in Tootsville))
{ Tootsville.parrot = {}; }

Tootsville.parrot.show = function (reallyp)
{ var parrot = document.getElementById('parrot');
  parrot.style.opacity = (reallyp ? 100 : 0);
  document.getElementById('parrot-image').style.left = (reallyp ? 0 : '-500px');
  if (reallyp)
  { parrot.style.display = 'block';
    parrot.style.zIndex = 2000;
    if (Tootsville.parrot.squawk)
    {Tootsville.parrot.squawk.play (); } }
  else
  { setTimeout(() => { parrot.style.display = 'none'; }, 1000 ); }}

Tootsville.parrot.done = function ()
{ return new Promise
  ( (finish) => { Tootsville.parrot.show (false); finish (); } ); }

Tootsville.parrot.say = function (title, message)
{ return new Promise
  ( (finish) => 
    { var reply = document.getElementById('parrot-reply');
      reply.style.display = 'none';
      reply.innerHTML = '';
      var buttonBox = document.createElement('DIV');
      buttonBox.setAttribute ('class', 'button-box');
      var okButton = document.createElement('BUTTON');
      okButton.innerText = ' O.K. ';
      okButton.onclick = () => { Tootsville.parrot.done().then(() => {finish ()}); };
      buttonBox.append (okButton);
      reply.append (buttonBox);
      reply.style.display = 'block';
      var speech = document.getElementById('parrot-speech');
      speech.style.display = 'none';
      speech.innerHTML = '<h2>' + title + '</h2>' + message;
      speech.style.display = 'block';
      Tootsville.parrot.show(true); })}

Tootsville.parrot.ask = function (title, message, replies)
{ return new Promise
  ( (finish) => 
    { var reply = document.getElementById ('parrot-reply');
      reply.style.display = 'none';
      reply.innerHTML = '';
      var buttonBox = document.createElement('DIV');
      buttonBox.setAttribute ('class', 'button-box');
      replies.forEach( (reply) =>
      { var button = document.createElement ('BUTTON');
        button.innerText = reply.text;
        button.onclick = () => { Tootsville.parrot.done().then(function () {finish (reply.tag)}); };
        buttonBox.append (button); });
      reply.append (buttonBox);
      reply.style.display = 'block';
      var speech = document.getElementById('parrot-speech');
      speech.style.display = 'none';
      speech.innerHTML = '<h2>' + title + '</h2>' + message;
      speech.style.display = 'block';
      Tootsville.parrot.show(true); })}

Tootsville.parrot.ynP = function (title, message)
{ return Tootsville.parrot.ask (title, message,
                                [  { tag: false, text: "No" },
                                   { tag: true, text: "Yes" } ]); }


// 
Tootsville.parrot.parrotErrorText = function (body)
{ let code = body.error;
  let text = body.status;
  if (! text)
  { if (code)
    { text = "HTTP Error code " + code; }
    else
    { text = "The server did not respond with a regonizeable error reply." } }
  if (! code)
  { code = 500; }
  return "<P>The server reported an error.</P>" +
  "<P>" + text + "</P>" +
  "<P> <A TARGET=\"new\"" +
  " HREF=\"https://wiki.tootsville.org/wiki/Error_" + code + "\">Learn more ...</A></P>"; }
