function checkServer (uri)
{ document.writeln("<SPAN ID='check-" + uri + "'><I CLASS='fa fa-gear fa-spin'></I></SPAN>");
  setTimeout(function () { reallyCheckServer (uri) }, 150); }

function reallyCheckServer (uri)
{ fetch ("https://" + uri, { mode: 'no-cors' }).then (
    (response) =>
        { if (response.ok)
          { document.getElementById("check-" + uri).innerHTML = "<FONT COLOR='green'> GO </FONT>"; }
          else
          { document.getElementById("check-" + uri).innerHTML = "<FONT COLOR='red'> NO GO </FONT>"; } } ).
  catch (
      () => { document.getElementById("check-" + uri).innerHTML = "<FONT COLOR='red'> FAIL </FONT>"; } );; }
          
          
