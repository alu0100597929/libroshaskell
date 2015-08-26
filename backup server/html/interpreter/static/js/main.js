var radios = document.getElementsByName('codesample');

/* codemirror */

var editor = CodeMirror.fromTextArea(document.getElementById("code"), {
  mode: "scheme",
  lineNumbers: true,
  gutters: ["CodeMirror-linenumbers", "breakpoints"],
  theme: "ambiance"
});

var rellenarTextarea = function()
{
  //var textareaCode = document.getElementById('code');

  for (var i = 0 ; i < radios.length ; i++)
  {
      if (radios[i].checked)
      {
          editor.getDoc().setValue(radios[i].value);
          break;
      }
  }
};

function eventosRadios()
{
  //var radios = document.getElementsByName('codesample');

  for (var i = 0 ; i < radios.length ; i++)
  {
    radios[i].onclick = rellenarTextarea;
  }
}

// al pulsar ctrl + B se ejecuta el programa que haya pegado
function combinacionEjecutar(e)
{
  var evtobj = window.event? event : e;
  if (evtobj.keyCode == 66 && evtobj.ctrlKey && evtobj.shiftKey)
    document.getElementById('botonejecutar').click();
}

document.onkeydown = combinacionEjecutar;

window.onload = function()
{
  eventosRadios();
};