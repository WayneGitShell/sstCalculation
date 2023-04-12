function preventQuitOnReload() {
  Shiny.onInputChange("reload", JSON.stringify(true));
}

$(window).keydown(function(e) {
  switch (e.keyCode) {
    case 116:
      window.onbeforeunload = preventQuitOnReload;
      break;
    case 82:
      if(e.ctrlKey) {
        window.onbeforeunload = preventQuitOnReload;
      }
      break;
    default:
      window.onbeforeunload = null;
  }
});

window.oncontextmenu = function(e) {
  e.preventDefault();
  e.stopPropagation();
  return false;
};

