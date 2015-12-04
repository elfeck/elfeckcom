registerButton = function(button, pressFunction) {
  button.on("mouseenter", function() {
    button.removeClass("buttonidle");
    button.addClass("buttonhover");
  });
  button.on("mouseout", function() {
    button.removeClass("buttonpress");
    button.removeClass("buttonhover");
    button.addClass("buttonidle");
  });
  button.on("mousedown", function() {
    button.removeClass("buttonhover");
    button.addClass("buttonpress");
  });
  button.on("mouseup", function() {
    button.removeClass("buttonpress");
    button.addClass("buttonhover");
    pressFunction();
  });
}
