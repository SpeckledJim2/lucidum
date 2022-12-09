// input$dimensions[0] and input$dimensions[1] contain width and height
var dimensions = [0, 0];
$(document).on("shiny:connected", function(e) {
      dimensions[0] = window.innerWidth;
      dimensions[1] = window.innerHeight;
      Shiny.onInputChange("dimensions", dimensions);
  });
$(window).resize(function(e) {
      dimensions[0] = window.innerWidth;
      dimensions[1] = window.innerHeight;
      Shiny.onInputChange("dimensions", dimensions);
});