$( document ).ready(function() {
  var mainContent = $(".wrapper > .content-wrapper");
  var curWidth = 250;
  $("#sidebarWidth").on("change", function() {
      if(parseFloat($(this).val()) < 200 || parseFloat($(this).val()) > 400) {return false}
      curWidth = $(this).val();
      mainContent.css("margin-left", `${curWidth}px`);
      $(".main-header .navbar").css("margin-left", `${curWidth}px`);
      $(".main-header .logo").css("width", `${curWidth}px`);
      $("#sidebarCollapsed").css("width", `${curWidth}px`);
      $("#sidebarItemExpanded").css("width", `${curWidth}px`);
  });
  $(".sidebar-toggle").on("click", function() {
      if(!$("body").hasClass("sidebar-collapse")) {
          mainContent.css("margin-left", `0px`);
          $(".main-header .navbar").css("margin-left", `0px`);
          $(".main-header .logo").css("width", `0px`);
          $("#sidebarCollapsed").css("width", `0px`);
          $("#sidebarItemExpanded").css("width", `0px`);
      } else {
          mainContent.css("margin-left", `${curWidth}px`);
          $(".main-header .navbar").css("margin-left", `${curWidth}px`);
          $(".main-header .logo").css("width", `${curWidth}px`);
          $("#sidebarCollapsed").css("width", `${curWidth}px`);
          $("#sidebarItemExpanded").css("width", `${curWidth}px`);
      }
  })
});
