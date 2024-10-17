$(document).ready(function() {

  var mainContent = $('.wrapper > .content-wrapper');
  var curWidth = 250;  // Default width
  var dragging = false;

  // Function to update widths
  function updateWidths(width) {
    mainContent.css('margin-left', `${width}px`);
    $('.main-header .navbar').css('margin-left', `${width}px`);
    $('.main-header .logo').css('width', `${width}px`);
    $('#sidebarCollapsed').css('width', `${width}px`);
    $('#sidebarItemExpanded').css('width', `${width}px`);
  }
  
  // Mouse down event for draggable element
  $('#draggable').on('mousedown', function(e) {
    dragging = true;
    e.preventDefault();  // Prevent text selection

    $(document).on('mousemove', function(e) {
      if (dragging) {
        var newWidth = e.pageX - $('.wrapper').offset().left;
        
        // Set the top position to its current value
        $('#draggable').css('top', 0 + 'px');
        
        
        if (newWidth >= 10 && newWidth <= 2000) {
          curWidth = newWidth;
          updateWidths(curWidth);
        }
      }
    });

    // Mouse up event to stop dragging
    $(document).on('mouseup', function() {
      dragging = false;
      $(document).off('mousemove');  // Unbind mousemove event
    });
  });

  // Hamburger button behavior
  $('.sidebar-toggle').on('click', function() {
    if (!$('body').hasClass('sidebar-collapse')) {
      updateWidths(0);  // Collapse sidebar
    } else {
      updateWidths(curWidth);  // Expand sidebar
    }
  });
});