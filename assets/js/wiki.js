$(document).ready(function() {
  function addAnchorsToHeadings() {
    const sel = 'article h1, article h2, article h3, article h4, article h5, article h6';
    $(sel).each(function(index, item) {
      const id = $(item).attr('id');
      if (id) {
        const anchor = '<a href="#' + id + '" class="link"/>';
        $(item).prepend(anchor);
      }
    });
  }

  function enableLinkVisibilityToggle() {
    $('#hide-links').on('click', function() {
      const selector = 'article'
      const state = $(selector).attr('hide-links');
      if (state) {
        $(selector).attr('hide-links', null);
        $(this).attr('hide-links', null);
        $(this).text('off');
      } else {
        $(selector).attr('hide-links', 'true');
        $(this).attr('hide-links', 'true');
        $(this).text('on');
      }
    });
  }

  $(document).ready(function() {
    addAnchorsToHeadings();
    enableLinkVisibilityToggle();
  });
});
