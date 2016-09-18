---
---

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

var autocompleteList = [
  {% for post in site.categories['wiki'] %}
  ["{{ post.title }}", "{{ post.slug }}"],
  {% endfor %}
];

$(document).ready(function() {
  addAnchorsToHeadings();
  enableLinkVisibilityToggle();

  var autocompleteFuse = new Fuse(autocompleteList, { keys: ['title', '0'] });
  var completion = new autoComplete({
    selector: 'input[name="search"]',
    minChars: 1,
    delay: 0,
    source: function(term, suggest) {
      suggest(autocompleteFuse.search(term));
    },
    renderItem: function (item, search) {
      return '<div class="autocomplete-suggestion" data-slug="' + item[1] + '">' + item[0] + '</div>';
    },
    onSelect: function(event, term, item) {
      var slug = item.getAttribute('data-slug');
      window.location = 'wiki/article/' + slug;
    }
  });
});
