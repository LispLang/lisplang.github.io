function fixCodeClasses(block) {
    /* Jekyll emits <code> tags that have classes like 'language-lisp' or
       'language-markdown'. highlight-lisp allegedly supports those. This isn't
       the case. So this function removes the 'language-' prefix. */
    if($(block).attr('class').match('language-lisp')) {
        const new_class = $(block).attr('class').replace('language-', '');
        $(block).attr('class', new_class);
    }
}

function highlightEverything(block) {
    /* Highlight code */
    HighlightLisp.highlight_element(block);
}


$(document).ready(function() {
    /* Create a table of contents */
    $('#toc').toc({
        listType: 'ol',
        noBackToTopLinks: true,
        headers: 'article h1, article h2, article h3',
        showSpeed: 0,
    });
    /* Syntax highlighting */
    $('pre code').each(function(i, block) {
        fixCodeClasses(block);
        highlightEverything(block);
    });
});
