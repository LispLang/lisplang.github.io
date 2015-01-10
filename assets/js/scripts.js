$(document).ready(function() {
    /* Create a table of contents */
    $('#toc').toc({
        listType: 'ol',
        noBackToTopLinks: true,
        headers: 'h1, h2, h3',
        minimumHeaders: 1,
        showSpeed: 0
    });
    /* Syntax highlighting */
    HighlightLisp.highlight_auto({
        className: 'language-lisp'
    });
});
