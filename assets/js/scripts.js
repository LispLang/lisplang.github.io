$(document).ready(function() {
    /* Highlight code */
    hljs.initHighlightingOnLoad();
    /* Create a table of contents */
    $('#toc').toc({
        listType: 'ol',
        noBackToTopLinks: true,
        headers: 'article h1, article h2, article h3',
        showSpeed: 0,
    });
});
