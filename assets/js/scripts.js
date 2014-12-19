$(document).ready(function() {
    /* Highlight code */
    hljs.initHighlightingOnLoad();
    /* Create a table of contents */
    $('#toc').toc({
        'selectors': 'h1,h2,h3', // Only detect headers down to three levels
        'container': 'article',
        'smoothScrolling': true,
    });
});
