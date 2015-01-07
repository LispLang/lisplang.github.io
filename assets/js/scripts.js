function fixCodeClasses(block) {
    /* Jekyll emits <code> tags that have classes like 'language-lisp' or
       'language-markdown'. highlight-lisp allegedly supports those. This isn't
       the case. So this function removes the 'language-' prefix. */
    const current_class = $(block).attr('class')
    if(current_class) {
        if(current_class.match('language-lisp')) {
            const new_class = current_class.replace('language-', '');
            $(block).attr('class', new_class);
        }
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
        headers: 'h1, h2, h3',
        minimumHeaders: 1,
        showSpeed: 0
    });
    /* Syntax highlighting */
    $('pre code').each(function(i, block) {
        fixCodeClasses(block);
        highlightEverything(block);
    });
});
