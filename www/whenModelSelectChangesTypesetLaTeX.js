// https://docs.mathjax.org/en/latest/web/typeset.html#typesetting-math-in-a-web-page
$(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'modelSelect') {
        if (window.MathJax) MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
        // FIXME: The MathJax documentation says this is the function to call
        // when math is added to the page dynamically, however, this function
        // isn't recognized. It's probably the version of MathJax that is loaded
        // with Shiny that is causing the issue.
        // if (window.MathJax) MathJax.typeset();
    }
});
