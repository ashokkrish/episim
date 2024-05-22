$(document)
    .on('shiny:inputchanged', function (event) {
        // If the model selection widget was interacted with,
        if (event.name === 'modelSelect') {
            // typeset LaTeX again.
            MathJax
                .Hub
                .Queue(["Typeset", MathJax.Hub]);
            // FIXME: function does not exist. MathJax.typesetPromise(['beta-label',
            // 'gamma-label', 'xi-label', 'modelLaTeX']);
            if (!([''].includes(input.modelSelect))) {
                // Add 'margin-bottom: 1rem;' to the tag with CSS Selector:
                $('#inputPanel > div:nth-child(1)').css('margin-bottom', '1rem');
            } else {
                // Remove 'margin-bottom: 1rem;' to the tag with CSS Selector: #inputPanel >
                // div:nth-child(1)
                $('#inputPanel > div:nth-child(1)').css('margin-bottom', 0);
            }
        }
    });
