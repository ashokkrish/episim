$(document).ready($(document).on('shiny:inputchanged', function(event) {
    // If the model selection widget was interacted with,
    if (event.name === 'modelSelect') {
        // Typeset all control labels, which generally contain LaTeX.
        $('.control-label').each(function( index ) {
	          MathJax.Hub.Typeset($( this ));
        });

        // Add or remove the extra bottom-margin based on the application state.
        if (!([''].includes(input.modelSelect))) {
            $('div:has(> #modelSelect-label)').css('margin-bottom', '1rem');
        } else {
            $('div:has(> #modelSelect-label)').css('margin-bottom', 0);
        }
    }
}));
