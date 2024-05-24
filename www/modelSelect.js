<<<<<<< HEAD
$(document).ready($(document).on('shiny:inputchanged', function (event) {
    // If the model selection widget was interacted with,
    if (event.name === 'modelSelect' && !([''].includes(event.value))) {
        $('div:has(> #modelSelect-label)').css('margin-bottom', '1rem');
    } else if (event.name === 'modelSelect') {
        $('div:has(> #modelSelect-label)').css('margin-bottom', 0);
    }
}));

// NOTE: the following JS is kept in a comment because it was working 95%.
// During one test the label was not typeset again; in another it was. When the
// label is changed the typesetting causes some flashing, and it would be nicer
// if a stack could be used to prevent re-rendering the label when the label is
// the same. The label is always updated whenever the model type changes,
// however.
//
// // Typeset the changed LaTeX in the tags with the given IDs when their input
// // widget is changed from R.
// $(document).ready($(document).on('shiny:updateinput', function(event) {
//     if (event.target.id === 'beta' && 'label' in event.message) {
//         console.log("DEBUG: beta's label changed!");
//         console.log(event.message);
//         MathJax.Hub.Typeset(['beta-label']);
//     } else if (event.target.id === 'gamma' && 'label' in event.message) {
//         console.log("DEBUG: gamma's label changed!");
//         MathJax.Hub.Typeset(['gamma-label']);
//     }
// }));
=======
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
>>>>>>> 739d908 (Hack on the JavaScript and UI)
