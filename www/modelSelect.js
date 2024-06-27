$(document)
.ready(function() {
    $('head').add('<link rel="stylesheet" href="modelSelect.css">');


    $(document).on('shiny:inputchanged', function(event) {
        // If the model selection widget was interacted with, and it't not empty...
        if (event.name === 'modelSelect' && !([''].includes(event.value.split("_")[0]))) {
            // modify the CSS applied to modelSelect.
            $('div:has(> #modelSelect-label)').css('margin-bottom', '1rem');

            // Protect the following expressions from running until the visualchange
            // event is fired, i.e. the conditionalPanels have changed.
            $(document).on('shiny:visualchange', function(event) {
                var hiddenInputsArray = [];
                $('.shiny-bound-input:hidden').each(function(index) {
                    hiddenInputsArray.push($(this).attr('id'));
                });
                Shiny.setInputValue('hiddenInputs', hiddenInputsArray);
            });
        } else if (event.name === 'modelSelect') {
            // Modify the CSS applied to modelSelect.
            $('div:has(> #modelSelect-label)').css('margin-bottom', 0);
        }
    });
});
