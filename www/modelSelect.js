$(document).ready($(document).on('shiny:inputchanged', function(event) {
    // If the model selection widget was interacted with,
    if (event.name === 'modelSelect' && !([''].includes(event.value))) {
        $('div:has(> #modelSelect-label)').css('margin-bottom', '1rem');
        // Protect the following expressions from running until the visualchange
        // event is fired, i.e. the conditionalPanels have changed.
        $(document).on('shiny:visualchange', function(event) {
            console.log("Shiny visual change!");
            var hiddenInputsArray = [];
            $('.shiny-bound-input:hidden').each(function(index) {
                hiddenInputsArray.push($(this).attr('id'));
            });
            Shiny.setInputValue('hiddenInputs', hiddenInputsArray);
        });
    } else if (event.name === 'modelSelect') {
        $('div:has(> #modelSelect-label)').css('margin-bottom', 0);
    }
}));