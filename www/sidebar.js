$(document)
    .ready(function() {
        $('aside#mainSidebar').hide();
        // Keep trying to hide the button until it works.
        do {
            $('button.collapse-toggle').hide();
        } while ($('button.collapse-toggle').length() === 0);
    });
