documentReadyPainter <-
  function() {
    ## NOTE: Remove the btn-light class from the model select button to make its
    ## interior have a white background, rather than a light background matching
    ## the surrounding sidepanel.
    runjs(r"[$(document).ready($('#modelSelect + button').removeClass('btn-light'))]")
    r"($(document)
     .ready($('div:has(> #modelSelect)')
            .css({
            'border':'var(--bs-border-width) solid #8D959E',
            'border-radius':'var(--bs-border-radius)',
            'transition':'border-color 0.15s ease-in-out,box - shadow 0.15 s ease - in - out '
            })
     )
    )" |>
      gsub(pattern = r"(\n\s*)", replacement = "") |>
      runjs()

    ## NOTE: hack on the styling of the vitalDynamics widget group.
    r"[$(document).ready($('#vitalDynamics-card).css('padding', '10px'))]" |>
      gsub(pattern = r"(\n\s*)", replacement = "") |>
      runjs()
    r"[$(document).ready($('#vitalDynamics-card > div > div.checkbox')
                        .css('margin-bottom', '10px'))]" |>
      gsub(pattern = r"(\n\s*)", replacement = "") |>
      runjs()
    r"[$(document).ready($('#vitalDynamics-card > div')
                        .css({'padding':'10px'}))]" |>
      gsub(pattern = r"(\n\s*)", replacement = "") |>
      runjs()

    ## NOTE: Disable the stochastic radio button and the sidebar to toggle
    ## between sub-apps, respectively. When these are implemented the
    ## respective line can be removed.
    disable(selector = "#stochastic > div:nth-child(2) > label:nth-child(2) > input:nth-child(1)")
    disable(selector = "button.collapse-toggle")
    runjs(r"--($('button.collapse-toggle').hide())--")
  }
