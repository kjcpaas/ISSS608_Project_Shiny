# Ideally, this should be in CSS file but CSS file can't seem to be read if theme is provided
tags$style(
  HTML(
    "
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100..900;1,100..900&display=swap');

      body, h1, h2, h3, h4, h5, h6 {
        font-family: 'Roboto Condensed', sans-serif;
      }

      #networkGraph-refNodeSelection, #influenceGraph-refNodeSelection {
        margin-bottom: 1rem;
      }

      #networkGraph-refNodeSelection .dataTables_filter,
      #influenceGraph-refNodeSelection .dataTables_filter{
        float: none;
      }

      #networkGraph-refNodeSelection .dataTables_scrollHead,
      #influenceGraph-refNodeSelection .dataTables_scrollHead{
        height: 0;
      }

      #networkGraph-refNodeSelection .dataTables_filter input,
      #networkGraph-refNodeSelection .dataTables_filter label,
      #influenceGraph-refNodeSelection .dataTables_filter input,
      #influenceGraph-refNodeSelection .dataTables_filter label{
        width: 100%;
        margin: 0;
        border-bottom-left-radius: 0;
        border-bottom-right-radius: 0;
      }
    "
  )
)