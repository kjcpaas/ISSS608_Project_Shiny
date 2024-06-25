# Ideally, this should be in CSS file but CSS file can't seem to be read if theme is provided
tags$style(
  HTML(
    "
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,100..900;1,100..900&display=swap');

      body, h1, h2, h3, h4, h5, h6 {
        font-family: 'Roboto Condensed', sans-serif;
      }
      
      h3 {
        margin-top: 0;
      }

      #corpGraph-refNodeSelection,
      #networkGraph-refNodeSelection,
      #influenceGraph-refNodeSelection {
        margin-bottom: 1rem;
      }

      #corpGraph-refNodeSelection .dataTables_filter,
      #networkGraph-refNodeSelection .dataTables_filter,
      #influenceGraph-refNodeSelection .dataTables_filter{
        float: none;
      }

      #corpGraph-refNodeSelection .dataTables_scrollHead,
      #networkGraph-refNodeSelection .dataTables_scrollHead,
      #influenceGraph-refNodeSelection .dataTables_scrollHead{
        height: 0;
      }

      #corpGraph-refNodeSelection .dataTables_filter input,
      #corpGraph-refNodeSelection .dataTables_filter label,
      #networkGraph-refNodeSelection .dataTables_filter input,
      #networkGraph-refNodeSelection .dataTables_filter label,
      #influenceGraph-refNodeSelection .dataTables_filter input,
      #influenceGraph-refNodeSelection .dataTables_filter label{
        width: 100%;
        margin: 0;
        border-bottom-left-radius: 0;
        border-bottom-right-radius: 0;
      }
      
      .plot-instruction {
        padding: 4px 6px;
      }
      
      .tabbable .tab-pane {
        padding-top: 8px;
      }
      
      .tabbable .plot-instruction {
        margin-bottom: 8px;
      }
    "
  )
)