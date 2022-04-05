#' Make a participant flow diagram.
#'
#' @param cohort_steroids A lists of lists containing participant counts for different outcomes.
#'
#' @return A participant flow diagram.
#' @export
#'
#' @examples
report_diagram_participant_flow <- function(participant_flow) {
	library(DiagrammeR)
	library(DiagrammeRsvg)
	library(rsvg)
	library(magrittr)
	library(tidyverse)
	

	#For each dataset, make descriptions for each step of the data management process
	counts <- participant_flow %>% 
		mutate(n_and_pyrs=paste("n =", format(n, big.mark = ","), "\np-yrs =", format(round(pyrs), big.mark = ",")),
					 outcome_n_and_pyrs=paste(outcome, "\nn =", format(n, big.mark = ","), "\np-yrs =", format(round(pyrs), big.mark = ",")),
					 description=paste(str_wrap(step, 20), "\nn =", format(n, big.mark = ","), "\np-yrs =", format(round(pyrs), big.mark = ",")))
	

#The diagram uses the graphViz language
full <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      a [label = '@@1']
      b [label = '@@2']
      c [label = '@@3']
      d [label = '@@4']
      
      e1 [label = '@@5']
      f1 [label = '@@6']
      
      e2 [label = '@@7']
      f2 [label = '@@8']
      
      e3 [label = '@@9']
			f3 [label = '@@10']
			
      e4 [label = '@@11']
      f4 [label = '@@12']
      
      invis1[ shape = point, width = 0 ]
      excl1 [label = '@@13']
      
      invis2[ shape = point, width = 0 ]
      excl2 [label = '@@14']
      
      invis3[ shape = point, width = 0 ]
      excl3 [label = '@@15']
      
        
        subgraph cluster_3 {
        node [style=filled];
        e1  e2  e3  e4;
        label = '@@16';
        color=black}
        
        subgraph cluster_4 {
        node [style=filled];
        f1  f2  f3  f4;
        label = '@@17';
        color=black}
        
				
        
				# edge definitions with the node IDs
				a -> invis1[ arrowhead = none ]; 
				invis1 -> b;
				invis1 -> excl1; { rank = same; invis1; excl1 } 
				b -> invis2[ arrowhead = none ]; 
				invis2 -> c;
				invis2 -> excl2; { rank = same; invis2; excl2 }
				c -> invis3[ arrowhead = none ]; 
				invis3 -> d;
				invis3 -> excl3; { rank = same; invis3; excl3 }
				d -> {e1, e2, e3, e4}
				e1 -> f1;
				e2 -> f2;
				e3 -> f3;
				e4 -> f4;
}
      }

      [1]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[1]]
      [2]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[2]]
      [3]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[3]]
      [4]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[4]]
      [5]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[5]]
      [6]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[6]]
      [7]: counts$outcome_n_and_pyrs[counts$outcome=='fract_composite' & counts$analysis=='main'][[5]]
      [8]: counts$outcome_n_and_pyrs[counts$outcome=='fract_composite' & counts$analysis=='main'][[6]]
      [9]: counts$outcome_n_and_pyrs[counts$outcome=='calcium_and_vit_d' & counts$analysis=='main'][[5]]
      [10]: counts$outcome_n_and_pyrs[counts$outcome=='calcium_and_vit_d' & counts$analysis=='main'][[6]]
      [11]: counts$outcome_n_and_pyrs[counts$outcome=='fract_hip' & counts$analysis=='main'][[5]]
      [12]: counts$outcome_n_and_pyrs[counts$outcome=='fract_hip' & counts$analysis=='main'][[6]]
      [13]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[2]]
      [14]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[3]]
      [15]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[4]]
      [16]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[5]]
      [17]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[6]]




      
      ")


#The diagram uses the graphViz language
simple <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      a [label = '@@1']
      b [label = '@@2']
      c [label = '@@3']
      d [label = '@@4']
      e [label = '@@5']
      f [label = '@@6']

      
      
      invis1[ shape = point, width = 0 ]
      excl1 [label = '@@7']
      
			invis2[ shape = point, width = 0 ]
      excl2 [label = '@@8']
      
			invis3[ shape = point, width = 0 ]
      excl3 [label = '@@9']
      
      invis4[ shape = point, width = 0 ]
      excl4 [label = '@@10']
      
      invis5[ shape = point, width = 0 ]
      excl5 [label = '@@11']

        
        
				# edge definitions with the node IDs
				a -> invis1[ arrowhead = none ];
				invis1 -> excl1; { rank = same; invis1; excl1 } 
				invis1 -> b;
				
				b -> invis2[ arrowhead = none ];
				invis2 -> excl2; { rank = same; invis2; excl2 } 
				invis2 -> c;
				
				c -> invis3[ arrowhead = none ];
				invis3 -> excl3; { rank = same; invis3; excl3 } 
				invis3 -> d;
				
				d -> invis4[ arrowhead = none ];
				invis4 -> excl4; { rank = same; invis4; excl4 } 
				invis4 -> e;
				
				e -> invis5[ arrowhead = none ];
				invis5 -> excl5; { rank = same; invis5; excl5 } 
				invis5 -> f;
				

}
      }

      [1]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[1]]
      [2]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[2]]
      [3]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[3]]
      [4]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[4]]
      [5]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[5]]
      [6]: counts$outcome_n_and_pyrs[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[6]]
      [6]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[2]]
      [7]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[3]]
      [8]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[4]]
      [9]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[5]]
      [9]: counts$step[counts$outcome=='bisphosphonate' & counts$analysis=='main'][[6]]





      
      ")


# participant_flow %>%
# 	export_svg %>%
# 	charToRaw %>%
# 	rsvg_svg("output/participant_flow.svg")



# This will be saved to the list: -----------------------------------------
set_names(list(full, simple),
					c("full", "simple"))

}
