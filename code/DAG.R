pacman::p_load(
  DiagrammeR,     # for flow diagrams
  DiagrammeRsvg,  # save the DAG to image file
  rsvg,
  tidyverse       
)

UPR_dag <- DiagrammeR::grViz("               
digraph {   
  
  # graph layout
  #################
  graph [layout = dot,
         rankdir = BT            # layout left-to-right
         # fontsize = 10
         # , splines = ortho       # straight lines rather than curved
         ]
  
  # nodes (rectangles)
  #################
  node [shape = rectangle, style = filled, fontsize = 16]                      
  
  # graph statement
  #################
  BaselineMMR     [label = 'Baseline\nMMR']
  Recommending    [label = 'Recommending State\ninterest']
  Health_system     [label = 'Health system']
  national_budget   [label = 'Government\nhealth spending']
  external_finance   [label = 'External finance']
  SRHR_policy            [label = 'SRHR policies']
  abortion    [label = 'Access to\nsafe abortion']
  maternity_care   [label = 'Maternity care\nservices']
  personal_expenditure [label = 'Personal health\nexpenditure']
  government_effectiveness [label = 'Government\neffectiveness',
  # color = Brown,
  #           # fontcolor = Brown,
  #           style = filled,
  #           fillcolor = Tomato
  ]
  political_stability [label = 'Political\nstability',
  # color = Brown,
  #           # fontcolor = Brown,
  #           style = filled,
  #           fillcolor = Tomato
  ]
  Implementation [label = 'Policy\nimplementation']
  civil_society [label = 'Civil society\nmobilization']
  geographic_access [label = 'Geographic\naccess to care']
  fertility_rate [label = 'Fertility\nrate']
  family_planning [label = 'Family\nplanning']
  unintended_pregnancy [label = 'Unintended\npregnancy']
  age_first_pregnant [label = 'Age at first\npregnancy']
  conflict [label = 'Conflict\nstatus',
            color = Brown,
            # fontcolor = Brown,
            style = filled,
            fillcolor = Tomato]
  GDP [label = 'State financial\nresources (GDP)',
              color = Brown,
              # fontcolor = blue,
              style = filled,
              fillcolor = Tomato
  ]
  rec_relations [label = 'Recommending State\nrelationship with SUR']
  family_income [label = 'Family\nincome']
  
  
  Recommendations   [label = 'UPR recommendations',
  color = darkgreen,
             fontcolor = white, 
             style = filled,
             fillcolor = darkgreen
              ] 
                     
  Acceptance [label = 'State acceptance of\nUPR recommendations',
              # color = blue,
              # fontcolor = blue,
              # style = filled,
              # fillcolor = paleturquoise
              ] 
              
  MMR        [label = 'MMR',
             color = blue,
                     fontcolor = blue,
              style = filled,
              fillcolor = paleturquoise
             ]
  
  # edges
  #######
  BaselineMMR   -> {Recommendations Acceptance MMR Recommending}
  
  # grouped edge
  {Recommendations} -> MMR [
                                      fontcolor = darkgreen,
                                      color = darkgreen,
                                      style = dashed
                                      ]
  {civil_society} -> Recommending [
                                      fontcolor = red,
                                      color = red,
                                      style = dashed
                                      ]
  # {geographic_access Health_system Education personal_expenditure} -> {skilled_birth anc pnc abortion emergency_care} -> MMR
  {geographic_access Health_system Education personal_expenditure} -> {abortion maternity_care} -> MMR

  Recommendations -> Acceptance
                             {Recommendations Acceptance} -> {SRHR_policy civil_society} -> Implementation -> {MMR Health_system Education}
                  civil_society -> SRHR_policy
                  national_budget -> {Implementation Acceptance Health_system}
                  Health_system -> {MMR family_planning personal_expenditure}
                  Recommending -> {Recommendations external_finance}
                  external_finance -> SRHR_policy
                  Acceptance -> external_finance -> {Implementation Health_system}
                  Education -> family_planning -> unintended_pregnancy -> {abortion MMR}
                  Education -> {family_income Nutrition}
                  {government_effectiveness political_stability} -> {Recommending Acceptance Implementation Health_system Education national_budget}
                  family_planning -> fertility_rate -> {MMR}
                  {family_planning unintended_pregnancy} -> age_first_pregnant -> MMR
                  family_income -> {geographic_access Nutrition personal_expenditure}
                  Nutrition -> MMR
                  conflict -> {Recommending MMR national_budget family_income Education Health_system}
                  rec_relations -> {Recommending Acceptance}
                  GDP -> {Recommending Education national_budget Health_system family_income}
  
  # --- SUBGRAPHS (RANKING) ---
  
  subgraph {
    rank = same; conflict; GDP; government_effectiveness; political_stability;
  }
  
  # subgraph {
  #   rank = same; skilled_birth; anc; pnc; abortion; emergency_care;
  # }
  
  subgraph {
    rank = same; fertility_rate; maternity_care;
  }
  
  GDP -> BaselineMMR [style=invis]
  age_first_pregnant -> abortion [style=invis]

}
");UPR_dag

UPR_dag %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_png("DAG.png", height = 2000)