;; this model address the role of opportunity structure (e.g., variying level of selective exposure)
;; and role of individaul political interest

globals
[ number_of_agents ]


patches-own  ;; each patches provides media exposure
[
  red-media ;; conservative-leaning media exposure
  blue-media ;; liberal-leaning media exposure
]

turtles-own
[ political-interest  ;; interest and the level of engagement with politics
  attitudes   ;; an attitude of an agent (-3: oppose or +3: support). We defined the "supporting issue" means he or she is a Democrat.
  prior-attitudes  ;; attitudes at prior waves
  total_social  ;; sum of attitudes around me from social networks
  total_media_valence  ;; valence of media influence
  total_media   ;; sum of media exposure
  tally  ;; sum of one's own attitudes, total, and mass media exposure
  my-connected-neighbors  ;; other actors in their networks
  agree ;; degree of agreement on his or her social networks
  disagree ;; degree of disagreement on his or her social networks
  diversity
  total
  rep
  ind
  dem

]

to setup
  random-seed custom-random-seed ;; accepts input from RNetlogo command
  clear-all
  clear-all-plots

  resize-world 0 (world-size-x - 1) 0 (world-size-y - 1) ;; defining the size of the society (number of patches)
  set number_of_agents (world-size-x * world-size-y)     ;; one agent per patch
  set-patch-size 360 / world-size-y                      ;; setting patch size for good looking

  setup-turtles                                          ;; creating the agents, locating them and setting their attitudes values randomly

  ask turtles
    [ if political-interest < 0.3  ;; low-end of preference-for-politics (i.e., political interest)
      [ set attitudes -1 + random 3 ]
      if political-interest >= 0.3 and political-interest < 0.7  ;; middle of preference-for-politics (i.e., political interest)
      [ ifelse random-float 1 < 0.5
        [ set attitudes -2 + random 3 ]
        [ set attitudes 0 + random 3 ]]
      if political-interest >= 0.7 ;; high-end of preference-for-politics (i.e., political interest)
      [ ifelse random-float 1 < 0.5
        [ set attitudes -3 + random 2 ]
        [ set attitudes 2 + random 2 ]]

      recolor-turtles ]

  reset-ticks
end


;; Turtles settings
to setup-turtles
  set-default-shape turtles "default"
  create-turtles number_of_agents [
    set size 0.8
    while [any? other turtles-here] [ move-to one-of patches ] ;; setting agent location. Only one agent at each patch
    set political-interest random-normal-in-bounds 0.5 0.2 0 1
  ]

end


;; this is master behavioral schedule of an agent (= turtles)
to go

  if count turtles with [ attitudes > 0 ] = 0 or count turtles with [ attitudes < 0 ] = 0 [stop] ;; if one of the partisan camp disappears completely, the model stops
  if ticks >= 1095 [stop] ;; after 1000 iteration, model stops

  if model-election-cycle = "Yes" ;; see "The Soul of a Polarized Democracy" in CR by Binder et al (2009).
  [
  ;; during election
  if (ticks = 91 or ticks = 456 or ticks = 821) [
                  set media-influence-parameter 0.5
                  set social-influence-parameter 0.5
                  set propensity-for-homophily 0.6
                  set exposure-to-counter-media 0.6 ]

  ;; after election
  if (ticks = 274 or ticks = 639 or ticks = 1004) [
                   set media-influence-parameter 0.4
                   set social-influence-parameter 0.4
                   set propensity-for-homophily 0.5
                   set exposure-to-counter-media 0.5]
  if (ticks = 292 or ticks = 657 or ticks = 1022) [
                   set media-influence-parameter 0.3
                   set social-influence-parameter 0.3
                   set propensity-for-homophily 0.4
                   set exposure-to-counter-media 0.4 ]

  ;; approaching election
  if (ticks = 438 or ticks = 803) [
                   set media-influence-parameter 0.35
                   set social-influence-parameter 0.35
                   set propensity-for-homophily 0.4
                   set exposure-to-counter-media 0.4 ]

  ]

  if model-dropout-based-on-preference-for-politics = "Yes" ;; see "Media and polarization" by Prior (2007; 2013), especially polarization without persuasion part.
  ;; what if moderates are dropping out from engaging with partisan media?
  ;; what if availability of partisan media sources vary geographically?
  [
    ask turtles
    [ if political-interest < 0.3 and attitudes = 0 and random-float 1 < 0.2 [ die ]
      move ]

    if ((ticks > 274 and ticks < 456) or (ticks > 639 and ticks < 821)) and count turtles < number_of_agents
    [ let n-new-turtles (number_of_agents - count turtles) / 10
      ask n-of random n-new-turtles patches with [not any? turtles-here] [ sprout 1 [
          set political-interest random-normal-in-bounds 0.3 0.1 0 0.5
          if political-interest < 0.3  ;; low-end of political-interest
          [ set attitudes -1 + random 3 ]
          if political-interest >= 0.3 and political-interest < 0.7  ;; middle of political-interest
          [ ifelse random-float 1 < 0.5
            [ set attitudes -2 + random 3 ]
            [ set attitudes 0 + random 3 ]]
          if political-interest >= 0.7 ;; high-end of political-interest
          [ ifelse random-float 1 < 0.5
            [ set attitudes -3 + random 2 ]
            [ set attitudes 2 + random 2 ]]

      ] ] ]

  ]

  ask turtles [ select-discussion-partners ]
  ask-concurrent turtles [ discuss-politics ]
  ask-concurrent turtles [ watch-media ]
  if (indirect-exposure = "Yes") [ask-concurrent turtles [ indirect-media-exposure ]]
  ask-concurrent turtles [ update-opinion ]

  ;; for display purposes
  ask turtles [ recolor-turtles ]

  tick

  ;; reset for next tick (have to place after "tick" in order to avoid conflicts with plot updates)
  ask patches [set blue-media 0]
  ask patches [set red-media 0]

end


to watch-media  ;; how "selective" exposure map into this submodel? (direct exposure)

  ifelse attitudes < 0 ;; if republicans
  [
    if attitudes < random -2 [ set red-media 1 ]              ;; attitudes (-3,-2,-1) < (-1,0)?? --> (-3) and (-2) always get red media exposure, and (-1) randomly get red media exposure.
    if attitudes < random -3 [ set red-media (red-media * information-decay) + 1 ]  ;; attitudes (-3,-2,-1) < (-2,-1,0)?? --> (-3) always get red media exposure, while (-2) and (-1) randomly get red media exposure.
    if attitudes < random -4 [ set red-media (red-media * information-decay) + 1 ]  ;; attitudes (-3,-2,-1) < (-3,-2,-1,0)?? --> all randomly get red media exposure.


    if (media-interest-interaction = "Yes") ;; discount the degree of selective exposure by the lack of political interest
    [set red-media (red-media * political-interest)]

    if exposure-to-pro-media < 1  ;; (global) if exposure-to-pro-media parameter is less than 1, do following...

    ;; this scenario addresses European (relative to American) cases where the opportunity for selective exposure is much limited.
    [ set red-media (red-media * exposure-to-pro-media) ]

    ;; this senario discounts the degree of selective exposure to the factor equal to "exposure-to-pro-media" value.
    ;; e.g., if exposure-to-pro-media == 0.5, then
    ;; strong republicans (-3) get a max value of 1.5 red-media exposure
    ;; weak republicans (-2) get a max value of 1 red-media exposure
    ;; and republican leaners (-1) get a max value of 0.5 red-media exposure
  ]

  [ ;; if neutral
    ifelse attitudes = 0
    [ if random-float 1 <= exposure-to-pro-media
      [ let q random 3
        set red-media random 2
        set blue-media random 2
        repeat q [set red-media (red-media * information-decay) + 1]
        repeat q [set blue-media (blue-media * information-decay) + 1]
        set red-media (red-media * exposure-to-pro-media)
        set blue-media (blue-media * exposure-to-pro-media)]  ;; randomly get blue and red media exposure, to varying (random) degree from 0 to 3

       if (media-interest-interaction = "Yes") ;; discount the degree of selective exposure by the lack of political interest
       [ set red-media red-media * political-interest
         set blue-media blue-media * political-interest ]

     ;; (global) if exposure-to-pro-media parameter is less than 1, above formula also takes care of the discounting factor

    ]

  [ ;; finally, for democrats,
    if attitudes > random 2 [ set blue-media 1 ]               ;; attitudes (1,2,3) > (0,1)?? --> (2) and (3) always get blue-media exposure, and (1) randomly get blue media exposure
    if attitudes > random 3 [ set blue-media (blue-media * information-decay) + 1 ]  ;; attitudes (1,2,3) > (0,1,2)?? --> (3) always get blue-media exposure, whereas (2) and (1) randomly get blue media exposure
    if attitudes > random 4 [ set blue-media (blue-media * information-decay) + 1 ]  ;; attitudes (1,2,3) > (0,1,2,3)?? --> all randomly get blue media exposure

    if (media-interest-interaction = "Yes") ;; discount the degree of selective exposure by the lack of political interest
    [ set blue-media blue-media * political-interest ]

    if exposure-to-pro-media < 1  ;; (global) if exposure-to-pro-media parameter is less than 1, do following...
      [ set blue-media (blue-media * exposure-to-pro-media) ]

   ]  ;; end of democrat only
  ] ;; end of neutral & democrat


  if exposure-to-counter-media > 0   ;; (global) any setting for exposure-to-counter-media value greater than zero

  [ ifelse attitudes < 0 ;; if republicans
     [ set blue-media exposure-to-counter-media * (exposure-to-pro-media * red-media) ]
     [ ;; if neutral
      ifelse attitudes = 0
      [ set red-media random (4 * exposure-to-pro-media)
        set blue-media random (4 * exposure-to-pro-media)  ;; randomly get blue and red media exposure, to varying (random) degree from 0 to 3
      ]
      [ ;; finally, for democrats,
      set red-media exposure-to-counter-media * (exposure-to-pro-media * blue-media) ] ;; end of democrat only
      ] ;; end of neutral & democrat

  ]

  if (selective-avoidance = "Yes") ;; whether strong partisans (i.e., those who have higher political interest) actively avoid counter-attitudinal information?

  [if attitudes < 0 ;; if republicans
    [ set blue-media blue-media * (1 - political-interest) ]
   if attitudes > 0 ;; if democrats
    [ set red-media red-media * (1 - political-interest) ] ]


  ;; how much of "support-side" considerations from media exposure? any value greater than zero means media favors supporting the issue (=democrat-oriented)
  set total_media_valence (blue-media - red-media)
  set total_media (blue-media + red-media)

end



;; this part describes selection rules for potential discussants, which is based on homophily in their attitudes

to select-discussion-partners

ifelse discussant-select-base-on-homophily = true

[ repeat count neighbors  ;; for every neighbors repeat following

       [ let neighbor one-of turtles-on neighbors ;; define one of neighboring agents as "neighbor"

         if any? turtle-set neighbor [
         let q random-float 1

         ifelse ( ( count link-neighbors with [ similarity-between neighbor self >= propensity-for-homophily ] / (count neighbors)) <= propensity-for-homophily )
         [ if q < 0.3 [create-link-with neighbor] ] ;; value 0.3 determines the baseline num. of connected alters
         [ if similarity-between neighbor self >= propensity-for-homophily [create-link-with neighbor] ]

         ; this results in a situation where
         ; (a) for greater value of "propensity-for-homophily" (e.g., 0.8), selection of discussants based on homophily is more prevalent while there's still random connections
         ; (b) for smaller value of "propensity-for-homophily" (e.g., 0.2), random selection is more prevalent


         set my-connected-neighbors link-neighbors ]] ; end repeat procedure

    ;; define the agent-set "my-connected-neighbors" from linked neighbors

  ask links [die] ]  ;; clear links for next tick

[ set my-connected-neighbors turtles-on neighbors ]  ;; (global) if "false", agents interact with all other neighboring agents (N=8)


end


to-report similarity-between [turtle1 turtle2]
  let A [attitudes] of turtle1
  let B [attitudes] of turtle2
  let sim 0
  ;; define the similarity as the abolute distance of two attitudes
  set sim ((6 - abs(A - B)) / 6) ; sim ranges from 0 to 1, with 0 = max dissimilairty, and 1 = max similarity
  report sim
end



to discuss-politics

ifelse any? my-connected-neighbors  ; for any agent who has at least one connected neighbors,
[
   ; for mean average, define "total" as the mean opinion distribution of one's network (e.g., DeGroot, 1974; Friedkin, 1998)
   if social-influence-model = "mean-average-model" ;; similar to naive-learning and informational influence idea
   [ set total_social mean [attitudes] of my-connected-neighbors ]


   ; for majority modle, define "total" as the modal value of opinion distribution in one's network (e.g., Axelrod 1997; Watts & Dodds, 2007)
   if social-influence-model = "majority-model" ;; similar to "threshold model" and normative influence idea
   [ set total_social mean (modes [attitudes] of my-connected-neighbors) ]


   if attitudes > 0 ;; if an agent supports the position (== Democrats)
   [ set disagree (count my-connected-neighbors with [ attitudes < 0 ]) / count my-connected-neighbors ]
   if attitudes < 0 ;; if an agnet opposes the position (== Republicans)
   [ set disagree (count my-connected-neighbors with [ attitudes > 0 ]) / count my-connected-neighbors ]
   if attitudes = 0 ;; for neutral
   [ set disagree (count my-connected-neighbors with [ attitudes != 0 ]) / count my-connected-neighbors ]

   set total count my-connected-neighbors
   set rep (count my-connected-neighbors with [ attitudes < 0 ]) / count my-connected-neighbors
   set ind (count my-connected-neighbors with [ attitudes = 0 ]) / count my-connected-neighbors
   set dem (count my-connected-neighbors with [ attitudes > 0 ]) / count my-connected-neighbors
   set diversity (1 - (rep * rep) - (ind * ind) - (dem * dem))
]

[ ;; if there's no connected neighbors
  set total_social 0
  set disagree 0
  set diversity 0 ]

end



to indirect-media-exposure

if any? my-connected-neighbors  ; for any agent who has at least one connected neighbors,

[
  let red-media-indirect (sum [red-media] of my-connected-neighbors)
  let blue-media-indirect (sum [blue-media] of my-connected-neighbors)

  set red-media (red-media + red-media-indirect)
  set blue-media (blue-media + blue-media-indirect)

  ]

end



to update-opinion  ;;

  set prior-attitudes attitudes ;; copy the value of current attitudes before updating
  let SINP social-influence-parameter ;; initial value is 0.4
  let MINP media-influence-parameter ;; initial value is 0.4

  let STBP (1 - random-decay-parameter) ; random-decay-parameter defines the baseline attitude stability (i.e., random decay value)

  if any? my-connected-neighbors  ; for those who have social network influence, decay value increases (low attitude certainty)
  [ set agree 1 - disagree
    let qq (agree - disagree) / count my-connected-neighbors
    let decay_multiplier (1 + e ^ (- qq)) / 2
    set STBP (STBP * decay_multiplier)

    let media_multiplier 2 / (1 + e ^ (- qq))
    set MINP (MINP * media_multiplier) ;; media influence parameter would be weighted by "media_multiplier" (determined by the proportional difference of agree vs disagreeable discussants)

    ;;let social_multiplier 2 / (1 + e ^ (- qq))
    ;; set SINP (SINP * social_multiplier)

    ]

  let prior-attitudes-for-tally attitudes * (1 - STBP)  ;; if republican, a value of attitude increases. if democrat, a value of prior attitude decreases (randomly regresses to neutral point).



if opinion-update-model = "WMA-disagree-pro-and-counter-exposure" ;; interpersonal discussion moderation model

[   ifelse any? my-connected-neighbors
  [ ;; if there's connected neighbors

    ;; for counter-exposure: we previously defined MINP to be alreaday weighted by media_multiplier (MINP = MINP * media_multiplier)
    ;; in order to map equation 2 in paper, we implement following: media-influence-parameter / media_multiplier
    ;; which is same as media-influence-parameter * MINP / { modified "MINP" = MINP * media_multiplier}

    ;; we additionally model the effect of political interest ("preference-for-politics")

    if attitudes < 0  ;; for Republicans
    ;; previously, [ set tally (prior-attitudes-for-tally + (media-influence-parameter ^ 2 / MINP) * blue-media - (MINP * red-media) + SINP * total_social) ]
    [ set tally (prior-attitudes-for-tally + (media-influence-parameter ^ 2 / MINP) * (1 - political-interest) * blue-media - (MINP * red-media) + SINP * total_social) ]


    if attitudes > 0  ;; for democrats
    ;; previously, [ set tally (prior-attitudes-for-tally + (MINP * blue-media) - ((media-influence-parameter ^ 2 / MINP) * red-media) + SINP * total_social) ]
    [ set tally (prior-attitudes-for-tally + (MINP * blue-media) - ((media-influence-parameter ^ 2 / MINP) * (1 - political-interest) * red-media) + SINP * total_social) ]


    if attitudes = 0 ;; for independents
    [ set tally (prior-attitudes-for-tally + (media-influence-parameter ^ 2 / MINP) * blue-media - ((media-influence-parameter ^ 2 / MINP) * red-media) + SINP * total_social) ] ]

  [ ;; if there's no connected neighbors

    ;; previously, set tally (prior-attitudes-for-tally + media-influence-parameter * blue-media - media-influence-parameter * red-media) ] ;; for those who have no connected neighbors, social influence is not considered..

    if attitudes < 0  ;; for Republicans
    [set tally (prior-attitudes-for-tally + media-influence-parameter * (1 - political-interest) * blue-media - media-influence-parameter * red-media) ]

    if attitudes > 0  ;; for democrats
    [set tally (prior-attitudes-for-tally + media-influence-parameter * blue-media - media-influence-parameter * (1 - political-interest) * red-media) ]

    if attitudes = 0 ;; for independents
    [set tally (prior-attitudes-for-tally + media-influence-parameter * blue-media - media-influence-parameter * red-media) ]]


  ;; finally, update attitudes
  ifelse tally < prior-attitudes [set attitudes prior-attitudes - 1]
  [ifelse tally > prior-attitudes [set attitudes prior-attitudes + 1]
    [set attitudes prior-attitudes]]

 ]


if opinion-update-model = "weighted-mean-average"

[ ifelse any? my-connected-neighbors
  [ ;; if there's connected neighbors

    set tally (prior-attitudes-for-tally + (media-influence-parameter * blue-media - media-influence-parameter * red-media) + SINP * total_social) ]

  [ ;; if there's no connected neighbors

    set tally (prior-attitudes-for-tally + media-influence-parameter * blue-media - media-influence-parameter * red-media) ]  ;; for those who have no connected neighbors, social influence is not considered..

  ;; finally, update attitudes
  if tally < prior-attitudes [set attitudes prior-attitudes - 1]
  if tally > prior-attitudes [set attitudes prior-attitudes + 1]

 ]


  ;; setting the end-limits of attitude scale
  if attitudes < -3 [ set attitudes -3 ]
  if attitudes > 3 [ set attitudes 3 ]


end


to recolor-turtles

  if attitudes = 0 [ set color 9.9 ]

  if attitudes = -1 [ set color 17 ]
  if attitudes = -2 [ set color 16 ]
  if attitudes = -3 [ set color 15 ]

  if attitudes = 1 [ set color 97 ]
  if attitudes = 2 [ set color 96 ]
  if attitudes = 3 [ set color 95 ]


end

;; a series of reporter for outcome variables

to-report attitude-towards-extreme

  let changed-attitude-towards-extreme 0
  let attitude-change (attitudes - prior-attitudes)

  if prior-attitudes < 0 ;; Republican at prior-wave
  [ if attitude-change < 0 [ set changed-attitude-towards-extreme 1 ]
    if attitude-change > 0 [ set changed-attitude-towards-extreme -1 ] ]

  if prior-attitudes > 0 ;; Democrats at prior-wave
  [ if attitude-change > 0 [ set changed-attitude-towards-extreme 1 ]
    if attitude-change < 0 [ set changed-attitude-towards-extreme -1 ] ]

  report changed-attitude-towards-extreme

end

to-report random-normal-in-bounds [mean-val dev mmin mmax]
  let result random-normal mean-val dev
  if result < mmin or result > mmax
    [ report random-normal-in-bounds mean-val dev mmin mmax ]
  report result
end


to move ;; random moving procedure

    let empty-patches neighbors with [not any? turtles-here]
    if any? empty-patches
      [ let target one-of empty-patches
        face target
        move-to target ]

end


to-report kurtosis [variable]

  let s (standard-deviation variable)
  let n (length variable)
  let mu (mean variable)

  let x map [ [?1] -> ?1 - mu ] variable

  let r (  n * sum ( map [ [?1] -> ?1 ^ 4 ] x) / ((sum ( map [ [?1] -> ?1 ^ 2 ] x)) ^ 2)  )

  let kurt ( ((n + 1) * (r - 3) + 6) * (n - 1)/((n - 2) * (n - 3)) )
  report kurt

end

to-report less-than [num lst]
  let newlist filter [ [?1] -> ?1 < num ] lst
  report newlist
end

to-report equal-to [num lst]
  let newlist filter [ [?1] -> ?1 = num ] lst
  report newlist
end

to-report greater-than [num lst]
  let newlist filter [ [?1] -> ?1 > num ] lst
  report newlist
end

to-report euclidean-distance [lst1 lst2]  ;; report mean pairwise-distance of every elements in list1 and list2

   let sum_total 0

   let n length lst1
   let i 0

   while [ i < n]
   [ let val item i lst1
     set sum_total sum_total + sum (map [ [?1] -> abs(val - ?1) ] lst2)
     set i (i + 1)
   ]

   ifelse ((length lst1) = 0) or ((length lst2) = 0)
   [ set sum_total 0
     report sum_total ]
   [ report sum_total / (length lst1 * length lst2)]

end


to-report ER.pol.index [variable] ;; variable = attitudes

  let rep_p 0
  let rep_mu 0
  let ind_p 0
  let ind_mu 0
  let dem_p 0
  let dem_mu 0

  set rep_p length less-than 0 variable / length variable
  set ind_p length equal-to 0 variable / length variable
  set dem_p length greater-than 0 variable / length variable

  ifelse rep_p > 0
  [set rep_mu mean less-than 0 variable]
  [set rep_mu 0]

  ifelse dem_p > 0
  [set dem_mu mean greater-than 0 variable]
  [set dem_mu 0]

  let ERindex (  (rep_p ^ 2) * ind_p * euclidean-distance less-than 0 variable equal-to 0 variable +
                 (rep_p ^ 2) * dem_p * euclidean-distance less-than 0 variable greater-than 0 variable +
                 (ind_p ^ 2) * rep_p * euclidean-distance equal-to 0 variable less-than 0 variable +
                 (ind_p ^ 2) * dem_p * euclidean-distance equal-to 0 variable greater-than 0 variable +
                 (dem_p ^ 2) * rep_p * euclidean-distance greater-than 0 variable less-than 0 variable +
                 (dem_p ^ 2) * ind_p * euclidean-distance greater-than 0 variable equal-to 0 variable

              )

  set ERindex ERindex / 1.5

  report ERindex

end
@#$#@#$#@
GRAPHICS-WINDOW
174
10
542
379
-1
-1
7.2
1
10
1
1
1
0
1
1
1
0
49
0
49
0
0
1
ticks
30.0

BUTTON
8
251
103
306
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
7
197
62
230
Run
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
561
79
736
112
exposure-to-counter-media
exposure-to-counter-media
0
1
0.6
0.05
1
NIL
HORIZONTAL

INPUTBOX
10
55
82
115
world-size-x
50.0
1
0
Number

INPUTBOX
89
56
159
116
world-size-y
50.0
1
0
Number

BUTTON
71
197
154
230
Run once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
565
277
766
310
propensity-for-homophily
propensity-for-homophily
0
1
0.6
0.1
1
NIL
HORIZONTAL

SLIDER
559
480
736
513
social-influence-parameter
social-influence-parameter
0
1
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
558
444
736
477
media-influence-parameter
media-influence-parameter
0
1
0.5
0.05
1
NIL
HORIZONTAL

CHOOSER
565
227
765
272
discussant-select-base-on-homophily
discussant-select-base-on-homophily
true false
0

CHOOSER
744
458
894
503
opinion-update-model
opinion-update-model
"weighted-mean-average" "WMA-disagree-pro-and-counter-exposure"
1

SLIDER
559
519
736
552
random-decay-parameter
random-decay-parameter
0
1
0.2
0.05
1
NIL
HORIZONTAL

TEXTBOX
590
15
715
38
Media Exposure\n
14
0.0
1

TEXTBOX
597
193
747
211
Political discussion
14
0.0
1

TEXTBOX
664
396
814
414
Opinion dynamics
14
0.0
1

TEXTBOX
76
18
226
36
Setup
14
0.0
1

SLIDER
561
41
735
74
exposure-to-pro-media
exposure-to-pro-media
0
1
0.5
0.1
1
NIL
HORIZONTAL

CHOOSER
567
319
768
364
social-influence-model
social-influence-model
"mean-average-model" "majority-model"
0

CHOOSER
744
507
894
552
model-election-cycle
model-election-cycle
"Yes" "No"
0

CHOOSER
560
566
849
611
model-dropout-based-on-preference-for-politics
model-dropout-based-on-preference-for-politics
"Yes" "No"
0

INPUTBOX
9
119
160
179
custom-random-seed
2631.0
1
0
Number

SWITCH
742
40
931
73
indirect-exposure
indirect-exposure
1
1
-1000

SWITCH
742
79
929
112
media-interest-interaction
media-interest-interaction
1
1
-1000

SWITCH
743
118
908
151
selective-avoidance
selective-avoidance
1
1
-1000

TEXTBOX
948
78
1098
134
\"media-interest-interaction\": those with higher interest would be more likely to be selective in media use?
11
0.0
1

SLIDER
562
121
734
154
information-decay
information-decay
0
1
0.5
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model is a simple automaton that simulates opinion distribution by having each individuals (tuttles) take a "partisan media exposure" of its residing patches, "discuss" politics with maximum of eight surrounding neighbors, then change its own attitudes according to the exposure.

## HOW TO USE IT

Click the SETUP button to create an approximately equal but random distribution of blue, white, and red tuttles (representing individuals).  Click RUN to run the simulation.

## MEDIA EXPOSURE

If the INDIRECT-EXPOSURE switch is on, then discussion partners' partisan media exposure also affects one's opinion.

If the MEDIA-INTEREST-INTERACTION switch is on, then those who have higher political interest (randomly assigned from 0 to 1 at the beginning of the simulation) are more likely to selectively approach to pro-attitudinal media exposure.

If the SELECTIVE-AVOIDANCE switch is on, then those who have higher political interest are more likely to avoid counter-atttitudinal media exposure.

## POLITICAL DISCUSSION

If the DISCUSSANT-SELECT-BASED-ON-HOMOPHILY swith is set to "true", then agents are more likely to create discussion ties with whom they share similar political attitudes (+-1 range of one's own attitudes).

If the SOCIAL-INFLUENCE-MODEL switch is set to "mean-average-model", then the influence from one's discussion network is defined as the mean attitudes of one's connected neighbors. If this is set to "majority-model", then the influence from one's discussion network is defined as the modal value of attitudes of one's connected neighbors.

## OPINION DYNAMICS

If the OPINION-UPDATE-MODEL switch is set to "(simple) weighted-mean-average", then the running tally (valence tally from media exposure and political discussion) does not affected by the proportional difference between agreeable and disagreeable neighbors.

If this is set to "WMA-disagree-pro-and-counter-exposure", then the running tally is additionally weighted by the proportional difference as described in the equation (2) and (3) of the manuscript.

If the MODEL-ELECTION-CYCLE swith is set to "Yes", then (a) exposure-to-counter-media, (b) propensity-for-homophily, (c) media-influence-paramter, (d) social-influence-parameter, and (e) random-decay-parameter are changed through the election cycles as described in the Table 1 of the manuscript.

If the MODEL-DROPOUT-BASED-ON-PREFERENCE-FOR-POLITICS swith is set to "Yes", then those who are politically disinterested (less than 0.3 based on 0 - 1 scale) are randomly drop out of the system, and new agents are comming in to the system.

## CREDITS AND REFERENCES

This model is the extension and adaptation from Wilensky's (1998) voting model described in http://ccl.northwestern.edu/netlogo/models/Voting.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

NA

Please cite the NetLogo software as:

NA

## COPYRIGHT AND LICENSE

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
