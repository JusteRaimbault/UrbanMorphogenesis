;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Urban configuration optimization with CA
;;
;; v2
;; - more flexible implementation ?
;; - hybrid coupling with dynamic ABM
;;
;; --> Precise more motivations and frame of use. Check for justification of use in the litterature; epsitemological fwk is one of the key
;; --> clean code and make ir run faster.
;;
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



extensions[nw profiler]


__includes[
  
  ;;functions for CA-Network
  "setupCA.nls"
  
  ;;Evolutive ABM (economic evaluation)
  "economicABM.nls"
  
  ;;application and exploration of the model
  "application.nls"
  
  ;;Utilities
  "utils/ListUtilities.nls"
  "utils/EuclidianDistancesUtilities.nls"
  "utils/TypesUtilities.nls"
  
]


globals[
  
  
  ;;time profile spent in go at each tick
  current-time-spent
  
  
  ;;bounds as variables for efficiency purposes
  density-max
  density-min
  distance-to-roads-max
  distance-to-roads-min
  distance-to-centre-max
  distance-to-centre-min
  distance-to-activities-max
  distance-to-activities-min
  dmax
  
]



;;points of interest in the city
;;each is supposed to have an activity ?
breed[centres centre]

;;houses
;;really useful, since it is coupled with patch construction ?
breed[houses house]


;;network nodes
breed[intersections intersection]


undirected-link-breed [paths path]

paths-own [path-length]


patches-own[
  
  constructed?
  
  ;;objective value of the patch and associated internediate variables
  value
  pdensity
  pdistance-to-roads
  pdistance-to-centre
  pspeed-from-patch
  pdistance-to-activities
  
  ;;dynamic economic value (will be seen as a "rent")
  rent

]


intersections-own[
  new?
]



centres-own[
  
  ;;integer representing the activity of the center
  activity 
  
  ;; ??
  number
]












to setup-centres
  ifelse not config-from-file? [create-centres centers-number [ set shape "circle" set size 2 set color red setxy random world-width random world-height]]
  [let pos "" ifelse not config-comparison? [set pos user-new-file][set pos "/Users/Juste/Documents/Complex Systems/ProjetUrbanFramework/Models/Activities/atlantis.txt"]
   if pos != false [
     file-open pos
     let c file-read-line ; explanation line
     let n 0
     while [not file-at-end?][
       let s file-read-line ;comment line
       let x read-from-string file-read-line
       let y read-from-string file-read-line
       let a 0
       if not config-comparison? [set a read-from-string file-read-line]
       create-centres 1 [
         set shape "circle" set size 3 setxy x y 
         if not config-comparison? [ set color activity-color a set activity a]
         set number n
         ]
       set n n + 1
     ]
     file-close 
   ]
    ]
  
  ask centres [create-paths-with ((other centres) with-min [distance myself]) with [not path-neighbor? myself][new-path]]
end

to color-patches
  let mi min [value] of patches
  let ma max [value] of patches
  ask patches[set pcolor scale-color yellow  value mi ma]
end

to color-paths
  ask paths [set color red set thickness 0.3]
  ask houses [show-turtle]
end

to new-path
  set color red set thickness 0.3
  set path-length path-length
end

to new-house set shape "house" set size 1 set color blue end

;; go : one iteration
; cell by cell : need to be constructed? evolve network if needed
to go
  update-vars
  ask patches [set value patch-value]
  ask max-n-of built-cells-per-tick (patches with [not constructed?]) [value] [
    set constructed? true
    sprout-houses 1 [new-house]
    if distance-to-roads > distance-road-needed [ ;; construct a new road !! not "static var here" (iterative construction of the network -> random effects)
      let nearest-centre first sort-on [distance myself] centres
      let nearest-centre-distance distance nearest-centre
      ifelse count paths = 0 or distance-to-roads > nearest-centre-distance [
        sprout-intersections 1 [create-path-with nearest-centre [new-path]]
      ]
      [
        
        let mx pxcor
        let my pycor
        let nearest-path first sort-on [distance-to-point mx my] paths
        
        let inter one-of turtles
        ask nearest-path [
          set inter intersection-with mx my
        ]
   
        let e1 one-of intersections let e2 one-of intersections
        ask nearest-path [
          set e1 end1
          set e2 end2
          die
        ]
        ask inter [if inter != e1 [create-path-with e1 [new-path]] if inter != e2 [ create-path-with e2 [new-path]]]

        sprout-intersections 1 [hide-turtle create-path-with inter [new-path]]
      ]
    ]
  ]
  color-patches
  color-paths
  
  
  tick
end





;;descriptives variables : cells variables
  
to-report density
let s 0 let x pxcor let y pycor let i (- neighborhood-radius) let j (- neighborhood-radius)
repeat (2 * neighborhood-radius + 1) [
   set j (- neighborhood-radius)
   repeat (2 * neighborhood-radius + 1)[
     if x + i >= 0 and y + j >= 0 and  x + i <= worldwidth and y + j <= worldheight [
       ask patch (x + i) (y + j) [if constructed? [set s s + 1]]
     ]
     set j j + 1
   ]
   set i i + 1
] 
report s / (((2 * neighborhood-radius) + 1) ^ 2)
end


to-report distance-to-roads
  ;find the nearest road and calculate the distance to it
  ifelse count paths = 0 [
    ;no roads, report nearest center (always one)
    report distance first sort-on [distance myself] centres
  ]
  [
    let mx pxcor
    let my pycor
    let nearest-path first sort-on [distance-to-point mx my] paths 
    let inter one-of turtles let e1 0 let e2 0
    ask nearest-path [
      set inter intersection-with mx my
      set e1 end1 set e2 end2
    ]

    let d distance inter
    if inter != e1 and inter != e2 [ask inter [die]]
    report d
  ]
end






to-report distance-to-centre
  ;;find the nearest road 
  ; add an intersection at the connection point 
  ; take snapshot and make djikstra 
  ; reput the old configuration
  
  ;; Warning : needs a weighted distance !!
  
  ifelse count paths = 0 [
    ;no roads, report nearest center (always one)
    report distance first sort-on [distance myself] centres
  ]
  [
   let mx pxcor
   let my pycor
   let d2 0
   let nearest-path first sort-on [distance-to-point mx my] paths
   
   let inter one-of turtles
    ask nearest-path [
      set inter intersection-with mx my
    ]
   
   let e1 one-of intersections let e2 one-of intersections
   ask nearest-path [
     set d2 distance-to-point mx my
     set e1 end1
     set e2 end2
     die
   ]
   ask inter [if inter != e1 [create-path-with e1 [new-path]] if inter != e2 [ create-path-with e2 [new-path]]]
   
   nw:set-snapshot turtles paths
   ask paths [set path-length path-length] ; to own the variable
   let d3 0
   ask inter [
     set d3 intvalue nw:weighted-distance-to first sort-on [intvalue nw:weighted-distance-to myself "path-length"] centres "path-length"
   ]
   if inter != e1 and inter != e2 [
     ask inter [ask my-paths [die] die]
     ask e1 [create-path-with e2 [new-path]]
   ]
   report d2 + d3
  ]
   
end

;behavior of that function? always connex! so for the center to have distance to itself zero!
to-report intvalue [n]
  ifelse is-number? n [report n][report 0]
end





to-report distance-to-activity [a]
  ;;find the nearest road 
  ; add an intersection at the connection point 
  ; take snapshot and make djikstra 
  ; reput the old configuration
  
  ;; Warning : needs a weighted distance !!
  
  ifelse count paths = 0 [
    ;no roads, report nearest center (always one)
    report distance first sort-on [distance myself] centres
  ]
  [
   let mx pxcor
   let my pycor
   let d2 0
   let nearest-path first sort-on [distance-to-point mx my] paths
   
   let inter one-of turtles
    ask nearest-path [
      set inter intersection-with mx my
    ]
   
   let e1 one-of intersections let e2 one-of intersections
   ask nearest-path [
     set d2 distance-to-point mx my
     set e1 end1
     set e2 end2
     die
   ]
   ask inter [if inter != e1 [create-path-with e1 [new-path]] if inter != e2 [ create-path-with e2 [new-path]]]
   
   nw:set-snapshot turtles paths
   ask paths [set path-length path-length] ; to own the variable
   let d3 0
   ask inter [
     set d3 intvalue nw:weighted-distance-to first sort-on [intvalue nw:weighted-distance-to myself "path-length"] centres with [activity = a] "path-length"
   ]
   if inter != e1 and inter != e2 [
     ask inter [ask my-paths [die] die]
     ask e1 [create-path-with e2 [new-path]]
   ]
   report d2 + d3
  ]
   
end


to test-distance-to-activity
  let a 0
  repeat activities-number [
    ask patch 0 0 [show distance-to-activity a]
    set a a + 1 
  ]
end


;;first test with no preferences of patches for given activities : choose max ??
to-report distance-to-activities
  let a 0
  let s 0
  repeat activities-number [ifelse activities-norma = -1 [set s max (list s distance-to-activity a)] [set s s + ( (distance-to-activity a) ^ activities-norma)] set a a + 1]
  ifelse activities-norma = -1 [report  s] [report s ^ (1 / activities-norma)]
end



;common function to both second variables : return turtle at the junction of the nearest road
; maybe exists a mor efficient method, but now test all roads
to-report intersection-with-nearest-road
  ifelse count paths = 0 [
    ;no roads, report nearest center (always one)
    report first sort-on [distance myself] centres
  ]
  [
    let mx pxcor
    let my pycor
    let nearest-path first sort-on [distance-to-point mx my] paths
    let inter one-of turtles
    ask nearest-path [
      set inter intersection-with mx my
    ]
    report inter
  ]
  
end


;;
;; NEED MORE DEBUGGING -> PB WHEN GETS OUT OF THE WORLD?
;;

to-report intersection-with [x y]
  let x1 0 let y1 0 let x2 0 let y2 0
  ask end1[set x1 xcor
  set y1 ycor]
  ask end2 [set x2 xcor
  set y2 ycor]
  let m1m sqrt (((x1 - x ) ^ 2) + ((y1 - y) ^ 2))
  let m2m sqrt (((x2 - x ) ^ 2) + ((y2 - y) ^ 2))
  let m1m2 sqrt (((x1 - x2 ) ^ 2) + ((y1 - y2) ^ 2))
  if m1m = 0 or m1m2 = 0 [report end1]
  if m2m = 0 [report end2]
  let cost1 (((x - x1)*(x2 - x1)) + ((y - y1)*(y2 - y1)))/(m1m * m1m2)
  let cost2 (((x - x2)*(x1 - x2)) + ((y - y2)*(y1 - y2)))/(m2m * m1m2)
    
  let mq 0 let xx 0 let yy 0 let m1q 0
  
  ifelse cost1 < 0 [
     report end1

  ]
  [
  ifelse cost2 < 0 [
     report end2

  ]
  [set mq m1m * sqrt abs (1 - (cost1 ^ 2))
   set m1q sqrt ((m1m ^ 2) - (mq ^ 2))  
   set xx x1 + m1q * (x2 - x1) / m1m2
   set yy y1 + m1q * (y2 - y1) / m1m2
   
   if count intersections-on patch xx yy = 0 and count centres-on patch xx yy = 0 [
     ask patch xx yy [sprout-intersections 1 [
       setxy xx yy
       if not is-centre? self [hide-turtle]]
     ]
   ]
  report one-of turtles-on patch xx yy
   ]
  ]
  
end


to update-vars
    ask patches [set pdensity density set pdistance-to-roads distance-to-roads set pdistance-to-centre distance-to-centre set pdistance-to-activities distance-to-activities]
    set density-max max [pdensity] of patches
    set density-min min [pdensity] of patches
    set distance-to-roads-max max [pdistance-to-roads] of patches
    set distance-to-roads-min min [pdistance-to-roads] of patches
    set distance-to-centre-max max [pdistance-to-centre] of patches
    set distance-to-centre-min min [pdistance-to-centre] of patches
    set distance-to-activities-max max [pdistance-to-activities] of patches
    set distance-to-activities-min min [pdistance-to-activities] of patches
end


;calculate the patch value thanks to the 3 variables
to-report patch-value
  let c1 density-coefficient let c2 distance-to-roads-coefficient let c3 distance-to-center-coefficient let c4 distance-to-activities-coefficient
  let c c1 + c2 + c3 + c4
  let d1 pdensity let d2 pdistance-to-roads let d3 pdistance-to-centre let d4 pdistance-to-activities

  let dd1 density-max - density-min
  let dd2 distance-to-roads-max - distance-to-roads-min
  let dd3 distance-to-centre-max - distance-to-centre-min
  let dd4 distance-to-activities-max - distance-to-activities-min
  if dd1 = 0 [set c1 0 set dd1 1]
  if dd2 = 0 [set c2 0 set dd2 1]
  if dd3 = 0 [set c3 0 set dd3 1]
  if dd4 = 0 [set c4 0 set dd4 1]
  
  let s (c1 * (density-max - d1) / dd1) + (c2 * (distance-to-roads-max - d2 ) / dd2) + (c3 * (distance-to-centre-max - d3 ) / dd3) + (c4 * (distance-to-activities-max - d4 ) / dd4) 

  report 100 * s / c
  
end



to-report speed-from-patch
  let eucli distance first sort-on [distance myself] centres
  ifelse eucli = 0 [report 1]
  [report pdistance-to-centre / (eucli)]
end



to-report eval-speed
  report ((sum [pspeed-from-patch ^ p-speed] of patches with [constructed?]) /(count patches with [constructed?])) ^ (1 / p-speed )
end


;;density
to-report eval-density
  ;;global density : really pertinent?
;  let xmin min [pxcor] of patches with [constructed?]
;  let xmax max [pxcor] of patches with [constructed?]
;  let ymin min [pycor] of patches with [constructed?]
;  let ymax max [pycor] of patches with [constructed?]
;  let ntot count patches with [pxcor >= xmin and pxcor <= xmax and pycor >= ymin and pycor <= ymax]
;  let nc count patches with [pxcor >= xmin and pxcor <= xmax and pycor >= ymin and pycor <= ymax and constructed?]
;  report nc / ntot

  ;; test local density with certain norm on all cells
    report ((sum [pdensity ^ p-density] of patches with [constructed?]) /(count patches with [constructed?])) ^ (1 / p-density )

end

to-report eval-activities
    report ((sum [pdistance-to-activities ^ p-activities] of patches with [constructed?]) /(count patches with [constructed?])) ^ (1 / p-activities )
end





to-report activity-color [a]
  if a = 0 [report 0]
  if a = 1 [report 25]
  if a = 2 [report 115]
end

@#$#@#$#@
GRAPHICS-WINDOW
529
23
979
539
-1
-1
5.0
1
10
1
1
1
0
0
0
1
0
87
0
96
1
1
1
ticks
30.0

SLIDER
11
14
183
47
psize
psize
1
20
5
1
1
NIL
HORIZONTAL

SLIDER
11
54
183
87
worldwidth
worldwidth
1
200
87
1
1
NIL
HORIZONTAL

SLIDER
12
94
184
127
worldheight
worldheight
1
200
96
1
1
NIL
HORIZONTAL

BUTTON
259
23
325
56
NIL
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

SLIDER
15
163
187
196
centers-number
centers-number
1
20
7
1
1
NIL
HORIZONTAL

SLIDER
21
332
209
365
neighborhood-radius
neighborhood-radius
1
10
6
1
1
NIL
HORIZONTAL

SLIDER
25
544
197
577
density-coefficient
density-coefficient
-1
1
1
0.1
1
NIL
HORIZONTAL

SLIDER
26
584
265
617
distance-to-roads-coefficient
distance-to-roads-coefficient
-1
1
0
0.1
1
NIL
HORIZONTAL

SLIDER
26
625
268
658
distance-to-center-coefficient
distance-to-center-coefficient
-1
1
1
0.1
1
NIL
HORIZONTAL

SLIDER
305
543
498
576
distance-road-needed
distance-road-needed
0
50
3.9
0.1
1
NIL
HORIZONTAL

SLIDER
306
583
485
616
built-cells-per-tick
built-cells-per-tick
0
100
29
1
1
NIL
HORIZONTAL

BUTTON
344
23
407
56
go
profiler:reset\nprofiler:start\ngo\nset current-time-spent profiler:inclusive-time \"go\"
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1229
24
1369
69
patches constructed
count patches with [constructed?]
5
1
11

MONITOR
1231
80
1336
125
road segments
count links
17
1
11

PLOT
1002
403
1162
523
patches values
ticks
values
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [value] of patches"
"pen-1" 1.0 0 -7500403 true "" "plot max [value] of patches"
"pen-2" 1.0 0 -2674135 true "" "plot min [value] of patches"

MONITOR
993
28
1087
73
Mean density
mean [pdensity] of patches
17
1
11

MONITOR
1090
24
1202
69
Mean d to roads
mean [pdistance-to-roads] of patches
17
1
11

MONITOR
998
80
1114
125
Mean d to centre
mean [pdistance-to-centre] of patches
17
1
11

SLIDER
15
209
187
242
activities-number
activities-number
0
10
5
1
1
NIL
HORIZONTAL

SLIDER
26
502
243
535
distance-to-activities-coefficient
distance-to-activities-coefficient
0
1
1
0.1
1
NIL
HORIZONTAL

SLIDER
352
497
524
530
activities-norma
activities-norma
-1
20
3
1
1
NIL
HORIZONTAL

PLOT
1244
602
1404
722
Calc time
ticks
time
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot current-time-spent / 1000"

SWITCH
212
72
376
105
config-from-file?
config-from-file?
1
1
-1000

SLIDER
351
459
523
492
p-speed
p-speed
1
100
3
1
1
NIL
HORIZONTAL

SLIDER
251
118
423
151
param-step
param-step
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
251
160
423
193
ticks-number
ticks-number
1
50
4
1
1
NIL
HORIZONTAL

SWITCH
386
71
509
104
config-comparison?
config-comparison?
1
1
-1000

SLIDER
351
422
523
455
p-density
p-density
1
50
3
1
1
NIL
HORIZONTAL

SLIDER
351
385
523
418
p-activities
p-activities
1
50
3
1
1
NIL
HORIZONTAL

PLOT
1002
142
1162
262
local densities
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [pdensity] of patches"

PLOT
1168
141
1328
261
d to roads
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [pdistance-to-roads] of patches"

@#$#@#$#@
## WHAT IS IT?

Hybrid model (CA coupled with evolving network) for Urban configuration optimisation.

## HOW IT WORKS





## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL



;;add network auto-evolution :: add paths if not direct (eval all x ticks) ; evolve capacity of paths % density and quantity that goes through.

;; multiples centers :: creates independante clusters ; need to force connexion by diversification of activity?

;; irregular grid?? // elevation


## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)




Try of quick implantation of the Raumulus Model
(Diego Moreno, Dominique Badariotti and Arnaud Banos, « Un automate cellulaire pour expérimenter les effets de la proximité dans le processus d’étalement urbain : le modèle Raumulus », Cybergeo : European Journal of Geography)
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
