$Title SIGASUS: Strategic Investment in Generation And Storage UnitS

$onText
For more details please refer to Chapter 3, of the following book:
AAA

Contributed by
Dra. Sonja Wogrin          , IEEE Senior Member, email: sonja.wogrin@gmail.com 
Dr.  Salvador Pineda       , IEEE Senior Member, email: spinedamorente@gmail.com
Dr.  Diego A. Tejada-Arango, IEEE        Member, email: diegoatejada@gmail.com

We do request that publications derived from the use of the developed GAMS code
explicitly acknowledge that fact by citing
AAA
DOI: BBB
$offText

* general options
$EOLCOM //
$OnEmpty OnMulti OffListing
OPTION optcr    =    0 ; // tolerance to solve MIP until IntGap < OptcR
OPTION threads  =   -1 ; // number of cores
;
* Indices, sets, parameters, and variables
SETS
g     "Generating unit index                           "
s     "Storage unit index                              "
t     "Time period index                               "
GE(g) "Subset of existing generating units             "
SE(s) "Subset of existing storage units                "
GO(g) "Subset of generating units owned by the producer"
SO(s) "Subset of storage units owned by the producer   "
;
PARAMETERS
CS               "Load shedding cost (€/MWh)                            "
ETA        (s  ) "Energy capacity of storage unit s (h)                 "
RHO        (g,t) "Capacity factor of generating unit g and time t (p.u.)"
CG         (g  ) "Linear cost parameter of generating unit g (€/MWh)    "
D          (  t) "Demand level at time period t (MW)                    "
IG         (g  ) "Annualized investment cost of generating unit g (€)   "
IS         (s  ) "Annualized investment cost of storage unit s (€)      "
PG         (g  ) "Capacity of genering unit g (MW).                     "
PS         (s  ) "Capacity of storage unit s (MW).                      "
BETAUB_MIN (g,t) ""
BETAUB_MAX (g,t) ""
GAMMALB_MIN(s,t) ""
GAMMALB_MAX(s,t) ""
GAMMAUB_MIN(s,t) ""
GAMMAUB_MAX(s,t) ""
MUUB_MIN   (s,t) ""
MUUB_MAX   (s,t) ""
;
FREE VARIABLES
of           "objective function                                                                                                         "
lambda (  t) "Electricity price at time t (€/MWh)                                                                                        "
kappa  (s,t) ""
;
POSITIVE VARIABLES
p_gt       (g,t) "Output of generating unit g in time t (MW)                                                                                 "
p_st       (s,t) "Output of storage unit s and time t. Discharge if positive and charge if negative (MW)                                     "
d_t        (  t) "Satisfied demand in time t (MW)                                                                                            "
e_st       (s,t) "Energy level of storage unit s and time t (MWh)                                                                            "
alphaUB    (  t) ""
alphaLB    (  t) ""
betaUB     (g,t) ""
betaLB     (g,t) ""
betaUB_aux (g,t) ""
gammaUB    (s,t) ""
gammaLB    (s,t) ""
gammaUB_aux(s,t) ""
gammaLB_aux(s,t) ""
muUB       (s,t) ""
muLB       (s,t) ""
muUB_aux   (s,t) ""
;
BINARY VARIABLES
u_g    (g  ) "Binary variable equal to 1 if generating unit g already exists or is built in the current planning period, and 0 otherwise."
v_s    (s  ) "Binary variable equal to 1 if storage unit s already exists or is built in the current planning period, and 0 otherwise.   "
;
* Constraints and Model definition
EQUATIONS
eObjFun "Objective function                            (20a)"
eDemBal "Demand balance                                (13a)"
eUBGPrd "Upper bound generating unit production        (13c)"
eLBSPrd "Lower bound storage    unit production        (13d)"
eUBSPrd "Upper bound storage    unit production        (13d)"
eStoBal "Storage balance constraint                    (13e)"
eUBSLev "Upper bound storage    unit level             (13f)"
edLdPgt "Derivative of Lagrangian with respect to p_gt (13g)"
edLdPst "Derivative of Lagrangian with respect to p_st (13h)"
edLdEs1 "Derivative of Lagrangian with respect to e_st (13i)"
edLdEs2 "Derivative of Lagrangian with respect to e_st (13j)"
eLinEqa "Linearized complementarity equality constraint(15a)"
eLinLBb "Linearized complementarity lower bound        (15b)"
eLinUBb "Linearized complementarity upper bound        (15b)"
eLinLBc "Linearized complementarity lower bound        (15c)"
eLinUBc "Linearized complementarity upper bound        (15c)"
eLinLBd "Linearized complementarity lower bound        (15d)"
eLinUBd "Linearized complementarity upper bound        (15d)"
eLinLBe "Linearized complementarity lower bound        (15e)"
eLinUBe "Linearized complementarity upper bound        (15e)"
eLinLBf "Linearized complementarity lower bound        (15f)"
eLinUBf "Linearized complementarity upper bound        (15f)"
eLinLBg "Linearized complementarity lower bound        (15g)"
eLinUBg "Linearized complementarity upper bound        (15g)"
eLinLBh "Linearized complementarity lower bound        (15h)"
eLinUBh "Linearized complementarity upper bound        (15h)"
eLinLBi "Linearized complementarity lower bound        (15i)"
eLinUBi "Linearized complementarity upper bound        (15i)"
;

eObjFun..
of =e=
    +SUM[(g,t)$GO(g), (betaUB (g,t)-betaUB_aux (g,t))*PG(g)*RHO(g,t)]
    +SUM[(s,t)$SO(s), (gammaLB(s,t)-gammaLB_aux(s,t))*PS(s)         ]
    +SUM[(s,t)$SO(s), (gammaUB(s,t)-gammaUB_aux(s,t))*PS(s)         ]
    +SUM[(s,t)$SO(s), (muUB   (s,t)-muUB_aux   (s,t))*PS(s)*ETA(s  )]
    +SUM[(g  )$GO(g),               u_g        (g  ) *IG(g)         ]
    +SUM[(s  )$SO(s),               v_s        (s  ) *IS(s)         ]    
;
eDemBal(  t)..SUM[g,p_gt(g,t)]+SUM[s,p_st(s,t)] =e= d_t(t)                ;
eUBGPrd(g,t)..      p_gt(g,t)                   =l= u_g(g)*PG(g)*RHO(g,t) ;
eLBSPrd(s,t)..                       p_st(s,t)  =g=-v_s(s)*PS(s)          ;
eUBSPrd(s,t)..                       p_st(s,t)  =l= v_s(s)*PS(s)          ;
eUBSLev(s,t)..                       e_st(s,t)  =l= v_s(s)*PS(s)*ETA(s)   ;
eStoBal(s,t)..      e_st(s,t) =e=    e_st(s,t-1) -  p_st(s,t)             ;

edLdPgt(g,t)..CG(g)-lambda(t)-betaLB (g,t)+betaUB (g,t)           =e= 0   ;
edLdPst(s,t)..     -lambda(t)-gammaLB(s,t)+gammaUB(s,t)+kappa(s,t)=e= 0   ;

edLdEs1(s,t)$[ORD(t)<CARD(t)].. kappa(s,t)-kappa(s,t+1)-muLB(s,t)+muUB(s,t) =e= 0 ;
edLdEs2(s,t)$[ORD(t)=CARD(t)].. kappa(s,t)             -muLB(s,t)+muUB(s,t) =e= 0 ;

eLinEqa..
    +SUM[(g,t), CG(g)*                       p_gt       (g,t) ]
    +SUM[(  t), CS   *         (D(t)        -d_t        (  t))]
   =e= 
    +SUM[(  t), D (t)*          alphaUB(  t)                  ]
    +SUM[(g,t), PG(g)*RHO(g,t)*(betaUB (g,t)-betaUB_aux (g,t))]
    +SUM[(s,t), PS(s)*         (gammaLB(s,t)-gammaLB_aux(s,t))]
    +SUM[(s,t), PS(s)*         (gammaUB(s,t)-gammaUB_aux(s,t))]
    +SUM[(s,t), PS(s)*ETA(s  )*(muUB   (s,t)-muUB_aux   (s,t))]
;
eLinLBb(g,t)..betaUB (g,t)-betaUB_aux (g,t) =g= BETAUB_MIN (g,t)*   u_g(g)  ;
eLinUBb(g,t)..betaUB (g,t)-betaUB_aux (g,t) =l= BETAUB_MAX (g,t)*   u_g(g)  ;
eLinLBc(g,t)..             betaUB_aux (g,t) =g= BETAUB_MIN (g,t)*(1-u_g(g)) ;
eLinUBc(g,t)..             betaUB_aux (g,t) =l= BETAUB_MAX (g,t)*(1-u_g(g)) ;
eLinLBd(s,t)..gammaLB(s,t)-gammaLB_aux(s,t) =g= GAMMALB_MIN(s,t)*   v_s(s)  ;
eLinUBd(s,t)..gammaLB(s,t)-gammaLB_aux(s,t) =l= GAMMALB_MAX(s,t)*   v_s(s)  ;
eLinLBe(s,t)..             gammaLB_aux(s,t) =g= GAMMALB_MIN(s,t)*(1-v_s(s)) ;
eLinUBe(s,t)..             gammaLB_aux(s,t) =l= GAMMALB_MAX(s,t)*(1-v_s(s)) ;
eLinLBf(s,t)..gammaUB(s,t)-gammaUB_aux(s,t) =g= GAMMAUB_MIN(s,t)*   v_s(s)  ; 
eLinUBf(s,t)..gammaUB(s,t)-gammaUB_aux(s,t) =l= GAMMAUB_MAX(s,t)*   v_s(s)  ; 
eLinLBg(s,t)..             gammaUB_aux(s,t) =g= GAMMAUB_MIN(s,t)*(1-v_s(s)) ; 
eLinUBg(s,t)..             gammaUB_aux(s,t) =l= GAMMAUB_MAX(s,t)*(1-v_s(s)) ; 
eLinLBh(s,t)..muUB   (s,t)-muUB_aux   (s,t) =g= MUUB_MIN   (s,t)*   v_s(s)  ;
eLinUBh(s,t)..muUB   (s,t)-muUB_aux   (s,t) =l= MUUB_MAX   (s,t)*   v_s(s)  ;
eLinLBi(s,t)..             muUB_aux   (s,t) =g= MUUB_MIN   (s,t)*(1-v_s(s)) ;
eLinUBi(s,t)..             muUB_aux   (s,t) =l= MUUB_MAX   (s,t)*(1-v_s(s)) ;

MODEL  mSIGASUS / all /
;
* Input data
SETS
g     /g01*g02/
s     /s01*s02/
t     /t01*t24/
GE(g) /g01    /
SE(s) /s01    /
GO(g) /g02    /
SO(s) /s01    /
;
TABLE tGDATA(g,*) 'generator data'
        LinCost AnInvCost   Cap
*       [€/MWh]    [€]      [MW]     
    g01   50       2e2      100
    g02    5       5e2      100
;
TABLE tSDATA(s,*) 'storage units data'
        Effic   AnInvCost   Cap 
*       [p.u.]     [€]      [MW] 
    s01  0.85      4e2       50 
    s02  0.98      8e2       50 
;
TABLE tRHODATA(t,g) 'capacity factor [p.u.]'
	g01	g02
t01	1.00	1.00
t02	1.00	0.99
t03	1.00	0.95
t04	1.00	0.91
t05	1.00	0.86
t06	1.00	0.71
t07	1.00	0.56
t08	1.00	0.41
t09	1.00	0.30
t10	1.00	0.30
t11	1.00	0.39
t12	1.00	0.42
t13	1.00	0.44
t14	1.00	0.45
t15	1.00	0.41
t16	1.00	0.41
t17	1.00	0.41
t18	1.00	0.37
t19	1.00	0.37
t20	1.00	0.45
t21	1.00	0.58
t22	1.00	0.62
t23	1.00	0.58
t24	1.00	0.51
;
TABLE tDEMDATA(t,*) 'demand [MW]'
	MW
t01	100
t02	93
t03	78
t04	43
t05	48
t06	72
t07	100
t08	115
t09	123
t10	134
t11	136
t12	127
t13	107
t14	92
t15	87
t16	102
t17	122
t18	145
t19	155
t20	152
t21	137
t22	117
t23	107
t24	100
;
* load data to parameters
CG (g  ) = tGDATA  (g,'LinCost'  ) ; 
IG (g  ) = tGDATA  (g,'AnInvCost') ; 
PG (g  ) = tGDATA  (g,'Cap'      ) ; 
ETA(s  ) = tSDATA  (s,'Effic'    ) ; 
IS (s  ) = tSDATA  (s,'AnInvCost') ; 
PS (s  ) = tSDATA  (s,'Cap'      ) ; 
RHO(g,t) = tRHODATA(t,g          ) ;
D  (  t) = tDEMDATA(t,'MW'       ) ;
CS       = 10000                   ;
* Big-M values
BETAUB_MIN (g,t) = 1000 ;
BETAUB_MAX (g,t) = 1000 ;
GAMMALB_MIN(s,t) = 1000 ;
GAMMALB_MAX(s,t) = 1000 ;
GAMMAUB_MIN(s,t) = 1000 ;
GAMMAUB_MAX(s,t) = 1000 ;
MUUB_MIN   (s,t) = 1000 ;
MUUB_MAX   (s,t) = 1000 ;

* Constraints as bounds on variables
u_g.fx(g)$[GE(g)] = 1   ; // (20b)
v_s.fx(s)$[SE(s)] = 1   ; // (20c)
d_t.up(t)         = D(t); // (13b)

* solve model
solve mSIGASUS using MIP maximizing of ;

* save all in gdx format
execute_unload 'SIGASUS.gdx'

$onlisting