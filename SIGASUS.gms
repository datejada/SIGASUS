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
g     "Generating unit index                 "
s     "Storage unit index                    "
t     "Time period index                     "
GE(g) "Subset of existing generating units   "
SE(s) "Subset of existing storage units      "
GB(g) "Subset of generating units to be built"
SB(s) "Subset of storage    units to be built"
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
BETAUB_MIN (g,t) "Auxiliary large constants used for linearization      "
BETAUB_MAX (g,t) "Auxiliary large constants used for linearization      "
GAMMALB_MIN(s,t) "Auxiliary large constants used for linearization      "
GAMMALB_MAX(s,t) "Auxiliary large constants used for linearization      "
GAMMAUB_MIN(s,t) "Auxiliary large constants used for linearization      "
GAMMAUB_MAX(s,t) "Auxiliary large constants used for linearization      "
MUUB_MIN   (s,t) "Auxiliary large constants used for linearization      "
MUUB_MAX   (s,t) "Auxiliary large constants used for linearization      "
;
FREE VARIABLES
of           "objective function                                                                    "
p_st   (s,t) "Output of storage unit s and time t. Discharge if positive and charge if negative (MW)"
lambda (  t) "Electricity price at time t (€/MWh)                                                   "
kappa  (s,t) "Dual of definition of storage balance of storage unit s and time t (€/MWh)            "
;
POSITIVE VARIABLES
p_gt       (g,t) "Output of generating unit g in time t (MW)                        "
d_t        (  t) "Satisfied demand in time t (MW)                                   "
e_st       (s,t) "Energy level of storage unit s and time t (MWh)                   "
alphaUB    (  t) "Dual of upper bound on demand                                     "
alphaLB    (  t) "Dual of lower bound on demand                                     "
betaUB     (g,t) "Dual of upper bound on output       of generating unit g in time t"
betaLB     (g,t) "Dual of lower bound on output       of generating unit g in time t"
betaUB_aux (g,t) "Dual of upper bound on output       of generating unit g in time t"
gammaUB    (s,t) "Dual of upper bound on output       of storage    unit s in time t"
gammaLB    (s,t) "Dual of lower bound on output       of storage    unit s in time t"
gammaUB_aux(s,t) "Dual of upper bound on output       of storage    unit s in time t"
gammaLB_aux(s,t) "Dual of lower bound on output       of storage    unit s in time t"
muUB       (s,t) "Dual of upper bound on energy level of storage    unit s in time t"
muLB       (s,t) "Dual of lower bound on energy level of storage    unit s in time t"
muUB_aux   (s,t) "Dual of upper bound on energy level of storage    unit s in time t"
;
BINARY VARIABLES
u_g    (g  ) "Binary variable equal to 1 if generating unit g already exists or is built in the current planning period, and 0 otherwise"
v_s    (s  ) "Binary variable equal to 1 if storage    unit s already exists or is built in the current planning period, and 0 otherwise"
;
* Constraints and Model definition
EQUATIONS
eObjFun "Objective function                            (11a)"
eDemBal "Demand balance                                (4a) "
eUBGPrd "Upper bound generating unit production        (4c) "
eLBSPrd "Lower bound storage    unit production        (4d) "
eUBSPrd "Upper bound storage    unit production        (4d) "
eStoBal "Storage balance constraint                    (4e) "
eUBSLev "Upper bound storage    unit level             (4f) "
edLdPgt "Derivative of Lagrangian with respect to p_gt (4g) "
edLdPst "Derivative of Lagrangian with respect to p_st (4h) "
edLdEs1 "Derivative of Lagrangian with respect to e_st (4i) "
edLdEs2 "Derivative of Lagrangian with respect to e_st (4j) "
eLinEqa "Linearized complementarity equality constraint(6a) "
eLinLBb "Linearized complementarity lower bound        (6b) "
eLinUBb "Linearized complementarity upper bound        (6b) "
eLinLBc "Linearized complementarity lower bound        (6c) "
eLinUBc "Linearized complementarity upper bound        (6c) "
eLinLBd "Linearized complementarity lower bound        (6d) "
eLinUBd "Linearized complementarity upper bound        (6d) "
eLinLBe "Linearized complementarity lower bound        (6e) "
eLinUBe "Linearized complementarity upper bound        (6e) "
eLinLBf "Linearized complementarity lower bound        (6f) "
eLinUBf "Linearized complementarity upper bound        (6f) "
eLinLBg "Linearized complementarity lower bound        (6g) "
eLinUBg "Linearized complementarity upper bound        (6g) "
eLinLBh "Linearized complementarity lower bound        (6h) "
eLinUBh "Linearized complementarity upper bound        (6h) "
eLinLBi "Linearized complementarity lower bound        (6i) "
eLinUBi "Linearized complementarity upper bound        (6i) "
;

eObjFun..
of =e=
    +SUM[(g,t)$GB(g), (betaUB (g,t)-betaUB_aux (g,t))*PG(g)*RHO(g,t)]
    +SUM[(s,t)$SB(s), (gammaLB(s,t)-gammaLB_aux(s,t))*PS(s)         ]
    +SUM[(s,t)$SB(s), (gammaUB(s,t)-gammaUB_aux(s,t))*PS(s)         ]
    +SUM[(s,t)$SB(s), (muUB   (s,t)-muUB_aux   (s,t))*PS(s)*ETA(s  )]
    -SUM[(g  )$GB(g),               u_g        (g  ) *IG(g)         ]
    -SUM[(s  )$SB(s),               v_s        (s  ) *IS(s)         ]    
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

MODEL  mSIGASUS / all / ;
mSIGASUS.holdfixed = 1  ;
mSIGASUS.optfile   = 1  ;

FILE     COPT / cplex.opt  /
PUT      COPT / 'IIS yes'  /
PUTCLOSE COPT ;

* Input data
SETS
g     /g01*g02/
s     /s01*s02/
t     /t01*t24/
GE(g) /g01    /
SE(s) /s01    /
GB(g) /g02    /
SB(s) /s02    /
;
TABLE tGDATA(g,*) 'generator data'
        LinCost AnInvCost   Cap
*       [€/MWh] [M€/year]   [MW]     
    g01   85       4.2      100
    g02    2       7.3      100
;
TABLE tSDATA(s,*) 'storage units data'
         ETA   AnInvCost   Cap  
*        [h]   [M€/year]  [MW]   
    s01  24       4.6      50   
    s02   4       2.9      10   
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
* load data to parameters and unit conversion
CG (g  ) = tGDATA  (g,'LinCost'  ) * 1e-3          ; //M€/GWh 
IG (g  ) = tGDATA  (g,'AnInvCost') * (CARD(t)/8760); //M€ per number of periods
PG (g  ) = tGDATA  (g,'Cap'      ) * 1e-3          ; //GW
ETA(s  ) = tSDATA  (s,'ETA'      )                 ; 
IS (s  ) = tSDATA  (s,'AnInvCost') * (CARD(t)/8760); //M€ per number of periods
PS (s  ) = tSDATA  (s,'Cap'      ) * 1e-3          ; //GW
RHO(g,t) = tRHODATA(t,g          )                 ;
D  (  t) = tDEMDATA(t,'MW'       ) * 1e-3          ; //GW
CS       = 10                                      ; //M€/GW
* Big-M values
BETAUB_MIN (g,t) =  0            ;
BETAUB_MAX (g,t) = +PG(g)        ;
GAMMALB_MIN(s,t) = -PS(s)        ;
GAMMALB_MAX(s,t) =  0            ;
GAMMAUB_MIN(s,t) =  0            ;
GAMMAUB_MAX(s,t) = +PS(s)        ;
MUUB_MIN   (s,t) =  0            ;
MUUB_MAX   (s,t) = +PS(s)*ETA(s) ;

* Constraints as bounds on variables
u_g.fx(g)$[GE(g)] = 1   ; // (11d)
v_s.fx(s)$[SE(s)] = 1   ; // (11e)
d_t.up(t)         = D(t); // (4b)

* solve model
solve mSIGASUS using MIP maximizing of ;

* save all in gdx format
execute_unload 'SIGASUS.gdx'

$onlisting