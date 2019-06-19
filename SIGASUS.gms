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
*OPTION limrow   =    0 ; // maximum number of equations in the .lst file
*OPTION limcol   =     0 ; // maximum number of variables in the .lst file
$OnEmpty OnMulti OffListing
OPTION optcr    =    0 ; // tolerance to solve MIP until IntGap < OptcR
OPTION threads  =   -1 ; // number of cores
;
* Scalars, indices, sets, parameters, and variables
SCALAR
MIMOD "merchant investor model (1), (0) cost minimization model" / 1 /
;
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
DMAX             "Maximum demand level (MW)                             "            
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
betaUB_aux (g,t) "Dual of upper bound on output       of generating unit g in time t"
gammaUB_aux(s,t) "Dual of upper bound on output       of storage    unit s in time t"
gammaLB_aux(s,t) "Dual of lower bound on output       of storage    unit s in time t"
muUB_aux   (s,t) "Dual of upper bound on energy level of storage    unit s in time t"
;
POSITIVE VARIABLES
p_gt       (g,t) "Output of generating unit g in time t (MW)                        "
d_t        (  t) "Satisfied demand in time t (MW)                                   "
e_st       (s,t) "Energy level of storage unit s and time t (MWh)                   "
alphaUB    (  t) "Dual of upper bound on demand                                     "
alphaLB    (  t) "Dual of lower bound on demand                                     "
betaUB     (g,t) "Dual of upper bound on output       of generating unit g in time t"
betaLB     (g,t) "Dual of lower bound on output       of generating unit g in time t"
gammaUB    (s,t) "Dual of upper bound on output       of storage    unit s in time t"
gammaLB    (s,t) "Dual of lower bound on output       of storage    unit s in time t"
muUB       (s,t) "Dual of upper bound on energy level of storage    unit s in time t"
muLB       (s,t) "Dual of lower bound on energy level of storage    unit s in time t"
;
BINARY VARIABLES
u_g    (g  ) "Binary variable equal to 1 if generating unit g already exists or is built in the current planning period, and 0 otherwise"
v_s    (s  ) "Binary variable equal to 1 if storage    unit s already exists or is built in the current planning period, and 0 otherwise"
;
* Constraints and Model definition
EQUATIONS
eOFCInv "Objective function centralized investment     (2a) "
eOFMInv "Objective function of merchant investors      (11a)"
eDemBal "Demand balance                                (4a) "
eUBDem  "Upper bound on satisfied demand               (4b) "
eUBGPrd "Upper bound generating unit production        (4c) "
eLBSPrd "Lower bound storage    unit production        (4d) "
eUBSPrd "Upper bound storage    unit production        (4d) "
eStoBal "Storage balance constraint                    (4e) "
eUBSLev "Upper bound storage    unit level             (4f) "
edLdPdt "Derivative of Lagrangian with respect to d_t  (4g) "
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

eOFCInv $[MIMOD=0]..
of =e=
    +SUM[(g,t)$GB(g), CG(g)*      p_gt(g,t) ]
    +SUM[(g  )$GB(g), IG(g)*      u_g (g  ) ]
    +SUM[(s  )$SB(s), IS(s)*      v_s (s  ) ]    
    +SUM[(  t)      , CS   *(D(t)-d_t (  t))]
;
eOFMInv $[MIMOD=1]..
of =e=
    +SUM[(g,t)$GB(g), (betaUB (g,t)-betaUB_aux (g,t))*PG(g)*RHO(g,t)]
    +SUM[(s,t)$SB(s), (gammaLB(s,t)-gammaLB_aux(s,t))*PS(s)         ]
    +SUM[(s,t)$SB(s), (gammaUB(s,t)-gammaUB_aux(s,t))*PS(s)         ]
    +SUM[(s,t)$SB(s), (muUB   (s,t)-muUB_aux   (s,t))*PS(s)*ETA(s  )]
    -SUM[(g  )$GB(g),               u_g        (g  ) *IG(g)         ]
    -SUM[(s  )$SB(s),               v_s        (s  ) *IS(s)         ]    
;
eDemBal(  t)..d_t(t)- SUM[g,p_gt(g,t)]-SUM[s,p_st(s,t)] =e= 0             ;
eUBDem(   t)..d_t(t)                            =l= D(t)                  ;
eUBGPrd(g,t)..      p_gt(g,t)                   =l= u_g(g)*PG(g)*RHO(g,t) ;
eLBSPrd(s,t)..                       p_st(s,t)  =g=-v_s(s)*PS(s)          ;
eUBSPrd(s,t)..                       p_st(s,t)  =l= v_s(s)*PS(s)          ;
eUBSLev(s,t)..                       e_st(s,t)  =l= v_s(s)*PS(s)*ETA(s)   ;
eStoBal(s,t)..      e_st(s,t) =e=    e_st(s,t-1) -  p_st(s,t)             ;

edLdPdt(t)$[MIMOD=1]  ..-CS+lambda(t)-alphaLB(t)+alphaUB(t)                 =e= 0   ; 
edLdPgt(g,t)$[MIMOD=1]..CG(g)-lambda(t)-betaLB (g,t)+betaUB (g,t)           =e= 0   ;
edLdPst(s,t)$[MIMOD=1]..     -lambda(t)-gammaLB(s,t)+gammaUB(s,t)+kappa(s,t)=e= 0   ;

edLdEs1(s,t)$[MIMOD=1 AND ORD(t)<CARD(t)].. kappa(s,t)-kappa(s,t+1)-muLB(s,t)+muUB(s,t) =e= 0 ;
edLdEs2(s,t)$[MIMOD=1 AND ORD(t)=CARD(t)].. kappa(s,t)             -muLB(s,t)+muUB(s,t) =e= 0 ;

eLinEqa$[MIMOD=1]..
    +SUM[(g,t), CG(g)*                       p_gt       (g,t) ]
    +SUM[(  t), CS   *         (D(t)        -d_t        (  t))]
   =e=    
    -SUM[(  t), D (t)*         (alphaUB(  t)-CS)              ]
    -SUM[(g,t), PG(g)*RHO(g,t)*(betaUB (g,t)-betaUB_aux (g,t))]
    -SUM[(s,t), PS(s)*         (gammaLB(s,t)-gammaLB_aux(s,t))]
    -SUM[(s,t), PS(s)*         (gammaUB(s,t)-gammaUB_aux(s,t))]
    -SUM[(s,t), PS(s)*ETA(s  )*(muUB   (s,t)-muUB_aux   (s,t))]
;
eLinLBb(g,t)$[MIMOD=1]..betaUB (g,t)-betaUB_aux (g,t) =g= BETAUB_MIN (g,t)*   u_g(g)  ;
eLinUBb(g,t)$[MIMOD=1]..betaUB (g,t)-betaUB_aux (g,t) =l= BETAUB_MAX (g,t)*   u_g(g)  ;
eLinLBc(g,t)$[MIMOD=1]..             betaUB_aux (g,t) =g= BETAUB_MIN (g,t)*(1-u_g(g)) ;
eLinUBc(g,t)$[MIMOD=1]..             betaUB_aux (g,t) =l= BETAUB_MAX (g,t)*(1-u_g(g)) ;
eLinLBd(s,t)$[MIMOD=1]..gammaLB(s,t)-gammaLB_aux(s,t) =g= GAMMALB_MIN(s,t)*   v_s(s)  ;
eLinUBd(s,t)$[MIMOD=1]..gammaLB(s,t)-gammaLB_aux(s,t) =l= GAMMALB_MAX(s,t)*   v_s(s)  ;
eLinLBe(s,t)$[MIMOD=1]..             gammaLB_aux(s,t) =g= GAMMALB_MIN(s,t)*(1-v_s(s)) ;
eLinUBe(s,t)$[MIMOD=1]..             gammaLB_aux(s,t) =l= GAMMALB_MAX(s,t)*(1-v_s(s)) ;
eLinLBf(s,t)$[MIMOD=1]..gammaUB(s,t)-gammaUB_aux(s,t) =g= GAMMAUB_MIN(s,t)*   v_s(s)  ; 
eLinUBf(s,t)$[MIMOD=1]..gammaUB(s,t)-gammaUB_aux(s,t) =l= GAMMAUB_MAX(s,t)*   v_s(s)  ; 
eLinLBg(s,t)$[MIMOD=1]..             gammaUB_aux(s,t) =g= GAMMAUB_MIN(s,t)*(1-v_s(s)) ; 
eLinUBg(s,t)$[MIMOD=1]..             gammaUB_aux(s,t) =l= GAMMAUB_MAX(s,t)*(1-v_s(s)) ; 
eLinLBh(s,t)$[MIMOD=1]..muUB   (s,t)-muUB_aux   (s,t) =g= MUUB_MIN   (s,t)*   v_s(s)  ;
eLinUBh(s,t)$[MIMOD=1]..muUB   (s,t)-muUB_aux   (s,t) =l= MUUB_MAX   (s,t)*   v_s(s)  ;
eLinLBi(s,t)$[MIMOD=1]..             muUB_aux   (s,t) =g= MUUB_MIN   (s,t)*(1-v_s(s)) ;
eLinUBi(s,t)$[MIMOD=1]..             muUB_aux   (s,t) =l= MUUB_MAX   (s,t)*(1-v_s(s)) ;

MODEL  mSIGASUS / all / ;
mSIGASUS.holdfixed = 1  ;
mSIGASUS.optfile   = 1  ;

FILE     COPT / cplex.opt  /
PUT      COPT / 'IIS yes'  /
PUT      COPT / 'numericalemphasis 1'  /
PUTCLOSE COPT ;

* Input data
SETS
g     /th1,th2,th3,th4,th5,th6,th7,th8,th9,th10,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10/
s     /ph1,ph2,ph3,ph4,ph5,ph6,ph7,ph8,ph9,ph10,be1,be2,be3,be4,be5,be6,be7,be8,be9,be10/
t     /t01*t24/
GB(g) /th1,th2,th3,th4,th5,th6,th7,th8,th9,th10,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10/
SB(s) /ph1,ph2,ph3,ph4,ph5,ph6,ph7,ph8,ph9,ph10,be1,be2,be3,be4,be5,be6,be7,be8,be9,be10/
;
* greenfield approach
GE(g)=NO ;
SE(s)=NO ;

TABLE tGDATA(g,*) 'generator data'
        LinCost   InvCost    Cap
*       [€/MWh] [€/kW/year] [MW]     
   th1    60       42       100
   th2    60       42       100
   th3    60       42       100
   th4    60       42       100
   th5    60       42       100
   th6    60       42       100
   th7    60       42       100
   th8    60       42       100
   th9    60       42       100
   th10   60       42       100
   w1      2       73       100
   w2      2       73       100
   w3      2       73       100
   w4      2       73       100
   w5      2       73       100
   w6      2       73       100
   w7      2       73       100
   w8      2       73       100
   w9      2       73       100
   w10     2       73       100
   s1      0       85       100   
   s2      0       85       100   
   s3      0       85       100   
   s4      0       85       100   
   s5      0       85       100   
   s6      0       85       100   
   s7      0       85       100   
   s8      0       85       100   
   s9      0       85       100   
   s10     0       85       100   
;
TABLE tSDATA(s,*) 'storage units data'
         ETA     InvCost     Cap  
*        [h]   [€/kW/year]  [MW]   
   ph1   24        62       100   
   ph2   24        62       100   
   ph3   24        62       100   
   ph4   24        62       100   
   ph5   24        62       100   
   ph6   24        62       100   
   ph7   24        62       100   
   ph8   24        62       100   
   ph9   24        62       100   
   ph10  24        62       100   
   be1    4         4       100   
   be2    4         4       100   
   be3    4         4       100   
   be4    4         4       100   
   be5    4         4       100   
   be6    4         4       100   
   be7    4         4       100   
   be8    4         4       100   
   be9    4         4       100   
   be10   4         4       100   
;
TABLE tRHODATA(t,g) 'capacity factor [p.u.]'
	th1	th2	th3	th4	th5	th6	th7	th8	th9	th10	w1	w2	w3	w4	w5	w6	w7	w8	w9	w10	s1	s2	s3	s4	s5	s6	s7	s8	s9	s10
t01	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0
t02	1	1	1	1	1	1	1	1	1	1	0.99	0.99	0.99	0.99	0.99	0.99	0.99	0.99	0.99	0.99	0	0	0	0	0	0	0	0	0	0
t03	1	1	1	1	1	1	1	1	1	1	0.95	0.95	0.95	0.95	0.95	0.95	0.95	0.95	0.95	0.95	0	0	0	0	0	0	0	0	0	0
t04	1	1	1	1	1	1	1	1	1	1	0.91	0.91	0.91	0.91	0.91	0.91	0.91	0.91	0.91	0.91	0	0	0	0	0	0	0	0	0	0
t05	1	1	1	1	1	1	1	1	1	1	0.86	0.86	0.86	0.86	0.86	0.86	0.86	0.86	0.86	0.86	0.03	0.03	0.03	0.03	0.03	0.03	0.03	0.03	0.03	0.03
t06	1	1	1	1	1	1	1	1	1	1	0.71	0.71	0.71	0.71	0.71	0.71	0.71	0.71	0.71	0.71	0.35	0.35	0.35	0.35	0.35	0.35	0.35	0.35	0.35	0.35
t07	1	1	1	1	1	1	1	1	1	1	0.56	0.56	0.56	0.56	0.56	0.56	0.56	0.56	0.56	0.56	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0.51
t08	1	1	1	1	1	1	1	1	1	1	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.59	0.59	0.59	0.59	0.59	0.59	0.59	0.59	0.59	0.59
t09	1	1	1	1	1	1	1	1	1	1	0.3	0.3	0.3	0.3	0.3	0.3	0.3	0.3	0.3	0.3	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0.58
t10	1	1	1	1	1	1	1	1	1	1	0.3	0.3	0.3	0.3	0.3	0.3	0.3	0.3	0.3	0.3	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0.51
t11	1	1	1	1	1	1	1	1	1	1	0.39	0.39	0.39	0.39	0.39	0.39	0.39	0.39	0.39	0.39	0.23	0.23	0.23	0.23	0.23	0.23	0.23	0.23	0.23	0.23
t12	1	1	1	1	1	1	1	1	1	1	0.42	0.42	0.42	0.42	0.42	0.42	0.42	0.42	0.42	0.42	0.54	0.54	0.54	0.54	0.54	0.54	0.54	0.54	0.54	0.54
t13	1	1	1	1	1	1	1	1	1	1	0.44	0.44	0.44	0.44	0.44	0.44	0.44	0.44	0.44	0.44	0.28	0.28	0.28	0.28	0.28	0.28	0.28	0.28	0.28	0.28
t14	1	1	1	1	1	1	1	1	1	1	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.34	0.34	0.34	0.34	0.34	0.34	0.34	0.34	0.34	0.34
t15	1	1	1	1	1	1	1	1	1	1	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.45
t16	1	1	1	1	1	1	1	1	1	1	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.69	0.69	0.69	0.69	0.69	0.69	0.69	0.69	0.69	0.69
t17	1	1	1	1	1	1	1	1	1	1	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.41	0.7	0.7	0.7	0.7	0.7	0.7	0.7	0.7	0.7	0.7
t18	1	1	1	1	1	1	1	1	1	1	0.37	0.37	0.37	0.37	0.37	0.37	0.37	0.37	0.37	0.37	0.61	0.61	0.61	0.61	0.61	0.61	0.61	0.61	0.61	0.61
t19	1	1	1	1	1	1	1	1	1	1	0.37	0.37	0.37	0.37	0.37	0.37	0.37	0.37	0.37	0.37	0.32	0.32	0.32	0.32	0.32	0.32	0.32	0.32	0.32	0.32
t20	1	1	1	1	1	1	1	1	1	1	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.45	0.02	0.02	0.02	0.02	0.02	0.02	0.02	0.02	0.02	0.02
t21	1	1	1	1	1	1	1	1	1	1	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0	0	0	0	0	0	0	0	0	0
t22	1	1	1	1	1	1	1	1	1	1	0.62	0.62	0.62	0.62	0.62	0.62	0.62	0.62	0.62	0.62	0	0	0	0	0	0	0	0	0	0
t23	1	1	1	1	1	1	1	1	1	1	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0.58	0	0	0	0	0	0	0	0	0	0
t24	1	1	1	1	1	1	1	1	1	1	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0.51	0	0	0	0	0	0	0	0	0	0
;

TABLE tDEMDATA(t,*) 'demand profile [p.u.]'
	Profile
t01	0.65
t02	0.60
t03	0.50
t04	0.28
t05	0.31
t06	0.46
t07	0.65
t08	0.74
t09	0.79
t10	0.86
t11	0.88
t12	0.82
t13	0.69
t14	0.59
t15	0.56
t16	0.66
t17	0.79
t18	0.94
t19	1.00
t20	0.98
t21	0.88
t22	0.75
t23	0.69
t24	0.65
;
* load data to parameters and unit conversion
CS       = 5.0                                         ; //M€/GW
DMAX     = 1.5                                         ; //GW
CG (g  ) = tGDATA  (g,'LinCost') * 1e-3                ; //M€/GWh 
PG (g  ) = tGDATA  (g,'Cap'    ) * 1e-3                ; //GW
IG (g  ) = tGDATA  (g,'InvCost') * PG(g)*(CARD(t)/8760); //M€ per number of periods
ETA(s  ) = tSDATA  (s,'ETA'    )                       ; 
PS (s  ) = tSDATA  (s,'Cap'    ) * 1e-3                ; //GW
IS (s  ) = tSDATA  (s,'InvCost') * PS(s)*(CARD(t)/8760); //M€ per number of periods
RHO(g,t) = tRHODATA(t,g        )                       ;
D  (  t) = tDEMDATA(t,'Profile') * DMAX                ; //GW
* Big-M values
BETAUB_MIN (g,t) =   0 ;
BETAUB_MAX (g,t) = 1e2 ;
GAMMALB_MIN(s,t) =   0 ;
GAMMALB_MAX(s,t) = 1e2 ;
GAMMAUB_MIN(s,t) =   0 ;
GAMMAUB_MAX(s,t) = 1e2 ;
MUUB_MIN   (s,t) =   0 ;
MUUB_MAX   (s,t) = 1e2 ;

* Constraints as bounds on variables
u_g.fx(g)$[GE(g)] = 1   ; // (11d)
v_s.fx(s)$[SE(s)] = 1   ; // (11e)

* solve model merchant investors problem
MIMOD=1;
solve mSIGASUS using MIP maximizing of ;

File results / results.txt /;
put results;
put "Bilevel results"/;
put "Model status",  mSIGASUS.modelstat /;
put "Solver status", mSIGASUS.solvestat /;
put "Objective", of.l /;
loop((g),
  put g.tl , u_g.l(g) /
);
loop((s),
  put s.tl , v_s.l(s) /
);

* solve model centralized investment problem
MIMOD=0;
solve mSIGASUS using MIP minimizing of ;

put "Centralized results"/;
put "Model status",  mSIGASUS.modelstat /;
put "Solver status", mSIGASUS.solvestat /;
put "Objective", of.l /;
loop((g),
  put g.tl , u_g.l(g) /
);
loop((s),
  put s.tl , v_s.l(s) /
);
putclose;

* save all in gdx format
execute_unload 'SIGASUS.gdx'

$onlisting
