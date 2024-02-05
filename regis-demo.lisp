;; -*- Lisp -*-

(in-package :regis)

(start-server)

(with-regis () "")

;; traffic symbols
(with-regis () "
P[240,150] V[ + 140][, + 60][-140][,-60]
P[305,320] V[ + 32][+ 32, + 32][, + 32][-32, + 32][-32][-32,-32][,-32][ + 32,-32]
P[285,359] T(s2)(w(c))(h2) 'STOP'
P[180,250] V[][ + 60][, + 140][-60][,- 140]
P[365,280] V[ + 60,-60][ + 60, + 60][-60, + 60][-60,-60]
P[442,257] V[, + 20][ + 10,-10][-10,-10]
P[207,280] W(I(R)S1) C[+13] W(S0)
P[207,320] W(I(Y)S1) C[+13] W(S0)
P[207,360] W(I(G)S1) C[+13] W(S0)
P[406,310] V[,-30]C(CA-90) +[+ 10]V[+18]
P[406,310] V[-8][,-30]C(CA-90) +[+ 18]V[ + 18]
P[246,165] T(s2)(h3) 'MAIN ST'
")

;; text attributes
(with-regis () "
w(m20)
s(e)
p[165,135]
t(a0 s10)'Big'
p[165,290]
w(i(r) s1)
t(s3 h16)'Tall'0
w(i(b))
t(s3 i-27)'Italic'666666444444444444444444444
w(i(g) s0)
t(e s6 d0 h2)'Short'
w(i(r) s1)
")

(with-regis () "
w(m20)
s(e)
p[0,0]
w(i(r))
t(a0 s10)'C'
p[0,130]
w(i(g))
t'M'
p[0,260]
w(i(b))
t'H'
w(i(w) s0)
t(e s6 d0 h3)
p[90,96]
t'omputer'
p[90,226]
t'useum'
p[90,356]
t'amburg'

p[1,470]v[16,470]v[13,466]v[11,465]v[11,465]v[12,465]v[8,460]v[10,460]v[10,459]v[9,459]v[9,458]v[9,458]v[10,458]v[10,458]v[10,459]v[10,459]v[10,460]v[12,460]v[12,441]v[13,442]v[13,442]v[14,443]v[16,443]v[15,444]v[16,445]v[14,445]v[12,444]v[12,455]v[12,460]v[30,462]v[30,458]v[27,458]v[31,455]v[37,455]v[38,452]v[36,452]v[36,451]v[38,450]v[49,449]v[48,446]v[47,446]v[52,445]v[52,444]v[52,444]v[53,445]v[54,445]v[54,443]v[54,443]v[53,443]v[53,442]v[55,443]v[56,443]v[56,443]v[55,443]v[55,443]v[55,445]v[59,445]v[60,442]v[57,442]v[57,441]v[60,441]v[60,439]v[60,439]v[60,439]v[60,441]v[62,442]v[62,442]v[60,442]v[60,445]v[62,445]v[66,447]v[66,447]v[66,448]v[69,448]v[81,452]v[81,452]v[80,452]v[80,454]v[89,455]v[91,456]v[91,456]v[89,456]v[89,457]v[90,458]v[90,459]v[93,459]v[93,460]v[92,460]v[92,462]v[93,462]v[93,463]v[94,463]v[94,464]v[94,464]v[92,464]v[92,465]v[92,466]v[93,466]v[96,466]v[96,467]v[96,467]v[96,468]v[96,469]v[95,470]v[102,470]v[110,460]v[113,460]v[113,459]v[116,458]v[116,460]v[125,460]
v[124,460]v[146,460]v[137,470]v[150,470]v[150,434]v[152,434]v[152,429]v[159,429]v[159,431]v[162,431]v[162,434]v[165,434]v[165,470]v[169,470]v[169,466]v[168,466]v[168,461]v[169,461]v[170,454]v[171,454]v[171,453]v[171,454]v[171,454]v[173,461]v[173,461]v[173,466]v[173,466]v[173,470]v[178,470]v[178,441]v[179,441]v[179,437]v[180,437]v[185,439]v[185,437]v[185,437]v[192,440]v[192,442]v[193,442]v[193,470]v[197,470]v[197,425]v[198,425]v[198,423]v[197,423]v[197,421]v[199,421]v[209,411]v[219,421]v[221,421]v[221,423]v[221,423]v[221,425]v[222,425]v[222,459]v[228,459]v[228,458]v[232,441]v[233,427]v[234,425]v[236,426]v[237,441]v[241,457]v[278,455]v[284,436]v[284,421]v[284,419]v[286,417]v[288,419]v[289,436]v[294,455]v[306,455]v[306,451]v[305,450]v[306,450]v[306,446]v[306,446]v[306,443]v[308,443]v[308,442]v[308,442]v[308,441]v[308,440]v[309,440]v[309,437]v[310,440]v[310,440]v[311,442]v[311,442]v[311,443]v[315,443]v[343,443]v[344,442]v[348,442]v[348,428]v[347,427]v[347,427]v[348,426]v[348,417]v[347,417]
v[348,416]v[348,415]v[348,413]v[349,409]v[350,413]v[352,406]v[351,404]v[351,403]v[351,401]v[351,400]v[352,399]v[352,394]v[352,394]v[353,399]v[353,399]v[353,394]v[354,393]v[355,386]v[356,393]v[357,394]v[357,399]v[357,399]v[358,394]v[358,394]v[358,399]v[359,400]v[359,401]v[359,403]v[359,404]v[358,406]v[360,413]v[361,409]v[362,413]v[362,415]v[362,416]v[363,417]v[362,417]v[362,426]v[363,427]v[363,427]v[362,428]v[362,442]v[366,442]v[367,443]v[395,443]v[399,443]v[399,442]v[399,442]v[400,440]v[400,440]v[401,437]v[401,440]v[402,440]v[402,441]v[402,442]v[402,442]v[402,443]v[404,443]v[404,446]v[404,446]v[404,450]v[405,450]v[404,451]v[404,457]v[405,458]v[405,464]v[405,465]v[405,470]v[411,470]v[411,452]v[410,450]v[410,449]v[411,449]v[411,441]v[411,440]v[411,437]v[413,437]v[413,427]v[412,427]v[412,423]v[417,419]v[418,416]v[418,411]v[412,411]v[413,410]v[412,408]v[419,408]v[419,416]v[420,419]v[424,423]v[424,427]v[424,427]v[424,437]v[426,437]v[426,440]v[425,441]v[425,449]v[427,449]v[427,450]v[426,452]
v[426,470]v[435,470]v[436,425]v[432,422]v[431,420]v[431,418]v[434,418]v[437,418]v[437,412]v[429,409]v[429,406]v[436,406]v[437,374]v[436,374]v[436,372]v[437,372]v[437,362]v[437,362]v[437,360]v[437,360]v[438,351]v[440,351]v[440,351]v[440,360]v[441,360]v[441,362]v[440,362]v[440,372]v[441,372]v[441,374]v[440,374]v[441,406]v[449,406]v[449,409]v[441,412]v[441,418]v[444,418]v[447,418]v[447,420]v[445,422]v[441,425]v[442,470]v[449,470]v[449,438]v[468,434]v[482,427]v[503,412]v[515,418]v[515,467]v[515,470]v[520,470]v[520,460]v[521,459]v[521,457]v[521,457]v[523,455]v[523,455]v[524,455]v[524,455]v[525,455]v[525,455]v[527,457]v[527,457]v[527,459]v[530,459]v[530,456]v[530,456]v[530,454]v[530,454]v[531,452]v[536,450]v[541,452]v[542,454]v[542,454]v[542,456]v[542,456]v[542,459]v[545,459]v[545,457]v[545,457]v[547,455]v[547,455]v[548,455]v[548,455]v[549,455]v[549,455]v[551,457]v[551,457]v[551,459]v[553,459]v[553,457]v[553,457]v[555,455]v[555,455]v[556,455]v[557,455]v[558,455]v[558,455]v[560,457]v[560,457]
v[560,459]v[563,459]v[563,456]v[562,456]v[562,456]v[566,456]v[566,458]v[566,458]v[568,456]v[569,455]v[571,454]v[571,454]v[573,455]v[574,456]v[575,457]v[575,437]v[575,437]v[576,436]v[576,436]v[577,437]v[577,437]v[578,437]v[578,433]v[578,433]v[578,432]v[580,427]v[580,427]v[580,426]v[580,426]v[580,426]v[581,426]v[581,426]v[581,426]v[581,427]v[581,427]v[584,432]v[584,433]v[583,433]v[583,437]v[584,437]v[585,437]v[585,436]v[585,436]v[586,437]v[586,437]v[586,462]v[587,462]v[587,465]v[587,467]v[588,468]v[588,470]v[591,470]v[591,465]v[591,465]v[591,465]v[596,465]v[596,448]v[597,448]v[597,445]v[597,445]v[597,444]v[614,444]v[614,445]v[614,445]v[614,448]v[616,448]v[616,465]v[620,465]v[620,465]v[620,465]v[620,470]v[625,470]v[625,453]v[624,452]v[624,449]v[625,448]v[625,441]v[624,439]v[624,437]v[624,428]v[627,428]v[627,422]v[628,422]v[628,423]v[629,423]v[630,416]v[631,416]v[632,404]v[634,416]v[635,416]v[636,423]v[637,423]v[637,422]v[638,422]v[638,428]v[641,428]v[641,437]v[641,439]v[640,441]v[640,448]
v[641,449]v[641,452]v[640,453]v[640,470]v[645,470]v[645,469]v[645,461]v[648,461]v[648,445]v[648,445]v[648,440]v[655,433]v[662,440]v[662,445]v[663,445]v[663,457]v[666,459]v[667,458]v[669,456]v[671,456]v[673,458]v[677,458]v[677,464]v[680,465]v[680,470]v[686,470]v[686,458]v[681,455]v[681,454]v[681,454]v[681,454]v[681,454]v[687,448]v[687,448]v[687,447]v[688,447]v[688,447]v[688,447]v[750,447]v[750,447]v[750,448]v[750,448]v[750,448]v[750,448]v[756,454]v[756,454]v[756,454]v[756,454]v[756,455]v[752,458]v[752,470]v[761,470]p[286,439]v[282,455]v[291,455]v[286,439]v[286,439]p[235,445]v[233,451]v[232,458]v[237,458]v[235,445]p[292,458]v[281,458]v[286,465]v[287,465]v[291,458]v[292,458]v[292,458]p[295,458]v[289,466]v[289,468]v[289,470]v[305,470]v[305,465]v[305,464]v[305,458]v[295,458]v[295,458]p[278,458]v[240,460]v[237,466]v[236,470]v[284,470]v[284,467]v[278,458]v[278,458]p[237,460]v[233,461]v[235,464]v[237,460]p[171,461]v[169,463]v[173,463]v[173,463]v[171,461]p[229,461]v[222,462]v[222,470]v[233,470]
v[233,466]v[229,461]p[171,462]v[172,462]v[171,463]v[170,462]v[171,462]v[171,462]
")

(with-regis () "
p[0,0]
v[760,1][760,480][1,480][1,1]

p[0,302]
v[0,330][2,330][4,330][5,330][5,329][6,301][7,272][8,257][9,226][9,211][9,209][9,207][13,206][16,206][16,205][13,199][9,195][6,191][2,187][2,184][2,181][5,181][7,181][7,178][6,175][5,173][3,172][2,171][3,154][4,123][4,102][4,94][5,74][6,61][6,57][6,50][11,50][15,50][15,59][15,68][19,68][22,68][24,72][26,76][32,76][37,76][37,74][37,72][39,72][50,77][59,82][68,88][77,93][81,96][82,96][92,91][101,85][106,82][117,76][120,76][122,76][124,76][124,78][125,80][130,80][135,80][138,76][140,72][143,72][146,72][146,64][146,55][148,55][150,55][150,58][150,62][151,62][152,62][152,86][152,111][155,111][158,111][158,116][158,122][159,122][161,122][161,132][163,150][166,158][167,162][172,168][176,170][178,171][180,172][180,180][182,188][182,188][183,188][184,188][184,180][184,172][186,172][191,167][194,163][195,160][198,142][198,128][198,116][198,105][200,105][202,105][202,100][202,94][205,94][207,94][207,69][207,45][210,45][212,45][212,40][212,36][214,36][215,36][215,46][215,56][222,56][229,56][229,62][229,69][238,69][248,69][248,73][254,91][261,101][264,105][275,110][283,111][287,112][291,112][292,116][294,120][298,120][302,120][303,116][305,112][308,112][315,110][318,109][320,108][331,88][335,72][335,63][336,58][345,58][355,58][355,51][355,44][361,44][368,44][368,34][368,24][371,24][374,24][374,42][374,59][376,59][378,59][379,59][379,60][380,69][382,78][382,83][384,92][386,92][389,83][390,73][391,63][393,55][393,51][393,50][394,49][408,49][421,49][421,72][421,95][424,95][427,95][429,101][431,107][431,117][431,127][432,127][433,125][433,123][434,120][435,118][436,118][437,118][439,118][439,118][439,118][441,144][443,170][443,172][443,174][445,174][448,174][448,173][448,173][450,159][451,145][451,138][452,124][453,117][453,117][454,117][455,117][456,117][457,119][457,122][458,124][459,127][460,127][461,127][462,127][462,117][462,108][464,101][466,94][469,94][472,94][472,71][472,49][486,49][500,49][506,49][506,49][507,58][509,67][509,72][511,81][513,81][514,81][516,72][517,63][518,59][518,59][520,59][522,59][523,59][525,59][525,41][525,24][526,24][528,24][528,26][528,27][530,27][532,27][532,53][532,79][535,82][538,85][538,90][538,94][540,96][542,98][542,104][542,110][544,110][545,110][545,104][545,98][547,96][549,94][549,90][549,85][552,82][555,78][555,70][555,62][558,62][560,62][565,72][571,82][574,86][574,86][580,77][585,67][588,62][594,52][596,52][598,52][598,60][598,68][601,71][604,75][604,79][604,83][606,86][608,88][608,94][608,99][610,99][612,99][612,94][612,88][614,86][616,83][616,79][616,75][618,72][621,68][621,43][621,17][622,17][624,17][624,24][624,31][625,31][626,31][627,76][627,121][654,121][682,121][682,76][682,31][684,31][686,31][686,27][686,24][688,24][690,24][690,60][690,96][714,96][738,96][738,60][738,24][743,24][748,24][748,18][748,11][754,11][760,11][760,5][760,0][402,0][44,0][44,9][44,19][50,19][55,19][55,22][55,24][59,24][63,24][63,68][63,111][70,111][77,111][77,101][77,91][93,91][109,91][109,57][109,24][112,24][114,24][114,20][114,16][118,16][122,16][124,17][124,17][124,35][125,53][125,63][126,100][127,127][127,137][128,157][126,158][125,162][125,164][125,166][125,168][128,168][130,168][130,171][130,175][127,178][123,184][123,186][123,188][123,190][126,190][130,190][130,193][131,224][132,253][133,269]
")