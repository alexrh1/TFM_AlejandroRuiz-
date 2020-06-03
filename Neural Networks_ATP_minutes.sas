libname datos "F:\TFM\Datos";run;
data atp;
set datos.redes_transf;
run;
%randomselect(data=DATOS.redes_transf,
directorio=C:\Users\aleja\Desktop,
listclass=tourney_level month best_of round G_tourney_name,
vardepen=minutes,
modelo=tourney_level month best_of round min_rank_points G_tourney_name IMP_REP_dif_rank IMP_REP_dif_rank_points IMP_REP_max_rank IMP_REP_max_rank_points IMP_REP_min_age IMP_REP_min_rank aleat LOG_IMP_REP_dif_rank SQR_IMP_REP_dif_rank_points LOG_IMP_REP_max_rank PWR_IMP_REP_min_age LOG_IMP_REP_min_rank,
criterio=AIC,
sinicio=12345,
sfinal=12400,
fracciontrain=0.8);

ods graphics off;
%cruzada(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375);

data final1;set final;modelo=1;
%cruzada(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375);
data final2;set final;modelo=2;
%cruzada(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375);
data final3;set final;modelo=3;
%cruzada(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat ,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375);
data final4;set final;modelo=4;

data union;set final1 final2 final3 final4;
proc boxplot data=union;plot media*modelo;run;

ods html;

%redneuronal(archivo=atp,listclass=tourney_level best_of round G_tourney_name,listconti=min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
vardep=minutes,porcen=0.80,semilla=471145,ocultos=18,algo=BPROP MOM=0.5 LEARN=0.1,acti=TANH);

%redneuronal(archivo=atp,listclass=tourney_level best_of round G_tourney_name,listconti=min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
vardep=minutes,porcen=0.80,semilla=471145,ocultos=15,algo=QUANEW ,acti=TANH);

%redneuronal(archivo=atp,listclass=tourney_level best_of round G_tourney_name,listconti=min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
vardep=minutes,porcen=0.80,semilla=471145,ocultos=15,algo=LEVMAR ,acti=TANH);

/*Primer intento de validación cruzada repetida*/

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=3,early=,algo=QUANEW);
data final5;set final;modelo=5;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=5,early=28,algo=bprop mom=0.5 learn=0.1);
data final6;set final;modelo=6;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=5,early=37,algo=QUANEW);
data final7;set final;modelo=7;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=10,early=28,algo=bprop mom=0.5 learn=0.1);
data final8;set final;modelo=8;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat ,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=15,early=28,algo=bprop mom=0.5 learn=0.1);
data final9;set final;modelo=9;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=15,early=37,algo=QUANEW);
data final10;set final;modelo=10;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=21,early=37,algo=QUANEW);
data final11;set final;modelo=11;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=21,early=28,algo=bprop mom=0.5 learn=0,1);
data final12;set final;modelo=12;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=10,early=37,algo=QUANEW);
data final13;set final;modelo=13;

data union;set final1 final2 final3 final4 final5 final6 final7 final8 final9 final10 final11 final12 final13;
proc boxplot data=union;plot media*modelo;run;

/*segundo intento para mejorar la regresion*/
%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=3,early=37,algo=QUANEW);
data final5;set final;modelo=5;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=5,early=28,algo=bprop mom=0.5 learn=0.01);
data final6;set final;modelo=6;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=10,early=28,algo=bprop mom=0.5 learn=0.01);
data final8;set final;modelo=8;

data union;set final1 final5 final6 final8 ;
proc boxplot data=union;plot media*modelo;run;

/*tercer intento para mejorar la regresión*/
%redneuronal(archivo=atp,listclass=tourney_level best_of round G_tourney_name,listconti=min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
vardep=minutes,porcen=0.80,semilla=471145,ocultos=15,algo=QUANEW ,acti=LOG);
%redneuronal(archivo=atp,listclass=tourney_level best_of round G_tourney_name,listconti=min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
vardep=minutes,porcen=0.80,semilla=471145,ocultos=15,algo=QUANEW ,acti=SOF);
%redneuronal(archivo=atp,listclass=tourney_level best_of round G_tourney_name,listconti=min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
vardep=minutes,porcen=0.80,semilla=471145,ocultos=15,algo=LEVMAR ,acti=SOF);

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=3,early=,algo=QUANEW, acti=LOG);
data final5;set final;modelo=5;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=3,early=,algo=LEVMAR, acti=LOG);
data final6;set final;modelo=6;

%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=15,early=,algo=QUANEW, acti=LOG);
data final7;set final;modelo=7;

proc printto print='c:\cubobasura.txt' ;run;
%cruzadaneural(archivo=atp,vardepen=minutes,
conti= min_rank_points IMP_REP_min_rank LOG_IMP_REP_min_rank IMP_REP_min_age aleat,
categor=tourney_level best_of round G_tourney_name,
ngrupos=4,sinicio=12345,sfinal=12375,nodos=15,early=,algo=LEVMAR, acti=LOG);
data final8;set final;modelo=9;

proc printto ;run;

data union;set final1 final5 final6 final7 final8 ;
proc boxplot data=union;plot media*modelo;run;

ODS HTML close;
ods listing;
