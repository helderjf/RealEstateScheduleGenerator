
:- use_module(library(random)).

%----------------------------------BASE_CONHECIMENTO-----------------------------------

%ZONAS(#CODIGO,#DESCRICAO)
zona(a, 'Hospital Sao Joao').
zona(b, 'Areosa').
zona(c, 'Prelada').
zona(d, 'Paranhos').
zona(e, 'Contumil').
zona(f, 'Boavista').
zona(g, 'Antas').
zona(h, 'Fonte da Moura').
zona(i, 'Foz do Douro').
zona(j, 'Campanha').
zona(l, 'Massarelos').
zona(m, 'Lordelo do Ouro').
zona(n, 'Campo 24 Agosto').
zona(o, 'Azevedo').
zona(p, 'Vitoria').
zona(q, 'Freixo').
zona(r, 'Fontainhas').

%DISTANCIAS(#ORIGEM,#DESTINO,#MINUTOS_DESLOCACAO)
distancia(a,b,7).
distancia(a,c,8).
distancia(a,d,5).
distancia(a,e,12).
distancia(b,e,6).
distancia(d,e,7).
distancia(c,d,8).
distancia(c,f,13).
distancia(c,i,18).
distancia(d,f,15).
distancia(f,h,3).
distancia(f,m,4).
distancia(f,i,6).
distancia(i,m,5).
distancia(e,f,12).
distancia(e,g,2).
distancia(e,j,5).
distancia(g,h,16).
distancia(g,l,18).
distancia(g,j,6).
distancia(h,m,4).
distancia(h,n,15).
distancia(h,l,3).
distancia(j,l,16).
distancia(j,o,3).
distancia(l,n,18).
distancia(l,o,20).
distancia(m,n,15).
distancia(m,p,10).
distancia(n,p,11).
distancia(n,r,12).
distancia(o,n,3).
distancia(o,q,5).
distancia(p,r,4).


estrada(X,Y,Z):-distancia(X,Y,Z);distancia(Y,X,Z).


%COORDENADAS(#ZONA,#X,#Y)
coordenadas(a,45,95).
coordenadas(b,90,95).
coordenadas(c,15,85).
coordenadas(d,40,80).
coordenadas(e,70,80).
coordenadas(f,25,65).
coordenadas(g,65,65).
coordenadas(h,45,55).
coordenadas(i,5,50).
coordenadas(j,80,50).
coordenadas(l,65,45).
coordenadas(m,25,40).
coordenadas(n,55,30).
coordenadas(o,80,30).
coordenadas(p,25,15).
coordenadas(q,80,15).
coordenadas(r,55,10).



diasSemana(1,'Segunda-Feira').
diasSemana(2,'Terca-Feira').
diasSemana(3,'Quarta-Feira').
diasSemana(4,'Quinta-Feira').
diasSemana(5,'Sexta-Feira').
diasSemana(6,'Sabado').
diasSemana(7,'Domingo').






tempo_visita('T0',20).
tempo_visita('T1',30).
tempo_visita('T2',45).
tempo_visita('T3',50).
tempo_visita('T4',60).


imoveis_cliente(c1001,[i1001,i1005]).
imoveis_cliente(c1002,[i1002,i1003]).
imoveis_cliente(c1003,[i1003,i1005,i1002]).



mediador(m1001,'Zacarias Lopes',		[(3, 9:00, 17:00), (5, 9:00, 17:00)]).
mediador(m1002,'Zeca Afonso',			[(4, 12:00, 20:00), (5, 11:00, 19:30)]).
mediador(m1003,'Guilherme Andrade',		[(1, 10:00, 19:30),(2, 9:00, 19:00), (4, 11:00, 19:30)]).


cliente(c1001,'Tomas Torres',		[(2, 17:00, 20:30), (3, 14:00, 18:30),(7, 10:00,14:00), (1, 9:00, 12:00)]).
cliente(c1002,'Joao Lopes',			[(2, 17:00, 20:30), (5, 14:00, 18:30),(7, 10:00,14:00), (1, 9:00, 12:00)]).
cliente(c1003,'Pedro Miguel',		[(2, 17:00, 20:30), (5, 14:00, 19:30),(7, 10:00,14:00), (1, 9:00, 12:00)]).


imovel(i1001,'Paranhos','T2',175000,n,			[(1, 14:00, 18:00), (2, 14:00, 18:30),(4,14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1002,'Areosa','T0',100000,s,			[(2,  9:00, 18:00), (3, 12:00, 15:30),(4,11:00, 18:30),(5, 14:00, 18:00),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1003,'Campanha','T3',180000,n,			[(1, 15:00, 19:30), (3,  8:00, 18:00),(4,16:00, 18:30),(5, 12:00, 18:30),(6, 10:00, 18:30),(7, 9:00, 10:30), (7,15:00, 16:00)]).
imovel(i1004,'Prelada','T1',110000,n,			[(2, 10:00, 18:30), (3, 14:00, 18:30),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,16:00, 20:00)]).
imovel(i1005,'Contumil','T2',90000,s,			[(1,  9:00, 18:30), (2, 14:00, 18:00),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1006,'Boavista','T0',80000,n,			[(2,  8:00, 18:30), (3, 14:00, 18:30),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1007,'Antas','T4',200000,n,				[(2, 11:00, 20:30), (3, 14:00, 18:45),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1008,'Fonte da Moura','T1',150000,n,	[(2, 13:00, 10:30), (3, 12:00, 18:30),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1009,'Foz do Douro','T3',180000,s,		[(1, 13:00, 20:00), (3, 14:00, 18:30),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1010,'Massarelos','T3',100000,n,		[(2, 15:00, 18:30), (3, 15:00, 18:45),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1011,'Campo 24 Agosto','T2',85000,n,	[(2, 12:00, 18:30), (3, 14:00, 18:30),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1012,'Azevedo','T4',210000,s,			[(1, 11:00, 15:00), (3, 14:00, 18:00),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1013,'Vitoria','T2',175000,n,			[(1, 15:00, 18:00), (2, 14:00, 18:45),(3,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1014,'Freixo','T1',170000,n,			[(2, 16:00, 18:30), (3, 14:00, 18:30),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1015,'Fontainhas','T0',135000,n,		[(1, 12:00, 16:00), (3, 14:00, 18:00),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1016,'Antas','T4',140000,n,				[(1, 15:00, 18:30), (3, 14:00, 18:30),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1017,'Paranhos','T3',150000,n,			[(2, 11:00, 19:30), (3, 14:00, 18:00),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1018,'Freixo','T1',125000,n,			[(1, 10:00, 18:00), (3, 14:00, 18:30),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).
imovel(i1019,'Foz do Douro','T2',115000,n,		[(2, 14:00, 18:30), (3, 14:00, 18:30),(4,14:00, 18:30),(5, 14:00, 18:30),(6, 14:00, 18:30),(7, 9:00, 12:30), (7,15:00, 19:00)]).






%----------------------------------TRATAMENTO_INFORMACAO-----------------------------------


%----------------------------				converter hora do dia em minutos
converte(H:M,R):-
	R1 is H*60,
	R is R1 + M.

%----------------------------				converter minutos em hora do dia
converteMH(Minutos,H:M):-
	H is Minutos div 60,
	M is Minutos mod 60.

%----------------------------				separar as disponibilidades dos imoveis
separaImoveis:-
	retractall(disponibilidadeImovel(_,_,_,_,_,_,_,_)),
	findall((Imovel,Zona,Tipo,Valor,TChave,LDisp),imovel(Imovel,Zona,Tipo,Valor,TChave,LDisp),L),
	separaImoveis(L).

separaImoveis([]):-!.
separaImoveis([(Imovel,Zona,Tipo,Valor,TChave,LDisp)|Tail]):-
	separaImovel(Imovel,Zona,Tipo,Valor,TChave,LDisp),
	separaImoveis(Tail).


separaImovel(_,_,_,_,_,[]):-!.
separaImovel(Imovel,Zona,Tipo,Valor,TChave,[(Dia,Inicio,Fim)|Tail]):-
	assertz(disponibilidadeImovel(Imovel,Zona,Tipo,Valor,TChave,Dia,Inicio,Fim)),
	separaImovel(Imovel,Zona,Tipo,Valor,TChave,Tail).


%----------------------------			separar as disponibilidades dos clientes
separaClientes:-
	retractall(disponibilidadeCliente(_,_,_,_,_)),
	findall((Cliente,NomeCliente,LDisp),(cliente(Cliente,NomeCliente,LDisp)),L),
	separaClientes(L).

separaClientes([]):-!.
separaClientes([(Cliente,NomeCliente,LDisp)|Tail]):-
	separaCliente(Cliente,NomeCliente,LDisp),
	separaClientes(Tail).

separaCliente(_,_,[]):-!.
separaCliente(Cliente,NomeCliente,[(Dia,Inicio,Fim)|Tail]):-
	assertz(disponibilidadeCliente(Cliente,NomeCliente,Dia,Inicio,Fim)),
	separaCliente(Cliente,NomeCliente,Tail).


%---------------criar horarios
criaHorario(N):-N==1260.
criaHorario(N):-assert(horario(N)),
N1 is N+5,
criaHorario(N1).
%----------------------------			separar as disponibilidades dos mediadores
separaMediadores:-
	retractall(disponibilidadeMediador(_,_,_,_,_)),
	findall((Mediador,NomeMediador,LDisp),(mediador(Mediador,NomeMediador,LDisp)),L),
	separaMediadores(L).

separaMediadores([]):-!.
separaMediadores([(Mediador,NomeMediador,LDisp)|Tail]):-
	separaMediador(Mediador,NomeMediador,LDisp),
	separaMediadores(Tail).

separaMediador(_,_,[]):-!.
separaMediador(Mediador,NomeMediador,[(Dia,Inicio,Fim)|Tail]):-
	assertz(disponibilidadeMediador(Mediador,NomeMediador,Dia,Inicio,Fim)),
	separaMediador(Mediador,NomeMediador,Tail).



%----------------------------			separar factos com os imoveis que cada cliente quer visitar
separaRelacoesClienteImoveis:-
	retractall(imovelCliente(_,_)),
	findall((C,L),(imoveis_cliente(C,L)),LR),
	percorreLista(LR).

percorreLista([]):-!.
percorreLista([(C,L)|T]):-
	separa(C,L),
	percorreLista(T).

separa(_,[]):-!.
separa(Cliente,[Imovel|T]):-
	assertz(imovelCliente(Cliente,Imovel)),
	separa(Cliente,T).





%----------------------------    encontrar todas as visitas possiveis com todos os clientes, a todos os imoveis que estes querem visitar, em todos os horarios possiveis
determinaVisitasPossiveis:-
	findall(
		(Imovel,Mediador,Cliente,Dia,HorarioI),
		(
			imovelCliente(Cliente,Imovel),
			tempo_visita(Tipo,TempoVisita),
			zona(Zona,NomeZona),

			disponibilidadeCliente(Cliente,_,Dia,InicioCH,FimCH),
			disponibilidadeMediador(Mediador,_,Dia,InicioMH,FimMH),
			disponibilidadeImovel(Imovel,NomeZona,Tipo,Valor,n,Dia,InicioIH,FimIH),


			converte(InicioCH,InicioC),
			converte(InicioMH,InicioM),
			converte(InicioIH,InicioI),
			converte(FimCH,FimC),
			converte(FimMH,FimM),
			converte(FimIH,FimI),

			horario(HorarioI),
			HorarioI >=InicioC,
			HorarioI >=InicioM,
			HorarioI >=InicioI,

			tempo_visita(Tipo,TempoVisita),
			HorarioF is HorarioI + TempoVisita,
			HorarioF =< FimC,
			HorarioF =< FimM,
			HorarioF =< FimI
		),
		LSemChave),

		findall(
		(Imovel,Mediador,Cliente,Dia,HorarioI),
		(
			imovelCliente(Cliente,Imovel),
			tempo_visita(Tipo,TempoVisita),
			zona(Zona,NomeZona),

			disponibilidadeCliente(Cliente,_,Dia,InicioCH,FimCH),
			disponibilidadeMediador(Mediador,_,Dia,InicioMH,FimMH),
			disponibilidadeImovel(Imovel,NomeZona,Tipo,Valor,s,Dia,InicioIH,FimIH),


			converte(InicioCH,InicioC),
			converte(InicioMH,InicioM),
			%converte(InicioIH,InicioI),
			converte(FimCH,FimC),
			converte(FimMH,FimM),
			%converte(FimIH,FimI),

			horario(HorarioI),
			HorarioI >=InicioC,
			HorarioI >=InicioM,
			%HorarioI >=InicioI,

			tempo_visita(Tipo,TempoVisita),
			HorarioF is HorarioI + TempoVisita,
			HorarioF =< FimC,
			HorarioF =< FimM
			%HorarioF =< FimI
		),
		LComChave),
	append(LSemChave,LComChave,LTotal),
	eliminaRepetidos(LTotal,L),
	retractall(listaVisitasPossiveis(_)),
	assertz(listaVisitasPossiveis(L)),
	retractall(visitaPossivel(_,_,_,_,_)),
	insereVisitasPossiveis(L,1).

insereVisitasPossiveis([],_):-!.
insereVisitasPossiveis([(Imovel,Cliente,Dia,Horario)|Tail],IDVisita):-
	assert(visitaPossivel(IDVisita,Imovel,Cliente,Dia,Horario)),
	IDVisita1 is IDVisita+1,
	insereVisitasPossiveis(Tail,IDVisita1).



%------------------------			 eliminar elementos repetidos de uma lista
eliminaRepetidos(L,LR):-
	eliminaRepetidos(L,[],LR).

eliminaRepetidos([],Laux,Laux):-!.

eliminaRepetidos([H|T],Laux,LR):-
	member(H,Laux),!,
	eliminaRepetidos(T,Laux,LR).

eliminaRepetidos([H|T],Laux,LR):-
	\+ member(H,Laux),
	eliminaRepetidos(T,[H|Laux],LR).





%------------------------	ALGORITMO GENETICO		---------------------------------------------

%------------------------			 parameterizacao
geracoes(1000).
melhoresAGuardar(4).
populacao(20).
prob_cruzamento(0.6).
prob_mutacao(0.8).



%----------------------------		contar o nUmero de visitas a fazer

contaRelacoesClienteImoveis(NV):-
	findall((Cliente,Imovel),imovelCliente(Cliente,Imovel),LR),
	contaElementos(LR,NV).

contaElementos([],0):-!.
contaElementos([_|T],NE):-
	contaElementos(T,NE1),
	NE is NE1 +1.



%----------------------------				gerar populacao (solucoes aleatorias)
gera_populacao(Pop):-
	populacao(TamPop),
	listaVisitasPossiveis(LVP),
	contaRelacoesClienteImoveis(NVR),
	gera_populacao1(TamPop,NVR,LVP,Pop).

gera_populacao1(0,_,_,[]):-!.
gera_populacao1(TamPop,NVisitasRealizar,LVisitasPossiveis,[Ind|Pop1]):-
	TamPop1 is TamPop-1,
	gera_populacao1(TamPop1,NVisitasRealizar,LVisitasPossiveis,Pop1),
	gera_individuo(NVisitasRealizar,LVisitasPossiveis,Ind).


gera_individuo(0,_,[]):-!.
gera_individuo(NVisitasRealizar,LVisitasPossiveis,[X|Ind1]):-
	NVisitasRealizar1 is NVisitasRealizar -1,
	gera_individuo(NVisitasRealizar1,LVisitasPossiveis,Ind1),
	random_member(X,LVisitasPossiveis).



%---------------------------- AVALIACAO


%CRITERIOS AVALIACAO
	% verificar se todas as visitas cliente sao feitas (penalizacao grande caso nao)
	% contar numero de dias diferentes (penalizar - para minimizar o numero de dias)
	% contar numero de mudanÃ§as de par Mediador / cliente(penalizar- para tentar fazer as visitas de um ciente todas com o mesmo mediador e seguidas)
	% para o mesmo mediador veficar se tem tempo de se deslocar entre imoveis durante o dia
	% para o mesmo cliente veficar se tem tempo de se deslocar entre imoveis durante o dia
		%(penalizacao grande caso nÃ£o haja tempo para a deslocacao)
		%(penalizacao pequena caso tenha que ficar a espera pela hora da visita)
	%verificar que nao ha imoveis a serem visitados ao mesmo tempo por clientes diferentes (penalizacao grande por cada sobreposicao)



%---------------------------- associar o valor da penalizacao a cada individuo
avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Penalizacao*Ind|Resto1]):-
	avalia_individuo(Ind,Penalizacao),
	avalia_populacao(Resto,Resto1).



%----------------------------					penalizar(avaliar) individuo mediante os criterios de avaliacao
avalia_individuo(Ind,Penalizacao):-
	contaVisitasNaoFeitas(Ind,NVNF),
	diasDistintos(Ind,NDD),
	contaMudancasMediadorCliente(Ind,NM),
	calculaSobreposicoesMediador(Ind,MSM,MEM),%para o mediador
	calculaSobreposicoesCliente(Ind,MSC,MEC),%para o cliente
	contaSobreposicoesImoveis(Ind,NS),

	Penalizacao is (NVNF*300 + (NDD-1)*5 + NM*5 + MSM*5 + MEM*0 +MSC*5 + MEC*1 + NS*100).










%----------------------------						contar quantas visitas nao sao feitas
contaVisitasNaoFeitas(Ind,NVisitasNaoFeitas):-
	findall((I,C),imovelCliente(C,I),LVisitasAFazer),
	contaVisitasNaoFeitas(Ind,LVisitasAFazer,NVisitasNaoFeitas).

contaVisitasNaoFeitas(_,[],0):-!.

contaVisitasNaoFeitas(Ind,[(Imovel,Cliente)|Tail],NVisitasNaoFeitas):-
	member((Imovel,_,Cliente,_,_),Ind),!,
	contaVisitasNaoFeitas(Ind,Tail,NVisitasNaoFeitas1),
	NVisitasNaoFeitas is NVisitasNaoFeitas1.

contaVisitasNaoFeitas(Ind,[(Imovel,Cliente)|Tail],NVisitasNaoFeitas):-
	\+ member((Imovel,_,Cliente,_,_),Ind),
	contaVisitasNaoFeitas(Ind,Tail,NVisitasNaoFeitas1),
	NVisitasNaoFeitas is NVisitasNaoFeitas1 +1.



%----------------------------				contar dias distintos presentes no individuo
diasDistintos(Ind,NDD):-
	diasDistintos(Ind,7,NDD).

diasDistintos(_,0,0):-!.

diasDistintos(Ind,D,NDD):-
	member((_,_,_,D,_),Ind),!,
	D1 is D-1,
	diasDistintos(Ind,D1,NDD1),
	NDD is NDD1+1.

diasDistintos(Ind,D,NDD):-
	\+ member((_,_,_,D,_),Ind),
	D1 is D-1,
	diasDistintos(Ind,D1,NDD1),
	NDD is NDD1.




%----------------------------				ordenar individuo cronologicamente
ordemCronologica(Ind,IndCron):-
	sequenciaDiaHorarioImovelMediadorCliente(Ind,IndDH),
	msort(IndDH,IndDHOrd),
	sequenciaImovelMediadorClienteDiaHorario(IndDHOrd,IndCron).


sequenciaDiaHorarioImovelMediadorCliente([],[]).
sequenciaDiaHorarioImovelMediadorCliente([(Imovel,Mediador,Cliente,Dia,Horario)|Tail],[(Dia,Horario,Imovel,Mediador,Cliente)|IndDH1]):-
	sequenciaDiaHorarioImovelMediadorCliente(Tail,IndDH1).


sequenciaImovelMediadorClienteDiaHorario([],[]).
sequenciaImovelMediadorClienteDiaHorario([(Dia,Horario,Imovel,Mediador,Cliente)|Tail],[(Imovel,Mediador,Cliente,Dia,Horario)|Ind1]):-
	sequenciaImovelMediadorClienteDiaHorario(Tail,Ind1).




%----------------------------				extrair lista com os diferentes clientes
extraiClientes(Ind,LClientes):-
	extraiClientes(Ind,[],LClientes).

extraiClientes([],Laux,Laux):-!.

extraiClientes([(_,_,Cliente,_,_)|Tail],Laux,LClientes):-
	member(Cliente,Laux),!,
	extraiClientes(Tail,Laux,LClientes).

extraiClientes([(_,_,Cliente,_,_)|Tail],Laux,LClientes):-
	\+ member(Cliente,Laux),
	extraiClientes(Tail,[Cliente|Laux],LClientes).


%----------------------------				extrair lista com os diferentes mediadores
extraiMediadores(Ind,LMediadores):-
	extraiMediadores(Ind,[],LMediadores).

extraiMediadores([],Laux,Laux):-!.

extraiMediadores([(_,Mediador,_,_,_)|Tail],Laux,LMediadores):-
	member(Mediador,Laux),!,
	extraiMediadores(Tail,Laux,LMediadores).

extraiMediadores([(_,Mediador,_,_,_)|Tail],Laux,LMediadores):-
	\+ member(Mediador,Laux),
	extraiMediadores(Tail,[Mediador|Laux],LMediadores).





%----------------------------			contar mudanca de par mediador / cliente
contaMudancasMediadorCliente(Ind,NMudancas):-
	extraiClientes(Ind,LClientes),
	contaMudancasMediadorCliente(Ind,LClientes,NMudancas).

contaMudancasMediadorCliente(_,[],0):-!.

contaMudancasMediadorCliente(Ind,[Cliente|Tail],NMMC):-
	contaMediadoresCliente(Cliente,Ind,NMC),
	contaMudancasMediadorCliente(Ind,Tail,NMMC1),
	NMMC is NMMC1+NMC-1.


%----------------------------			contar com quantos mediadores diferentes interage um cliente
contaMediadoresCliente(Cliente,Ind,NMC):-
mediadoresDoCliente(Cliente,Ind,[],LMC),
contaElementos(LMC,NMC).

mediadoresDoCliente(_,[],Laux,Laux):-!.

mediadoresDoCliente(Cliente,[(_,Mediador,Cliente,_,_)|Tail],Laux,LMC):-
	member(Mediador,Laux),!,
	mediadoresDoCliente(Cliente,Tail,Laux,LMC).

mediadoresDoCliente(Cliente,[(_,Mediador,Cliente,_,_)|Tail],Laux,LMC):-
	\+ member(Mediador,Laux),!,
	mediadoresDoCliente(Cliente,Tail,[Mediador|Laux],LMC).

mediadoresDoCliente(Cliente,[(_,_,_,_,_)|Tail],Laux,LMC):-
	mediadoresDoCliente(Cliente,Tail,Laux,LMC).

%----------------------------
calculaSobreposicoesMediador(Ind,MinutosSobrepostos,MinutosEspera):-
	ordemCronologica(Ind,IndCron),
	extraiMediadores(Ind,LMediadores),
	calculaSobreposicoesMediador(IndCron,LMediadores,MinutosSobrepostos,MinutosEspera).



calculaSobreposicoesMediador(_,[],0,0):-!.

calculaSobreposicoesMediador(IndCron,[Mediador|Tail],MinutosSobrepostos,MinutosEspera):-
	filtraIndividuoPorMediador(IndCron,Mediador,IndCronMed),
	calculaSobreposicoesMediador1(IndCronMed,MSobrMed,MEspMed),
	calculaSobreposicoesMediador(IndCron,Tail,MinutosSobrepostos1,MinutosEspera1),
	MinutosSobrepostos is MinutosSobrepostos1 + MSobrMed,
	MinutosEspera is MinutosEspera1 + MEspMed.


calculaSobreposicoesMediador1([(Imovel,_,_,Dia,Horario)|Tail],MSobrMed,MEspMed):-
	imovel(Imovel,NomeZona,Tipo,_,_,_),
	tempo_visita(Tipo,TempoVisita),
	zona(Zona,NomeZona),
	calculaSobreposicoesMediador1(Zona,Dia,Horario,TempoVisita,Tail,MSobrMed,MEspMed).



%----------------------------
calculaSobreposicoesMediador1(_,_,_,_,[],0,0):-!.

%caso em que nao se trata do mesmo dia
calculaSobreposicoesMediador1(_,DiaAnterior,_,_,[(Imovel,_,_,Dia,Horario)|Tail],MSobrMed,MEspMed):-
		imovel(Imovel,NomeZona,Tipo,_,_,_),
		tempo_visita(Tipo,ProxTempoVisita),
		zona(ProxZona,NomeZona),

		DiaAnterior \= Dia,!,
		calculaSobreposicoesMediador1(ProxZona,Dia,Horario,ProxTempoVisita,Tail,MSobrMed1,MEspMed1),
		MSobrMed is MSobrMed1,
		MEspMed is MEspMed1.


%caso em que se trata do mesmo dia e hÃ¡ sobreposicao
calculaSobreposicoesMediador1(ZonaAnterior,DiaAnterior,HorarioAnterior,TempoVisitaAnterior,[(Imovel,_,_,Dia,Horario)|Tail],MSobrMed,MEspMed):-
		imovel(Imovel,NomeZona,Tipo,_,_,_),
		tempo_visita(Tipo,ProxTempoVisita),
		zona(ProxZona,NomeZona),

		DiaAnterior == Dia, go(ZonaAnterior,ProxZona,_,Custo), (HorarioAnterior+TempoVisitaAnterior+Custo) >= Horario,!,
		calculaSobreposicoesMediador1(ProxZona,Dia,Horario,ProxTempoVisita,Tail,MSobrMed1,MEspMed1),
		MSobrMed is (HorarioAnterior+TempoVisitaAnterior+Custo-Horario+MSobrMed1),
		MEspMed is MEspMed1.

%caso em que se trata do mesmo dia e hÃ¡ espera
calculaSobreposicoesMediador1(ZonaAnterior,DiaAnterior,HorarioAnterior,TempoVisitaAnterior,[(Imovel,_,_,Dia,Horario)|Tail],MSobrMed,MEspMed):-
		imovel(Imovel,NomeZona,Tipo,_,_,_),
		tempo_visita(Tipo,ProxTempoVisita),
		zona(ProxZona,NomeZona),

		DiaAnterior == Dia, go(ZonaAnterior,ProxZona,_,Custo), (HorarioAnterior+TempoVisitaAnterior+Custo) < Horario,!,
		calculaSobreposicoesMediador1(ProxZona,Dia,Horario,ProxTempoVisita,Tail,MSobrMed1,MEspMed1),
		MSobrMed is MSobrMed1,
		MEspMed is (Horario - HorarioAnterior - TempoVisitaAnterior -Custo + MEspMed1).




%----------------------------
filtraIndividuoPorMediador([],_,[]):-!.

filtraIndividuoPorMediador([(Imovel,Mediador,Cliente,Dia,Horario)|Tail],Mediador,[(Imovel,Mediador,Cliente,Dia,Horario)|LR]):-!,
	filtraIndividuoPorMediador(Tail,Mediador,LR).

filtraIndividuoPorMediador([(_,_,_,_,_)|Tail],Mediador,LR):-
	filtraIndividuoPorMediador(Tail,Mediador,LR).







%----------------------------
calculaSobreposicoesCliente(Ind,MinutosSobrepostos,MinutosEspera):-
	ordemCronologica(Ind,IndCron),
	extraiClientes(Ind,LClientes),
	calculaSobreposicoesCliente(IndCron,LClientes,MinutosSobrepostos,MinutosEspera).



calculaSobreposicoesCliente(_,[],0,0):-!.

calculaSobreposicoesCliente(IndCron,[Cliente|Tail],MinutosSobrepostos,MinutosEspera):-
	filtraIndividuoPorCliente(IndCron,Cliente,IndCronCli),
	calculaSobreposicoesCliente1(IndCronCli,MSobrCli,MEspCli),
	calculaSobreposicoesCliente(IndCron,Tail,MinutosSobrepostos1,MinutosEspera1),
	MinutosSobrepostos is MinutosSobrepostos1 + MSobrCli,
	MinutosEspera is MinutosEspera1 + MEspCli.


calculaSobreposicoesCliente1([(Imovel,_,_,Dia,Horario)|Tail],MSobrCli,MEspCli):-
	imovel(Imovel,NomeZona,Tipo,_,_,_),
	tempo_visita(Tipo,TempoVisita),
	zona(Zona,NomeZona),
	calculaSobreposicoesCliente1(Zona,Dia,Horario,TempoVisita,Tail,MSobrCli,MEspCli).



%----------------------------
calculaSobreposicoesCliente1(_,_,_,_,[],0,0):-!.

%caso em que nao se trata do mesmo dia
calculaSobreposicoesCliente1(_,DiaAnterior,_,_,[(Imovel,_,_,Dia,Horario)|Tail],MSobrCli,MEspCli):-
		imovel(Imovel,NomeZona,Tipo,_,_,_),
		tempo_visita(Tipo,ProxTempoVisita),
		zona(ProxZona,NomeZona),

		DiaAnterior \= Dia,!,
		calculaSobreposicoesCliente1(ProxZona,Dia,Horario,ProxTempoVisita,Tail,MSobrCli1,MEspCli1),
		MSobrCli is MSobrCli1,
		MEspCli is MEspCli1.


%caso em que se trata do mesmo dia e ha sobreposicao
calculaSobreposicoesCliente1(ZonaAnterior,DiaAnterior,HorarioAnterior,TempoVisitaAnterior,[(Imovel,_,_,Dia,Horario)|Tail],MSobrCli,MEspCli):-
		imovel(Imovel,NomeZona,Tipo,_,_,_),
		tempo_visita(Tipo,ProxTempoVisita),
		zona(ProxZona,NomeZona),

		DiaAnterior == Dia, go(ZonaAnterior,ProxZona,_,Custo), (HorarioAnterior+TempoVisitaAnterior+Custo) >= Horario,!,
		calculaSobreposicoesCliente1(ProxZona,Dia,Horario,ProxTempoVisita,Tail,MSobrCli1,MEspCli1),
		MSobrCli is (HorarioAnterior+TempoVisitaAnterior+Custo-Horario+MSobrCli1),
		MEspCli is MEspCli1.

%caso em que se trata do mesmo dia e ha espera
calculaSobreposicoesCliente1(ZonaAnterior,DiaAnterior,HorarioAnterior,TempoVisitaAnterior,[(Imovel,_,_,Dia,Horario)|Tail],MSobrCli,MEspCli):-
		imovel(Imovel,NomeZona,Tipo,_,_,_),
		tempo_visita(Tipo,ProxTempoVisita),
		zona(ProxZona,NomeZona),

		DiaAnterior == Dia, go(ZonaAnterior,ProxZona,_,Custo), (HorarioAnterior+TempoVisitaAnterior+Custo) < Horario,!,
		calculaSobreposicoesCliente1(ProxZona,Dia,Horario,ProxTempoVisita,Tail,MSobrCli1,MEspCli1),
		MSobrCli is MSobrCli1,
		MEspCli is (Horario - HorarioAnterior - TempoVisitaAnterior -Custo + MEspCli1).




%----------------------------
filtraIndividuoPorCliente([],_,[]):-!.

filtraIndividuoPorCliente([(Imovel,Mediador,Cliente,Dia,Horario)|Tail],Cliente,[(Imovel,Mediador,Cliente,Dia,Horario)|LR]):-!,
	filtraIndividuoPorCliente(Tail,Cliente,LR).

filtraIndividuoPorCliente([(_,_,_,_,_)|Tail],Cliente,LR):-
	filtraIndividuoPorCliente(Tail,Cliente,LR).











%----------------------------				extrair lista com os diferentes imoveis
extraiImoveis(Ind,LImoveis):-
	extraiImoveis(Ind,[],LImoveis).

extraiImoveis([],Laux,Laux):-!.

extraiImoveis([(Imovel,_,_,_,_)|Tail],Laux,LImoveis):-
	member(Imovel,Laux),!,
	extraiImoveis(Tail,Laux,LImoveis).

extraiImoveis([(Imovel,_,_,_,_)|Tail],Laux,LImoveis):-
	\+ member(Imovel,Laux),
	extraiImoveis(Tail,[Imovel|Laux],LImoveis).


%----------------------------				filtra individuo deixando apenas os tuplos com o imovel passado por parametro
filtraIndividuoPorImovel([],_,[]):-!.

filtraIndividuoPorImovel([(Imovel,Mediador,Cliente,Dia,Horario)|Tail],Imovel,[(Imovel,Mediador,Cliente,Dia,Horario)|LR]):-!,
	filtraIndividuoPorImovel(Tail,Imovel,LR).

filtraIndividuoPorImovel([(_,_,_,_,_)|Tail],Imovel,LR):-
	filtraIndividuoPorImovel(Tail,Imovel,LR).


%----------------------------				contar visitas ao mesmo imovel sobrepostas
contaSobreposicoesImoveis(Ind,NS):-
	extraiImoveis(Ind,LImoveis),
	ordemCronologica(Ind,IndCron),
	contaSobreposicoesImoveis(IndCron,LImoveis,NS).


contaSobreposicoesImoveis(_,[],0):-!.

contaSobreposicoesImoveis(IndCron,[Imovel|Tail],NS):-
	filtraIndividuoPorImovel(IndCron,Imovel,IndImovel),
	contaSobreposicoesImovel(IndImovel,NSI),
	contaSobreposicoesImoveis(IndCron,Tail,NSI1),
	NS is NSI + NSI1.


%----------------------------
contaSobreposicoesImovel([(Imovel,_,_,Dia,Horario)|Tail],NSI):-
	imovel(Imovel,_,Tipo,_,_,_),
	tempo_visita(Tipo,DuracaoVisita),
	contaSobreposicoesImovel(Dia,Horario,DuracaoVisita,Tail,NSI).

contaSobreposicoesImovel(_,_,_,[],0):-!.

%caso em que ha sobreposicao
contaSobreposicoesImovel(DiaAnt,HorarioAnt,DuracaoVisita,[(_,_,_,Dia,Horario)|Tail],NSI):-
	DiaAnt==Dia,
	(HorarioAnt + DuracaoVisita) >=Horario,!,
	contaSobreposicoesImovel(Dia,Horario,DuracaoVisita,Tail,NSI1),
	NSI is NSI1 + 1.

%caso em que nao ha sobreposicao
contaSobreposicoesImovel(DiaAnt,HorarioAnt,DuracaoVisita,[(_,_,_,Dia,Horario)|Tail],NSI):-
	(DiaAnt\=Dia ; (HorarioAnt + DuracaoVisita) < Horario),!,
	contaSobreposicoesImovel(Dia,Horario,DuracaoVisita,Tail,NSI1),
	NSI is NSI1.








%----------------------------			ordenar a populacao sem excluir repetidos
ordena_populacao(PopAv,PopAvOrd):- msort(PopAv,PopAvOrd).





%----------------------------			gerar nova geracao
gera_geracao(0,Pop):-!,
	nl, nl, write('Geracao'), write(0), write(':'), nl,
	write(Pop), nl,
	retractall(solucao(_)),
	assertz(solucao(Pop)).

gera_geracao(G,Pop):-
	nl, nl, write('Geracao'), write(G), write(':'), nl,
	write(Pop), nl,
	melhoresAGuardar(NMelhores),
	guardarMelhores(Pop,NMelhores,ListaMelhores),
	cruzamento(Pop,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	reintroduzMelhores(NPopOrd,ListaMelhores,PopFinal),

	G1 is G-1,
	gera_geracao(G1,PopFinal).




%------------------------------------
guardarMelhores(Pop,NMelhores,ListaMelhores):-
	%avalia_populacao(Pop,PopAv),
	ordena_populacao(Pop,PopOrd),
	parteInicial(PopOrd,NMelhores,ListaMelhores).



%------------------------------------
reintroduzMelhores(Pop,ListaMelhores,PopFinal):-
	contaElementos(ListaMelhores,NM),
	contaElementos(Pop,NP),
	N is NP - NM,
	parteInicial(Pop,N,PopCortada),
	append(ListaMelhores,PopCortada,Pop2),
	ordena_populacao(Pop2, PopFinal).






%------------------------------------
cruzamento([],[]).
cruzamento([_*Ind],[Ind]).
cruzamento([_*Ind1,_*Ind2|Resto],[NInd1,NInd2|Resto1]):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),
	( (maybe(Pcruz),!,cruzar(Ind1,Ind2,P1,P2,NInd1),cruzar(Ind2,Ind1,P1,P2,NInd2))  ;  (NInd1=Ind1,NInd2=Ind2) ),
	cruzamento(Resto,Resto1).


%------------------------------------
gerar_pontos_cruzamento(PC1,PC2):-
	contaRelacoesClienteImoveis(N),
	random(0,N,P1),
	random(0,N,P2),
	ordenar_pontos_cruzamento(P1,P2,PC1,PC2).

ordenar_pontos_cruzamento(P1,P2,P1F,P2F):-
	P2>P1,
	P1F is P1,
	P2F is P2,!.

ordenar_pontos_cruzamento(P1,P2,P1F,P2F):-
	P2=<P1,
	gerar_pontos_cruzamento(P1F,P2F).


%------------------------------------


gerar_pontos_mutacao(PC1,PC2):-
	contaRelacoesClienteImoveis(N),
	random(1,N,P1),
	random(1,N,P2),
	ordenar_pontos_mutacao(P1,P2,PC1,PC2).

ordenar_pontos_mutacao(P1,P2,P1F,P2F):-
	P2>P1,
	P1F is P1,
	P2F is P2,!.

ordenar_pontos_mutacao(P1,P2,P1F,P2F):-
	P2=<P1,
	gerar_pontos_mutacao(P1F,P2F).


%------------------------------------



cruzar(Ind1,Ind2,P1,P2,NInd1):-
	parteInicial(Ind1,P1,LI),
	parteCentral(Ind2,P1,P2,LC),
	parteFinal(Ind1,P2,LF),
	append(LI,LC,Laux),
	append(Laux,LF,NInd1).
%insere Sub2 em Sub1 a partir de P2 obtendo NInd1

%------------------------------------
parteCentral(L,P1,P2,SL):-
	parteCentral(L,P1,P2,1,SL).

parteCentral([_|T],P1,P2,Pos,LR):-
	Pos =< P1,!,
	Pos1 is Pos+1,
	parteCentral(T,P1,P2,Pos1,LR).

parteCentral([H|T],P1,P2,Pos,[H|LR]):-
	Pos = P1 +1,
	Pos1 is Pos+1,!,
	parteCentral(T,P1,P2,Pos1,LR).
	%append(H,LR1,LR).

parteCentral([H|T],P1,P2,Pos,[H|LR]):-
	Pos > P1, Pos < P2,
	Pos1 is Pos+1,!,
	parteCentral(T,P1,P2,Pos1,LR).
	%append(H,LR1,LR).

parteCentral([H|_],_,P2,Pos,[H]):-
	Pos = P2.


%------------------------------------

parteInicial(L,PCorte,LR):-
	parteInicial(L,PCorte,1,LR).

parteInicial([H|T],PC,Pos,[H|LR]):-
	Pos =< PC,!,
	Pos1 is Pos+1,
	parteInicial(T,PC,Pos1,LR).

parteInicial(_,PC,Pos,[]):-
	Pos > PC,!.



%------------------------------------


parteFinal(L,PCorte,LR):-
	parteFinal(L,PCorte,1,LR).

parteFinal([],_,_,[]).

parteFinal([_|T],PC,Pos,LR):-
	Pos =< PC,!,
	Pos1 is Pos+1,
	parteFinal(T,PC,Pos1,LR).

parteFinal([H|T],PC,Pos,[H|LR]):-
	Pos > PC,!,
	Pos1 is Pos+1,
	parteFinal(T,PC,Pos1,LR).



%------------------------------------


mutacao([],[]).

mutacao([Ind|Rest],[NInd|Rest1]):-
	prob_mutacao(Pmut),
	((maybe(Pmut),!,mutacao1(Ind,NInd));NInd=Ind),
	mutacao(Rest,Rest1).


%------------------------------------
mutacao1(Ind,NInd):- /* Selec. 2 genes para serem trocados */
	gerar_pontos_mutacao(P1,P2),
	mutacao1(Ind,P1,P2,1,[],NInd).



mutacao1([],_,_,_,Laux,Laux):-!.

mutacao1([_|Tail],P1,P2,NGeneActual,Laux,NInd):-
	NGeneActual = P1,!,
	listaVisitasPossiveis(LVP),
	random_member(X,LVP),
	NGeneActual1 is NGeneActual+1,
	mutacao1(Tail,P1,P2,NGeneActual1,[X|Laux],NInd).

mutacao1([_|Tail],P1,P2,NGeneActual,Laux,NInd):-
	NGeneActual = P2,!,
	listaVisitasPossiveis(LVP),
	random_member(X,LVP),
	NGeneActual1 is NGeneActual+1,
	mutacao1(Tail,P1,P2,NGeneActual1,[X|Laux],NInd).

mutacao1([Gene|Tail],P1,P2,NGeneActual,Laux,NInd):-
	NGeneActual1 is NGeneActual+1,
	mutacao1(Tail,P1,P2,NGeneActual1,[Gene|Laux],NInd).


%-------- ALGORITMO A*

go(Orig,Dest,Cam,Custo):-
	astar(Dest,[(0,0,[Orig])],Cam,Custo).

astar(Dest,[(_,Custo,[Dest|T])|_],Cam,Custo):-!,
	reverse([Dest|T],Cam).

astar(Dest,[(_,Ca,[H|T])|Outros],Cam,Custo):-
	findall((ECax,Cax,[X,H|T]),
			(H\==Dest,estrada(H,X,Cx),\+ member(X,[H|T]),estimativa(X,Dest,Ex),Cax is Cx+Ca,ECax is Cax+Ex),
			Novos),
	append(Novos,Outros,Todos),
	sort(Todos,TodosOrd),
	astar(Dest,TodosOrd,Cam,Custo).


estimativa(C1,C2,Est):-
		coordenadas(C1,X1,Y1),
		coordenadas(C2,X2,Y2),
		DX is X2-X1,
		DY is Y2-Y1,
		Est is sqrt(DX*DX+DY*DY).






%------------------------------------	mostrar a solucao obtida
mostraAgenda:-
	mostraAgenda(_).


%------------------------------------		converter a solucao para Dias da semana e Horas do dia


mostraAgenda(Solucao):-
	solucao([_*H|_]),
	ordemCronologica(H,IndCron),
	nl,nl,write('--------------AGENDA---------------'),nl,nl,
	mostraAgenda(IndCron,[],Solucao),
	nl,avalia_individuo_verbose(H),nl.

mostraAgenda([],Laux,Laux):-!.

mostraAgenda([(Imovel,Mediador,Cliente,Dia,Minutos)|Tail],Laux,Solucao):-
	converteMH(Minutos,HorasDia),
	diasSemana(Dia,DiaSemana),
	imovel(Imovel,Zona,Tipo,_,TChave,_),
	cliente(Cliente,NomeCliente,_),
	mediador(Mediador,NomeMediador,_),
	nl,write(DiaSemana),write(' '),write(HorasDia),nl,
	write(' Mediador: '),write(NomeMediador),nl,
	write(' Cliente: '),write(NomeCliente),nl,
	write(' Imovel: '),write(Imovel),write(' '),write(Tipo),write(' '),write(Zona),nl,
	write(' Tem chave: '),write(TChave),nl,
	nl,write('---------------------------------'),nl,
	mostraAgenda(Tail,[(Imovel,Mediador,Cliente,DiaSemana,HorasDia)|Laux],Solucao).






%------------------------------------	mostrar a avaliacoes da solucao obtida
	avalia_individuo_verbose(Ind):-
	contaVisitasNaoFeitas(Ind,NVNF),
	diasDistintos(Ind,NDD),
	contaMudancasMediadorCliente(Ind,NM),
	calculaSobreposicoesMediador(Ind,MSM,MEM),%para o mediador
	calculaSobreposicoesCliente(Ind,MSC,MEC),%para o cliente
	contaSobreposicoesImoveis(Ind,NS),

	Penalizacao is (NVNF*300 + (NDD-1)*5 + NM*5 + MSM*5 + MEM*0 +MSC*5 + MEC*1 + NS*100),
	nl,write('Numero de Visitas nao efectuadas: '), write(NVNF),nl,
	nl,write('Numero de dias distintos: '), write(NDD),nl,
	nl,write('Numero de mudancas de par mediador cliente: '), write(NM),nl,
	nl,write('Minutos de atraso mediadores: '), write(MSM),nl,
	nl,write('Minutos de espera mediadores: '), write(MEM),nl,
	nl,write('Minutos de atraso clientes: '), write(MSC),nl,
	nl,write('Minutos de espera clientes: '), write(MEC),nl,
	nl,write('Numero de visitas a imoveis sobrepostas (clientes diferentes): '),write(NS),nl,
	nl,write('Penalizacao: '),write(Penalizacao),nl.





mostraAgendaCliente:-write('Insira o seu ID de Cliente'),read(Cliente),mostraAgendaCliente(Cliente).
mostraAgendaCliente(Cliente):-
	mostraAgendaCliente(Cliente,_).
mostraAgendaCliente(Cliente,Solucao):-
	solucao([_*H|_]),
	ordemCronologica(H,IndCron),
	nl,nl,write('--------------AGENDA---------------'),nl,nl,
	mostraAgendaCliente(Cliente,IndCron,[],Solucao),
	nl,nl.

mostraAgendaCliente(_,[],Laux,Laux):-!.

mostraAgendaCliente(Cliente,[(Imovel,Mediador,Cliente,Dia,Minutos)|Tail],Laux,Solucao):-
	converteMH(Minutos,HorasDia),
	diasSemana(Dia,DiaSemana),
	imovel(Imovel,Zona,Tipo,_,TChave,_),
	cliente(Cliente,NomeCliente,_),
	mediador(Mediador,NomeMediador,_),
	nl,write(DiaSemana),write(' '),write(HorasDia),nl,
	write(' Mediador: '),write(NomeMediador),nl,
	write(' Cliente: '),write(NomeCliente),nl,
	write(' Imovel: '),write(Tipo),write(' '),write(Zona),nl,
	write(' Tem chave: '),write(TChave),nl,
	nl,write('---------------------------------'),nl,
	mostraAgendaCliente(Cliente,Tail,[(Imovel,Mediador,Cliente,DiaSemana,HorasDia)|Laux],Solucao).


mostraAgendaCliente(C,[(Imovel,Mediador,Cliente,Dia,Minutos)|Tail],Laux,Solucao):-
	converteMH(Minutos,HorasDia),
	diasSemana(Dia,DiaSemana),
	mostraAgendaCliente(C,Tail,[(Imovel,Mediador,Cliente,DiaSemana,HorasDia)|Laux],Solucao).



mostraAgendaImovel:-write('Insira o seu ID do Imovel'),read(Imovel),mostraAgendaImovel(Imovel).
mostraAgendaImovel(Imovel):-
	mostraAgendaImovel(Imovel,_).

mostraAgendaImovel(Imovel,Solucao):-
	solucao([_*H|_]),
	ordemCronologica(H,IndCron),
	nl,nl,write('--------------AGENDA---------------'),nl,nl,
	imovel(Imovel,Zona,Tipo,_,_,_),
	write(' Imovel: '),write(Tipo),write(' '),write(Zona),nl,
	mostraAgendaImovel(Imovel,IndCron,[],Solucao),
	nl,nl.

mostraAgendaImovel(_,[],Laux,Laux):-!.

mostraAgendaImovel(Imovel,[(Imovel,Mediador,Cliente,Dia,Minutos)|Tail],Laux,Solucao):-
	converteMH(Minutos,HorasDia),
	diasSemana(Dia,DiaSemana),
	cliente(Cliente,NomeCliente,_),
	mediador(Mediador,NomeMediador,_),
	nl,write(DiaSemana),write(' '),write(HorasDia),nl,
	write(' Mediador: '),write(NomeMediador),nl,
	write(' Cliente: '),write(NomeCliente),nl,
	nl,write('---------------------------------'),nl,
	mostraAgendaImovel(Imovel,Tail,[(Imovel,Mediador,Cliente,DiaSemana,HorasDia)|Laux],Solucao).
mostraAgendaImovel(I,[(Imovel,Mediador,Cliente,Dia,Minutos)|Tail],Laux,Solucao):-
	converteMH(Minutos,HorasDia),
	diasSemana(Dia,DiaSemana),
	mostraAgendaImovel(I,Tail,[(Imovel,Mediador,Cliente,DiaSemana,HorasDia)|Laux],Solucao).







%---------------PROGRAMA---------------------------------------------------------

run:-retractall(horario(_)),
	preencheAgenda,
cls,agenda.



agenda:-nl,
	write('============================='), nl,
	write('=        Imobiliaria        ='), nl,
	write('============================='), nl,
	write('= 1-Mostrar Agenda geral    ='), nl,
	write('= 2-Mostrar Agenda cliente ='), nl,
	write('= 3-Mostrar Agenda imóvel  ='), nl,
	%write('= 4-Inserir visita cliente  ='), nl,
	write('= 4-Mostrar dia Mediador    ='), nl,
	write('= 0-Sair                    ='), nl,
	write('============================='),nl, nl,
	optMenu.

preencheAgenda:-
	criaHorario(480),
	separaClientes,
	separaMediadores,
	separaImoveis,
	separaRelacoesClienteImoveis,
	contaRelacoesClienteImoveis(NVisitasRealizar),
	retractall(visitas(_)),
	assert(visitas(NVisitasRealizar)),
	determinaVisitasPossiveis,
	gera_populacao(Pop),
	avalia_populacao(Pop,PopAv),
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	gera_geracao(NG,PopOrd),write(ok1).

optMenu:-
	  repeat,
	  read(Opt), nl,
	  (Opt == 1; Opt == 2; Opt==3; Opt==0;Opt==4),
          mostra(Opt).

mostra(Opt):-Opt==1,cls,
	mostraAgenda,
	agenda.
mostra(Opt):-Opt==2,
	cls,
	mostraAgendaCliente,
	agenda.
mostra(Opt):-Opt==3,
	cls,
	mostraAgendaImovel,
	agenda.

mostra(Opt):-Opt==4,
	cls,
	mostraVisitaFechada,
	agenda.

mostra(Opt):-Opt==0.

cls :- write('\e[2J').

insereVisitaCliente:-write('Indique o seu ID de Cliente: '),read(Cliente),nl,
	write('Indique o ID do imóvel que deseja visitar: '),read(Imovel), nl,
	assert(imovelCliente(Cliente,Imovel)),retractall(horario(_)),preencheAgenda.


mostraVisitaFechada:-write('Indique o seu ID de mediador'),read(Mediador),nl,
	write('Indique o dia do roteiro: '), read(Dia), nl,
	write('Indique a zona de Origem'),read(ZonaO),nl,
	write('Indique a zona de chegada'),read(ZonaC),nl,
	criarVisitaFechada(Mediador,ZonaO,ZonaC,Dia).

retirarVisitasDia(_,_,[],[]).
retirarVisitasDia(M,DiaSemana,[(Imovel,M,Cliente,DiaSemana,HorasDia)|T],[(Imovel,M,Cliente,DiaSemana,HorasDia)|ListaDia]):-
	retirarVisitasDia(M,DiaSemana,T,ListaDia).
retirarVisitasDia(M,Dia,[(_,_,_,_,_)|T],ListaDia):-retirarVisitasDia(M,Dia,T,ListaDia).

criarVisitaFechada(M,Zo,Zc,Dia):-solucao([_*H|_]),
	ordemCronologica(H,IndCron),
	retirarVisitasDia(M,Dia,IndCron,ListaDia),
	nl,nl,write('--------------AGENDA---------------'),nl,nl,
	criaVisitaFechada(M,Zo,Zc,ListaDia).


criaVisitaFechada(_,Zc,Zc,[]).
criaVisitaFechada(_,Zo,Zc,[]):-
	zona(F,Zc),
	zona(I,Zo),
	go(I,F,Caminho,Custo),
	imprimeCaminho(Caminho,Custo).

criaVisitaFechada(M,Zo,Zc,[(Imovel,M,Cliente,_,HorasDia)|T]):-
	imovel(Imovel,Zo,Tipo,_,_,_),
	write(' Imovel: '),write(Imovel),write(' '),write(Tipo),write(' '),write(Zo),nl,
	write('Para o cliente: '),write(Cliente),nl,
	write('Inicio visita: '),converteMH(HorasDia,H),write(H),nl,
	criaVisitaFechada(M,Zo,Zc,T).

criaVisitaFechada(M,Zo,Zc,[(Imovel,M,Cliente,_,HorasDia)|T]):-
	imovel(Imovel,Zona,Tipo,_,_,_),
	zona(I,Zo),
	zona(F,Zona),
	go(I,F,Caminho,Custo),
	imprimeCaminho(Caminho,Custo),
	write(' Imovel: '),write(Imovel),write(' '),write(Tipo),write(' '),write(Zona),nl,
	write('Para o cliente: '),write(Cliente),nl,
	write('Inicio visita: '),converteMH(HorasDia,H),write(H),nl,
	criaVisitaFechada(M,Zona,Zc,T).

imprimeCaminho(Caminho,Custo):-nl,write('Custo caminho: '),write(Custo),nl,imprimeCaminho(Caminho).
imprimeCaminho([]).
imprimeCaminho([H|T]):-zona(H,NZ),nl,write('-->'),write(NZ),nl,imprimeCaminho(T).






















