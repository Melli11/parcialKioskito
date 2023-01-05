% atiende(Persona,diaAtencion,HorarioInicio,HorarioFin).
atiende(dodain,lunes,9,15).
atiende(dodain,miercoles,9,15).
atiende(dodain,viernes,9,15).
atiende(lucas,martes,10,20).
atiende(juanC,sabados,18,22).
atiende(juanC,domingos,18,22).
atiende(juanFdS,jueves,10,20).
atiende(juanFdS,viernes,12,20).
atiende(leoC,lunes,14,18).
atiende(leoC,miercoles,14,18).
atiende(martu,miercoles,23,24).

% Predicados Auxiliares
persona(Persona):-
    atiende(Persona,_,_,_).



% Punto 1: calentando motores (2 puntos)
% punto1.1: vale atiende los mismos días y horarios que dodain y juanC.

atiende(vale,Dia,HoraInicio,HoraFin):-
    atiende(dodain,Dia,HoraInicio,HoraFin).

atiende(vale,Dia,HoraInicio,HoraFin):-
    atiende(juanC,Dia,HoraInicio,HoraFin).


:-begin_tests(punto1).
test(punto1_calentando_motores,nondet):-

atiende(vale,lunes,9,15),
atiende(vale,miercoles,9,15),
atiende(vale,viernes,9,15),
atiende(vale,sabados,18,22),
atiende(vale,domingos,18,22).

end_tests(punto1).


% punto1.2 por principio de universo cerrado, si nadie lo hace, no necesita explicitar nada.

% punto1.3 idem anterior.

evaluandoHorario(martes,maiu,0,8).
evaluandoHorario(miercoles,maiu,0,8).

% Punto 2: quién atiende el kiosko... (2 puntos)

quienAtiende(Dia,Horario,Persona):-
atiende(Persona,Dia,HoraInicio,HoraFin),
trabajaEnEseHorario(Horario,HoraInicio,HoraFin).


trabajaEnEseHorario(Horario,HoraInicio,HoraFin):-
    between(HoraInicio, HoraFin, Horario),
    Horario =< HoraFin,
    Horario >= HoraInicio.
    


:-begin_tests(punto2).
    test(calentando_motores,nondet):-

    quienAtiende(lunes,14,dodain),
    quienAtiende(lunes,14,leoC),
    quienAtiende(lunes,14,vale),
    quienAtiende(sabados,18,juanC),
    quienAtiende(sabados,18,vale).

end_tests(punto2).

% Punto 3: Forever alone (2 puntos)
% atiende(Persona,diaAtencion,HorarioInicio,HorarioFin).
% quienAtiende(Dia,Horario,Persona):-


% no existe otra persona que atienda en ese dia y ese horario
foreverAlone(Persona,Dia,Hora):-  
quienAtiende(Dia,Hora,Persona),
not((quienAtiende(Dia,Hora,OtraPersona),Persona \= OtraPersona)).

:-begin_tests(punto3).
    test(calentando_motores,nondet):-

    quienAtiende(lunes,14,dodain),
    quienAtiende(lunes,14,leoC),
    quienAtiende(lunes,14,vale),
    quienAtiende(sabados,18,juanC),
    quienAtiende(sabados,18,vale).

end_tests(punto2).
