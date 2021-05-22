-module(mates).
-export([perimetro/1]).


perimetro({circle,Radio})-> 
    
    io:format("El perimetro del circulo de radio ~p es ~p ~n ",[Radio,3.14*Radio]);

perimetro({square,Side}) ->

    io:format("El perimetro de un cuadrado de lado ~p es ~p ~n",[Side,Side*4]);

perimetro({triangle,A,B,C}) ->

    io:format ("El perimetro de un triangulo con lados ~p ~p ~p es ~p ~n",[A,B,C,A+B+C]).