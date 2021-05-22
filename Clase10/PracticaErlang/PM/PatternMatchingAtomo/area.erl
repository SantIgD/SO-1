area(cuadrado, {Lado}) ->
    Lado * Lado;
area(triangulo,{Base, Altura}) ->
    Base * Altura / 2;
area(OtrasFiguras, Info) -> io:format("Completar").
