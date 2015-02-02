% Problem #3, "shutter bugs"
% Each people with a surname submitted pictures with different subject.

picture('flowering shrub').
picture(horse).
picture(lighthouse).
picture(sunset).
picture(tugboat).


people(donald).
people(emily).
people(gregory).
people(hannah).
people(iris).

surname(didduck).
surname(lombardi).
surname(purpuri).
surname(swaboda).
surname(wong).

solve :-
    surname(Donaldsurname), surname(Emilysurname), surname(Gregorysurname), surname(Hannahsurname), surname(Irissurname),
    all_different([Donaldsurname, Emilysurname, Gregorysurname, Hannahsurname, Irissurname]),
    picture(Donaldpicture), picture(Emilypicture), picture(Gregorypicture), picture(Hannahpicture), picture(Irispicture),
    all_different([Donaldpicture, Emilypicture, Gregorypicture, Hannahpicture, Irispicture]),

    Triples = [ [donald, Donaldsurname, Donaldpicture],
                [emily, Emilysurname, Emilypicture],
                [gregory, Gregorysurname, Gregorypicture],
                [hannah, Hannahsurname, Hannahpicture],
                [iris, Irissurname, Irispicture] ],


    % 1. The one surnamed Didduck (who isn't Iris) photographed a tugboat.
    member([_, didduck, tugboat], Triples),
    \+ member([iris, didduck, _], Triples),

    % 2. One woman (who isn't surnamed Purpuri) took a photograph of a breathtaking sunset.
    \+ member([_, purpuri, sunset], Triples),
    (member([emily, _, sunset], Triples);
    member([hannah, _, sunset], Triples);
    member([iris, _, sunset], Triples)),

    % 3. The one surnamed Lombardi (who isn't Hannah or Iris) used a fast shutter speed to capture a shot of young horse at play.
    member([_, lombardi, horse], Triples),
    \+ member([hannah, lombardi, _], Triples),
    \+ member([iris, lombardi, _], Triples),

    % 4. Gregory Swaboda took his photo with the camera he received for his birthday.
    member([gregory, swaboda, _], Triples),

    % 5. Neither Iris (who didn't take the picture of the flowering shrub) nor Hannah took a photograph of the sunset.
    \+ member([iris, _, 'flowering shrub'], Triples),
    \+ member([iris, _, sunset], Triples),
    \+ member([hannah, _, sunset], Triples),
    

    tell(donald, Donaldsurname, Donaldpicture),
    tell(emily, Emilysurname, Emilypicture),
    tell(gregory, Gregorysurname, Gregorypicture),
    tell(hannah, Hannahsurname, Hannahpicture),
    tell(iris, Irissurname, Irispicture).


% Succeeds if all elements of the argument list are bound and different.
% Fails if any elements are unbound or equal to some other element.
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

tell(X, Y, Z) :-
    write('The photographer '), write(X), write(' whose surname is '), write(Y),
    write(' submitted a picture with the subject of '), write(Z), write('.'), nl.

