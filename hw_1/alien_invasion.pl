% Problem #2, "alien invasion"
% Each child got an alien toy which had a different special feature.

toy(flubsub).
toy(gribbet).
toy(jarix).
toy(wattin).


child(andrew).
child(dudley).
child(georgina).
child(karen).

feature(bubbles).
feature(colors).
feature(eyes).
feature(fins).

solve :-
    toy(AndrewToy), toy(DudleyToy), toy(GeorginaToy), toy(KarenToy),
    all_different([AndrewToy, DudleyToy, GeorginaToy, KarenToy]),
    feature(AndrewFeature), feature(DudleyFeature), feature(GeorginaFeature), feature(KarenFeature),
    all_different([AndrewFeature, DudleyFeature, GeorginaFeature, KarenFeature]),

    Triples = [ [andrew, AndrewToy, AndrewFeature],
                [dudley, DudleyToy, DudleyFeature],
                [georgina, GeorginaToy, GeorginaFeature],
                [karen, KarenToy, KarenFeature] ],

    % 1. Dudley didn't walk out of the store with either Flubsub or Jarix, and his alien doesn't develop fins when placed in water.
    \+ member([dudley, flubsub, _], Triples),
    \+ member([dudley, jarix, _], Triples),
    \+ member([dudley, _, fins], Triples),

    % 2. Jarix (which isn't the name of the alien Andrew picked) has eyes that glow in the dark.
    member([_, jarix, eyes], Triples),
    \+ member([andrew, jarix, _], Triples),

    % 3. Karen left the toy store with the alien Wattin.
    member([karen, wattin, _], Triples),

    % 4. Andrew doesn't own the alien that develops fins, and Dudley doesn't own the alien that blows bubbles.
    \+ member([andrew, _, fins], Triples),
    \+ member([dudley, _, bubbles], Triples),

    tell(andrew, AndrewToy, AndrewFeature),
    tell(dudley, DudleyToy, DudleyFeature),
    tell(georgina, GeorginaToy, GeorginaFeature),
    tell(karen, KarenToy, KarenFeature).


% Succeeds if all elements of the argument list are bound and different.
% Fails if any elements are unbound or equal to some other element.
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

tell(X, Y, Z) :-
    write(X), write(' got the '), write(Y),
    write(' with special feature of '), write(Z), write('.'), nl.

