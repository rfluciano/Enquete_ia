
:- dynamic has_alibi/2, has_motive/2, was_near_crime_scene/2,
           has_fingerprint_on_weapon/2, eyewitness_identification/2,
           owns_fake_identity/2, has_bank_transaction/2.
:- dynamic dialog_state/1, dialog_history/1, fact_source/2, reason/2, dialog_line/1.

record_history(Event) :- assertz(dialog_history(Event)).
record_fact(Fact, Source) :-
    assertz(Fact),
    assertz(fact_source(Fact, Source)),
    record_history(assert(Fact)-Source).

undo_last :-
    ( retract(dialog_history(Event)) -> undo_event(Event) ; writeln('Rien a annuler.') ).
% corrige : ignorer la valeur avec underscore pour eviter singleton variable warning
undo_event(state(Key-_)) :- retractall(dialog_state(Key-_)), !.
undo_event(assert(Fact)) :- ( retract(Fact) -> format('Annule: ~w~n', [Fact]) ; true ).
undo_event(pop(_)) :- true.