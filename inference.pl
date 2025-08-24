% ===============================
% inference.pl - Règles d'inférence
% ===============================

% --- Règles d'inférence — vérifient explicitement l'absence d'alibi ---
is_guilty(Suspect, vol) :-
    \+ has_alibi(Suspect, vol),
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    has_fingerprint_on_weapon(Suspect, vol).

is_guilty(Suspect, assassinat) :-
    \+ has_alibi(Suspect, assassinat),
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    ( has_fingerprint_on_weapon(Suspect, assassinat)
    ; eyewitness_identification(Suspect, assassinat)
    ).

is_guilty(Suspect, escroquerie) :-
    \+ has_alibi(Suspect, escroquerie),
    ( has_bank_transaction(Suspect, escroquerie)
    ; owns_fake_identity(Suspect, escroquerie)
    ).

% --- Traduction des prédicats en phrases lisibles ---
pretty_fact(has_motive(S,_), Txt) :-
    format(string(Txt), "~w avait un mobile", [S]).
pretty_fact(was_near_crime_scene(S,_), Txt) :-
    format(string(Txt), "~w etait pres de la scene du crime", [S]).
pretty_fact(has_fingerprint_on_weapon(S,_), Txt) :-
    format(string(Txt), "empreintes de ~w retrouvees sur l'arme", [S]).
pretty_fact(eyewitness_identification(S,_), Txt) :-
    format(string(Txt), "~w a ete identifie par un temoin", [S]).
pretty_fact(owns_fake_identity(S,_), Txt) :-
    format(string(Txt), "~w possede une fausse identite", [S]).
pretty_fact(has_bank_transaction(S,_), Txt) :-
    format(string(Txt), "~w a des transactions bancaires suspectes", [S]).

% --- Preuves manquantes ---
missing_evidence(S, vol, M) :-
    ( has_motive(S, vol) -> M1=[] ; M1=["mobile manquant"] ),
    ( was_near_crime_scene(S, vol) -> M2=[] ; M2=["preuve de proximite manquante"] ),
    ( has_fingerprint_on_weapon(S, vol) -> M3=[] ; M3=["empreintes manquantes"] ),
    append(M1, M2, T), append(T, M3, M).

missing_evidence(S, assassinat, M) :-
    ( has_motive(S, assassinat) -> M1=[] ; M1=["mobile manquant"] ),
    ( was_near_crime_scene(S, assassinat) -> M2=[] ; M2=["preuve de proximite manquante"] ),
    ( (has_fingerprint_on_weapon(S, assassinat); eyewitness_identification(S, assassinat))
        -> M3=[] ; M3=["empreintes ou temoin manquant"] ),
    append(M1, M2, T), append(T, M3, M).

missing_evidence(S, escroquerie, M) :-
    ( (has_bank_transaction(S, escroquerie); owns_fake_identity(S, escroquerie))
        -> M=[] ; M=["transaction ou fausse identite manquante"] ).

% --- Collecteur de preuves brutes ---
evidence(S, Crime, has_motive(S, Crime)) :- has_motive(S, Crime).
evidence(S, Crime, was_near_crime_scene(S, Crime)) :- was_near_crime_scene(S, Crime).
evidence(S, Crime, has_fingerprint_on_weapon(S, Crime)) :- has_fingerprint_on_weapon(S, Crime).
evidence(S, Crime, eyewitness_identification(S, Crime)) :- eyewitness_identification(S, Crime).
evidence(S, Crime, owns_fake_identity(S, Crime)) :- owns_fake_identity(S, Crime).
evidence(S, Crime, has_bank_transaction(S, Crime)) :- has_bank_transaction(S, Crime).

% --- Convertir la liste de preuves en texte lisible ---
translate_evidence([], []).
translate_evidence([H|T], [Txt|TT]) :-
    pretty_fact(H, Txt),
    translate_evidence(T, TT).

% --- Verdict structuré avec textes propres ---
verdict(S, Crime, Verdict) :-
    findall(F, evidence(S, Crime, F), RawProofs),
    translate_evidence(RawProofs, PreuvesLisibles),
    missing_evidence(S, Crime, Manquantes),
    ( has_alibi(S, Crime) ->
        Status = "Non coupable (alibi)"
    ; is_guilty(S, Crime) ->
        Status = "Coupable"
    ; Status = "Non coupable (preuves insuffisantes)"
    ),
    Verdict = verdict{
        status: Status,
        suspect: S,
        crime: Crime,
        preuves: PreuvesLisibles,
        manquantes: Manquantes
    }.

% --- Support explicatif des preuves ---
supporting_facts(S, vol, L) :-
    findall("avait un mobile", has_motive(S, vol), M),
    findall("etait pres de la scene du crime", was_near_crime_scene(S, vol), P),
    findall("a laisse ses empreintes", has_fingerprint_on_weapon(S, vol), F),
    append([M,P,F], L).

supporting_facts(S, assassinat, L) :-
    findall("avait un mobile", has_motive(S, assassinat), M),
    findall("etait pres de la scene du crime", was_near_crime_scene(S, assassinat), P),
    findall("a laisse ses empreintes", has_fingerprint_on_weapon(S, assassinat), F),
    findall("a ete identifie par un temoin", eyewitness_identification(S, assassinat), T),
    append([M,P,F,T], L).

supporting_facts(S, escroquerie, L) :-
    findall("a des transactions bancaires suspectes", has_bank_transaction(S, escroquerie), B),
    findall("possede une fausse identite", owns_fake_identity(S, escroquerie), I),
    append(B, I, L).

% --- Génération des questions manquantes ---
questions_missing(Suspect, vol, Qs) :-
    findall(Q, (
        (\+ has_motive(Suspect, vol) -> Q = "Quel est le mobile ?" ; fail)
        ;
        (\+ was_near_crime_scene(Suspect, vol) -> Q = "Etait-il/elle pres de la scene du crime ?" ; fail)
        ;
        (\+ has_fingerprint_on_weapon(Suspect, vol) -> Q = "Y a-t-il des empreintes ?" ; fail)
    ), Qs).

questions_missing(Suspect, assassinat, Qs) :-
    findall(Q, (
        (\+ has_motive(Suspect, assassinat) -> Q = "Quel est le mobile ?" ; fail)
        ;
        (\+ was_near_crime_scene(Suspect, assassinat) -> Q = "Etait-il/elle pres de la scene du crime ?" ; fail)
        ;
        (\+ has_fingerprint_on_weapon(Suspect, assassinat), \+ eyewitness_identification(Suspect, assassinat)
            -> Q = "Y a-t-il des preuves (empreintes ou témoin) ?" ; fail)
    ), Qs).

questions_missing(Suspect, escroquerie, Qs) :-
    findall(Q, (
        (\+ has_bank_transaction(Suspect, escroquerie), \+ owns_fake_identity(Suspect, escroquerie)
            -> Q = "Y a-t-il eu des transactions suspectes ou fausse identité ?" ; fail)
    ), Qs).
