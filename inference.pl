% regles d'inference — verifient explicitement l'absence d'alibi
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

% helpers pour explanation (utilises par dialog.pl)
supporting_facts(S, vol, L) :-
    findall(motif, has_motive(S, vol), M),
    findall(proximite, was_near_crime_scene(S, vol), P),
    findall(empreintes, has_fingerprint_on_weapon(S, vol), F),
    append(M, P, T), append(T, F, L).
supporting_facts(S, assassinat, L) :-
    findall(motif, has_motive(S, assassinat), M),
    findall(proximite, was_near_crime_scene(S, assassinat), P),
    findall(empreintes, has_fingerprint_on_weapon(S, assassinat), F),
    findall(temoin, eyewitness_identification(S, assassinat), T),
    append(M, P, T1), append(T1, F, T2), append(T2, T, L).
supporting_facts(S, escroquerie, L) :-
    findall(transaction, has_bank_transaction(S, escroquerie), B),
    findall(fausse_id, owns_fake_identity(S, escroquerie), I),
    append(B, I, L).

missing_evidence(S, vol, M) :-
    ( has_motive(S, vol) -> M1=[] ; M1=[motif] ),
    ( was_near_crime_scene(S, vol) -> M2=[] ; M2=[proximite] ),
    ( has_fingerprint_on_weapon(S, vol) -> M3=[] ; M3=[empreintes] ),
    append(M1, M2, T), append(T, M3, M).
missing_evidence(S, assassinat, M) :-
    ( has_motive(S, assassinat) -> M1=[] ; M1=[motif] ),
    ( was_near_crime_scene(S, assassinat) -> M2=[] ; M2=[proximite] ),
    ( (has_fingerprint_on_weapon(S, assassinat); eyewitness_identification(S, assassinat)) -> M3=[] ; M3=[empreintes_ou_temoin] ),
    append(M1, M2, T), append(T, M3, M).
missing_evidence(S, escroquerie, M) :-
    ( (has_bank_transaction(S, escroquerie); owns_fake_identity(S, escroquerie)) -> M=[] ; M=[transaction_ou_fausse_id] ).