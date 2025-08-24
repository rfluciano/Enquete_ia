:- module(dialog, [ask/0, ask_nl/0]).
:- use_module(facts).
:- use_module(inference).

% =====================
% Interface principale
% =====================

ask :-
    writeln("Entrez une question du type:"),
    writeln(" - est-ce que [suspect] est coupable pour [crime]"),
    writeln(" - quitter"),
    read_line_to_string(user_input, input),
    handle_input(input).

ask_nl :-
    writeln("Question (ex: \"mary escroquerie\") ou taper quitter:"),
    read_line_to_string(user_input, Input),
    handle_simple_input(Input).

% =====================
% Gestion des questions
% =====================

handle_input("quitter") :- !, writeln("Au revoir !").
handle_input(Input) :-
    split_string(Input, " ", "", Words),
    (   parse_question(Words, Suspect, Crime)
    ->  verdict(Suspect, Crime)
    ;   writeln("❌ Je n'ai pas compris la question."),
        ask
    ).

handle_simple_input("quitter") :- !, writeln("Au revoir !").
handle_simple_input(Input) :-
    split_string(Input, " ", "", [SuspectStr, CrimeStr | _]),
    atom_string(Suspect, SuspectStr),
    atom_string(Crime, CrimeStr),
    verdict(Suspect, Crime),
    ask_nl.

% =====================
% Analyse des questions
% =====================

parse_question(Words, Suspect, Crime) :-
    append(_, [S, "est", "coupable", "pour", C], Words),
    atom_string(Suspect, S),
    atom_string(Crime, C).

% =====================
% Generation du verdict
% =====================

verdict(S, C) :-
    (   prove(S, C)
    ->  writeln("⚖️ Conclusion: Coupable."), explain_with_sources(S, C)
    ;   writeln("⚖️ Conclusion: Non coupable (preuves insuffisantes)."), explain_with_sources(S, C)
    ).

% =====================
% Explication humaine
% =====================

explain_with_sources(S, C) :-
    supporting_facts(S, C, Facts),
    missing_evidence(S, C, Miss),

    writeln(""),
    format("📌 Voici les elements concernant ~w dans l'affaire ~w :~n", [S, C]),

    ( Facts = [] ->
        writeln(" - Aucune preuve trouvee pour l'instant.")
    ; writeln(" ✅ Preuves disponibles :"),
      forall(member(F, Facts), describe_fact(F))
    ),

    ( Miss = [] ->
        writeln(" 🔎 Aucune preuve manquante.")
    ; writeln(" ❌ Preuves manquantes :"),
      forall(member(M, Miss), describe_fact(M))
    ),
    writeln("").

% =====================
% Traduction des faits
% =====================

describe_fact(has_alibi(S, Crime)) :-
    format("   - ~w possede un alibi pour ~w.~n", [S, Crime]).
describe_fact(has_motive(S, Crime)) :-
    format("   - ~w avait un motif pour ~w.~n", [S, Crime]).
describe_fact(was_near_crime_scene(S, Crime)) :-
    format("   - ~w etait proche du lieu du ~w.~n", [S, Crime]).
describe_fact(has_fingerprint_on_weapon(S, Crime)) :-
    format("   - On a trouve les empreintes de ~w sur l’arme du ~w.~n", [S, Crime]).
describe_fact(eyewitness_identification(S, Crime)) :-
    format("   - Un temoin a identifie ~w dans l’affaire de ~w.~n", [S, Crime]).
describe_fact(owns_fake_identity(S, Crime)) :-
    format("   - ~w possedait une fausse identite liee à ~w.~n", [S, Crime]).
describe_fact(has_bank_transaction(S, Crime)) :-
    format("   - Une transaction bancaire suspecte relie ~w à l’~w.~n", [S, Crime]).
describe_fact(_) :-
    writeln("   - [Preuve non decrite].").
